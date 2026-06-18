package scripts

import services.movies.TitleNormalizer
import services.titlerules.{MongoTitleRulesRepository, TitleRule, TitleRuleSet}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/** One-shot migration: rewrite the live title rules to reference the `{{SEP}}` /
 *  `{{SEPD}}` placeholders instead of hand-spelling a separator, and merge the
 *  rules that differed ONLY by separator. The point is the consolidation the
 *  task asked for — "wherever `:` `-` `|` is used, accept all of them (plus
 *  `/ _ \ – —`), spaces optional".
 *
 *  Reads the checked-in mirror [[GeneratedTitleRules]] as the "before" (so the
 *  dry-run needs no DB), prints every old→new rewrite, validates the new set
 *  compiles, and replays the prod titles fixture to show the impact (and flag
 *  any REGRESSION — a title the old rules stripped that the new ones no longer
 *  do). With `--apply` it overwrites the matching rules in prod Mongo by id and
 *  drops the merged-away rule, then the worker's change-stream backfill re-keys.
 *
 *  Run:  dry-run  `sbt 'worker/Test/runMain scripts.GeneralizeSeparators'`
 *        apply    (flyctl proxy 27017 + .env.local) `sbt 'worker/Test/runMain scripts.GeneralizeSeparators --apply'`
 *
 *  CAREFULLY-EXCLUDED rules (see [[leftAsIs]]): a separator that doubles as an
 *  in-word hyphen ("Spider-Man") must NOT be generalised — `structural-slash-suffix`
 *  and `bok-pipe-to-colon` would truncate real titles, so they stay literal. */
object GeneralizeSeparators {

  /** id → new pattern. The `{{SEP}}`/`{{SEPD}}`/`{{NSEP}}` tokens expand at
   *  compile time (see TitleRulePlaceholders). Befores come from GeneratedTitleRules. */
  val rewrites: Map[String, String] = Map(
    // ── prefix banners with FIXED text before the separator (safe) ──
    "canonical-gwiezdne-wojny"  -> """^Gwiezdne Wojny{{SEP}}""",
    "xtra-pp-konesera"          -> """(?i)^(?:Klub|Kino)\s+Konesera{{SEP}}""",
    "xtra-pp-pora-dla-seniora"  -> """(?i)^Pora\s+dla\s+Seniora{{SEP}}""",
    "xtra-pp-wtorki-seniora"    -> """(?i)^Wtorki\s+dla\s+Seniora{{SEP}}""",
    "xtra-pp-kino-dla-seniora"  -> """(?i)^Kino\s+dla\s+Seniora{{SEP}}""",
    "xtra-pp-spotkania-seniora" -> """(?i)^Filmowe\s+spotkania\s+seniora{{SEP}}""",
    "xtra-pp-fks-seniorki"      -> """(?i)^Filmowy\s+Klub\s+Seniora\s+i\s+Seniorki{{SEP}}""",
    "xtra-pp-modowy-klub"       -> """(?i)^Modowy\s+klub\s+filmowy{{SEP}}""",
    "xtra-pp-klub-dlr"          -> """(?i)^Klub\s+DLR{{SEP}}""",
    "xtra-pp-kino-dostepne"     -> """(?i)^Kino\s+Dostępne{{SEP}}""",
    "xtra-pp-psychoanaliza"     -> """(?i)^Kino\s+[ai]\s+psychoanaliz[ae]{{SEP}}""",
    "xtra-pp-psychologia"       -> """(?i)^Filmowe\s+spotkania\s+z\s+psycholog(?:ią|ia){{SEP}}""",
    "xtra-pp-filmoterapia"      -> """(?i)^Filmoterapia\s+z\s+Inspirą{{SEP}}""",
    "xtra-pp-portret-kobiety"   -> """(?i)^Portret\s+Kobiety{{SEP}}""",
    "xtra-pp-best-film-on-tour" -> """(?i)^Best\s+Film\s+on\s+Tour{{SEP}}""",
    "xtra-pp-klasyka-atlantic"  -> """(?i)^Klasyka\s+w\s+kinie\s+Atlantic{{SEP}}""",
    "xtra-pp-pnkf-prefix"       -> """(?i)^\d+\.\s*PRZEGLĄD\s+NOWEGO\s+KINA\s+FRANCUSKIEGO{{SEP}}""",
    "r-ax8ueh20"                -> """(?i)^Federico\s+Fellini{{SEP}}""",
    "r-uj0jp102"                -> """(?i)spotkania\s+filmowe{{SEP}}""",
    "r-xg9nmk5r"                -> """(?i)dobra\s+strona\s+filmu{{SEP}}""",
    "r-ror8g7kv"                -> """(?i)KF\s+Filmowa\s+Wisłostrada{{SEP}}""",
    "r-4ep3mmlt"                -> """(?i)^Rodzina w kinie{{SEP}}""",
    "r-uhuyt26o"                -> """(?i)^Rok z Marilyn Monroe(?:{{SEP}})?""",
    "r-onsvx1u5"                -> """(?i)^\s*SEANS PRZYJAZNY SENSORYCZNIE(?:{{SEP}})?""",
    "r-0tvhk11j"                -> """(?i)^\s*Spektakl(?:{{SEP}})?""",
    "r-7a8sumk7"                -> """\s*WSP{{SEP}}""",
    "r-ani4ylvo"                -> """(?i)KOSMICZNY\s+PORANEK{{SEP}}""",
    "r-nx96vnkj"                -> """(?i)^LUNATYCY\. KINO I ROZMOWA{{SEP}}""",

    // ── prefix banners with a VARIABLE name guard: widen the guard to {{NSEP}}
    //    so it stops at the first separator instead of backtracking into the film ──
    "search-programme-prefix"   -> """(?i)^(?:Kino\s+bez\s+barier|Pokaz\s+sensorycznie\s+przyjazny|Filmow[ey]\s+Poran(?:ki|ek)(?:\s+{{NSEP}}+)?|Zimowe\s+Poranki(?:\s+{{NSEP}}+)?|Poranek\s+dla\s+dzieci|Filmowy\s+Klub\s+Seniora|Dyskusyjny\s+Klub\s+Filmowy|Filmowe\s+spotkania\s+z\s+psychoanaliz[ąa]|Cinema\s+Italia\s+Oggi|Plenerowe\s+Pa[łl]acowe){{SEP}}""",
    "xtra-pp-dkf-named"         -> """(?i)^DKF\s+{{NSEP}}+{{SEP}}""",
    "xtra-pp-klub-filmowy"      -> """(?i)^Klub\s+Filmowy\s+{{NSEP}}+{{SEP}}""",
    "xtra-pp-klasyka-na-topie"  -> """(?i)^Klasyka\s+na\s+TOPie(?:\s+na\s+{{NSEP}}+)?{{SEP}}""",
    "xtra-pp-wajda-rewizje"     -> """(?i)^(?:Cykl\s+[„"]?\s*)?WAJDA{{SEP}}re-?\s*wizje{{NSEP}}*{{SEP}}""",

    // ── suffixes/prefixes anchored to a FIXED word after the separator (safe) ──
    "xtra-przedpremiera-suffix" -> """(?i){{SEP}}(?:przedpremiera|przedpremierowo|zobacz\s+przedpremierowo|seans\s+przedpremierowy)\s*$""",
    "xtra-przedpremiera-prefix" -> """(?i)^(?:przedpremiera|seans\s+przedpremierowy){{SEPD}}""",
    "xtra-fellini-prefix"       -> """(?i)^Federico\s+Fellini{{SEP}}(?:ciao\s+a?\s*tutti\s*!?)?{{SEP}}""",
    "xtra-fellini-suffix"       -> """(?i){{SEP}}(?:przegl[ąa]d\s+)?Federico\s+Fellini\b.*$""",
    // MERGE: pipe/underscore + dash DKF suffixes collapse into one (drop the dash rule).
    "xtra-dkf-suffix-pipe-underscore" -> """(?i){{SEP}}DKF\b.*$""",
    "xtra-dyskusyjny-suffix"    -> """(?i){{SEP}}dyskusyjny\s+klub\s+filmowy\s*$""",
    "xtra-wtf-fest-prefix"      -> """(?i)^WTF\s+Fest{{SEP}}""",
    "xtra-pipe-festival-suffix" -> """(?i){{SEP}}(?:6\s+razy\s+Pedro|Kino\s+cyrkularne)\b.*$""",
    "xtra-10-10-klasyka"        -> """(?i){{SEP}}10/10\s+Klasyka\s+filmowa\s*$""",
    "xtra-wajda-rewizje-suffix" -> """(?i){{SEP}}[„"]?\s*WAJDA{{SEP}}re-?\s*wizje.*$""",
    "xtra-pnkf-suffix-pipe"     -> """(?i){{SEP}}Przegląd\s+Nowego\s+Kina\s+Francuskiego\s*$""",
    "xtra-amerykanska-klasyka"  -> """(?iu){{SEP}}amerykańska\s+klasyka(?:\s*/.*)?\s*$""",
    "xtra-zulawski-kino-ekstazy"-> """(?iu){{SEP}}żuławski\.?\s*kino\s+ekstazy\s*$""",
    "xtra-poniedzialki-konwicki"-> """(?i){{SEP}}Poniedziałki\s+z\s+Konwickim\b.*$""",
    "structural-cykl-prefix"    -> """^Cykl\s+[„"][^„""]*[„""]?{{SEP}}""",
    "structural-anniversary-suffix" -> """(?i)(?:{{SEPD}})?\d+(?:st|nd|rd|th)?\.?\s*(?:anniversary|rocznica)\s*$""",
    "structural-restored-suffix"    -> """(?i)(?:{{SEPD}})?\d+\s*k\s+(?:restored|remaster(?:ed)?)\s*$""",
    "structural-wersja-suffix"      -> """(?i){{SEPD}}wersja\s+\p{L}+\s*$""",
    "r-lxmjlzev"                -> """(?i){{SEP}}UROCZYSTA\s+POLSKA\s+PREMIERA$""",
    "r-hg2rf2ly"                -> """{{SEP}}film$""",
    "r-mgt25ede"                -> """(?i){{SEP}}seans.*$""",
    "r-ectm6pbz"                -> """(?i)\s*UNLIMITED\s+SHOW{{SEP}}""",
    "r-r0h3k75l"                -> """{{SEP}}"WAJDA: re- wizje. Przegląd filmów Andrzeja Wajdy w 100. rocznicę urodzin"""",

    // ── per-cinema tag strips anchored to a FIXED tag after the separator ──
    "cc-ladies-night"           -> """^Ladies Night{{SEP}}""",
    "cc-powrot-do-kin"          -> """{{SEP}}powrót do kin$""",
    "cc-mamoru-hosody"          -> """^Kolekcja\s+Mamoru\s+Hosody{{SEP}}""",
    "mk-mamoru-hosody"          -> """^Kolekcja\s+Mamoru\s+Hosody{{SEP}}""",
    "mk-kino-na-obcasach"       -> """^Kino na obcasach{{SEP}}""",
    "helios-suffix-5"           -> """{{SEP}}Salon Kultury Helios$""",
    "helios-suffix-6"           -> """{{SEP}}KNTJ$""",
    "helios-suffix-7"           -> """{{SEP}}KNT$""",
    "helios-suffix-8"           -> """{{SEP}}Kino Kobiet$""",
    "helios-suffix-9"           -> """{{SEP}}Kino Konesera$""",
    "helios-suffix-10"          -> """{{SEP}}seanse z konkursami HDD$""",
    "helios-suffix-11"          -> """{{SEP}}Event projekt$""",
    "helios-suffix-12"          -> """{{SEP}}dubbing$""",
    "helios-suffix-13"          -> """{{SEP}}Dubbing$""",
    "helios-suffix-14"          -> """{{SEP}}napisy$""",
    "helios-suffix-15"          -> """{{SEP}}NAP$""",
    "helios-suffix-16"          -> """{{SEP}}DUB$""",
    "helios-suffix-17"          -> """{{SEP}}AF$""",
    "r-3yuy11of"                -> """(?i){{SEP}}Salon Kultury Helios\s*$""",
    "apollo-dzien-dziecka"      -> """^DZIEŃ DZIECKA W APOLLO{{SEP}}""",
    "apollo-przedpremierowy"    -> """{{SEP}}seans przedpremierowy$""",
    "palacowe-poranek"          -> """^Poranek dla dzieci{{SEP}}""",
    "palacowe-dkf-zamek"        -> """^DKF Zamek{{SEP}}""",
    "palacowe-wajda"            -> """^WAJDA{{SEP}}re-wizje\. """,
    "muza-najlepsze-z-najgorszych" -> """(?i){{SEP}}najlepsze\s+z\s+najgorszych\s*$""",
    "bok-promo"                 -> """{{SEP}}[A-ZĄĆĘŁŃÓŚŹŻ0-9 ]{3,}\s*$"""
  )

  /** Rules deleted because another rewrite now subsumes them. */
  val drops: Set[String] = Set("xtra-dkf-suffix-dash")  // merged into xtra-dkf-suffix-pipe-underscore

  /** Separator-bearing rules deliberately NOT generalised, with the reason. */
  val leftAsIs: Seq[(String, String)] = Seq(
    "structural-slash-suffix" -> "'/ <rest>' truncates everything after a slash; widening to {{SEP}} (incl. '-') would cut hyphenated titles at the hyphen",
    "bok-pipe-to-colon"       -> "applyAll '|'→': '; widening the match would rewrite an in-word hyphen ('Spider-Man' → 'Spider: Man')",
    "xtra-jim-jarmusch-suffix"-> "'//' double-slash tag; a single {{SEP}} can't span it cleanly (single '/' is already covered by {{SEP}} elsewhere)"
  )

  private def rewriteOrDrop(r: TitleRule): Option[TitleRule] =
    if (drops.contains(r.id)) None
    else rewrites.get(r.id).map(p => r.copy(pattern = p)).orElse(Some(r))

  def main(args: Array[String]): Unit = {
    val before = GeneratedTitleRules.all

    // Sanity: every id we name must exist in the mirror (catch a typo'd id).
    val known   = before.map(_.id).toSet
    val unknown = (rewrites.keySet ++ drops ++ leftAsIs.map(_._1)).diff(known)
    require(unknown.isEmpty, s"Unknown rule ids (not in GeneratedTitleRules): ${unknown.toSeq.sorted.mkString(", ")}")

    val after   = before.flatMap(rewriteOrDrop)
    val oldSet  = TitleRuleSet(before)
    val newSet  = TitleRuleSet(after)

    println(s"== Generalize separators: ${rewrites.size} rewrites, ${drops.size} merge-drop, ${leftAsIs.size} left as-is ==\n")

    val byId = before.map(r => r.id -> r).toMap
    println("-- rewrites (old → new) --")
    rewrites.toSeq.sortBy(_._1).foreach { case (id, _) =>
      val oldP = byId(id).pattern
      val newP = after.find(_.id == id).get.pattern
      println(s"  [$id]\n      - $oldP\n      + $newP")
    }
    println("\n-- merged away --")
    drops.toSeq.sorted.foreach(id => println(s"  [$id] dropped — '${byId(id).pattern}' now covered by its sibling"))
    println("\n-- left as-is (NOT generalised) --")
    leftAsIs.foreach { case (id, why) => println(s"  [$id] $why") }

    // The new set must fully compile — no invalid/unresolved patterns.
    val invalid = newSet.invalidRules
    if (invalid.nonEmpty) {
      println("\n!! INVALID new patterns — fix before applying:")
      invalid.foreach(r => println(s"   [${r.id}] ${r.pattern}"))
      sys.exit(1)
    }
    println("\nAll new patterns compile. ✓")

    replayFixture(oldSet, newSet)

    if (args.contains("--apply")) apply()
    else println("\n[dry-run] no --apply — nothing written. Re-run with --apply (flyctl proxy + .env.local) to upsert to prod.")
  }

  private val fixture = Paths.get("common/src/test/resources/fixtures/prod-movies/titles.txt")

  /** Replay every prod fixture title through old vs new and bucket the diffs:
   *  newly stripped (the intended broadening), changed strip (review), and —
   *  crucially — NO-LONGER-stripped (a regression that must be empty). */
  private def replayFixture(oldSet: TitleRuleSet, newSet: TitleRuleSet): Unit = {
    if (!Files.exists(fixture)) {
      println(s"\n[preview] fixture $fixture absent — skipping corpus replay.")
      return
    }
    val titles = Files.readAllLines(fixture).asScala.toSeq.filter(_.nonEmpty)
    def mergeKey(s: TitleRuleSet, t: String): String = TitleNormalizer.sanitize(s.canonical(t))

    val newly   = scala.collection.mutable.ListBuffer.empty[(String, String)]
    val changed = scala.collection.mutable.ListBuffer.empty[(String, String, String)]
    val lost    = scala.collection.mutable.ListBuffer.empty[(String, String, String)]
    titles.foreach { t =>
      val o = oldSet.search(t); val n = newSet.search(t)
      if (o != n) {
        if (o == t) newly += ((t, n))                 // old left it, new strips → broadening
        else if (n == t) lost += ((t, o, n))          // old stripped, new doesn't → REGRESSION
        else changed += ((t, o, n))
      }
    }
    val mergeShift = titles.filter(t => mergeKey(oldSet, t) != mergeKey(newSet, t))

    println(s"\n[preview] ${titles.size} prod titles replayed.")
    println(s"[preview] newly stripped (intended broadening): ${newly.size}")
    newly.foreach { case (t, n) => println(s"    \"$t\"  →  \"$n\"") }
    println(s"[preview] changed strip (review): ${changed.size}")
    changed.foreach { case (t, o, n) => println(s"    \"$t\"\n        was: \"$o\"\n        now: \"$n\"") }
    println(s"[preview] merge-key shifts: ${mergeShift.size}")
    mergeShift.foreach(t => println(s"    \"$t\"  →  merges as \"${newSet.canonical(t)}\""))
    println(s"[preview] REGRESSIONS (old stripped, new no longer — must be 0): ${lost.size}")
    lost.foreach { case (t, o, _) => println(s"    !! \"$t\" was stripped to \"$o\", now untouched") }
  }

  private def apply(): Unit = {
    val repo = new MongoTitleRulesRepository()
    if (!repo.enabled) { println("\n[apply] titleRules repository disabled — set MONGODB_URI (flyctl proxy + .env.local). Aborting."); return }
    try {
      var upserts = 0; var deletes = 0; var ruleHits = 0
      repo.loadRecords().foreach { rec =>
        val newNormal = rec.rules.flatMap(rewriteOrDrop)
        val newLast   = rec.lastRules.flatMap(rewriteOrDrop)
        if (newNormal != rec.rules || newLast != rec.lastRules) {
          ruleHits += (rec.rules.size - newNormal.size) + (rec.lastRules.size - newLast.size)
          if (newNormal.isEmpty && newLast.isEmpty) { repo.deleteRecord(rec.id); deletes += 1 }
          else { repo.upsertRecord(rec.copy(rules = newNormal, lastRules = newLast)); upserts += 1 }
        }
      }
      println(s"\n[apply] done — upserted $upserts record(s), deleted $deletes, dropped $ruleHits merged rule(s).")
      println("[apply] the worker change-stream backfill will re-key/re-enrich the affected rows.")
      println("[apply] next: regenerate the mirror (scripts.DumpTitleRules) and the page/e2e snapshots, then commit.")
    } finally repo.close()
  }
}
