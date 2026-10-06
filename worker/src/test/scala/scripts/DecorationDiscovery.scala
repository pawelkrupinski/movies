package scripts

import models.Country
import services.identity._
import services.identity.ListingShape
import services.movies.{ListingKey, TitleContainment, TitleNormalizer}
import services.titlerules.{ExtraTitleRules, RuleScope, TitleRule, TitleRuleSet, TitleRules}
import tools.UnmatchedClusters

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.regex.Pattern

/**
 * The weekly DECORATION DISCOVERY (`.github/workflows/decoration-discovery.yml`): mines the listing titles the
 * identity pipeline leaves unmatched (the unmatched clusters' fixture, [[UnmatchedClusters]]) for edge runs that
 * recur around many different titles — a venue's programme banner ("Edukacja Młode Horyzonty"), a format or
 * premiere tag — proposes each as a search strip ([[services.titlerules.ExtraTitleRules.discovered]]), and MEASURES
 * every proposal on its own against the fixture's replay:
 *
 *  - the clusters a proposal's new search titles touch are resolved again with it and without it (the control),
 *    and the agreement stage replayed over both — a TMDB question the capture lacks answered live when a TMDB key
 *    is configured, else the proposal is UNMEASURED and not kept;
 *  - each listing whose take moved is judged: one that took a film is RIGHT or WRONG by `labels.tsv` (a double
 *    programme taking a film is wrong), else UNJUDGED; one whose film switched is SWITCHED; one that lost its film
 *    is LOST — fine, as long as no part of a real title was stripped, which is checked before: no known film title
 *    (the capture's TMDB records, original and alternative titles, and every search hit) may START with a prefix
 *    run or END with a suffix run.
 *
 * KEPT: at least one right, none wrong, none switched, nothing unanswered. `--apply` writes the kept proposals into
 * `ExtraTitleRules.scala`; `--report` writes the evidence table the bot's PR carries.
 *
 *   sbt "worker/Test/runMain scripts.DecorationDiscovery [--apply] [--report <file.md>] [--limit <n>] [--only <side>:<run>,…]"
 */
object DecorationDiscovery {

  /** A candidate decoration: a token run at one edge of `films` different unmatched titles, billed by `venues`. */
  final case class Candidate(side: String, run: Seq[String], films: Int, venues: Int, examples: Seq[String]) {
    def text: String = run.mkString(" ")
  }

  /** A candidate as a search strip, with the titles it was built on and what each searches as with it. */
  final case class Proposal(candidate: Candidate, rule: TitleRule, examples: Seq[(String, String)])

  /** One listing whose take a proposal moved. */
  final case class Change(country: String, venue: String, rawTitle: String, before: String, after: String, verdict: String)

  final case class Evaluation(proposal: Proposal, affected: Int, changes: Seq[Change], unanswered: Int) {
    private def count(v: String) = changes.count(_.verdict == v)
    def right: Int = count(Right); def wrong: Int = count(Wrong); def switched: Int = count(Switched)
    def lost: Int = count(Lost); def unjudged: Int = count(Unjudged)
    def kept: Boolean = unanswered == 0 && right >= 1 && wrong == 0 && switched == 0
    def outcome: String =
      if (unanswered > 0) s"unmeasured ($unanswered unanswered)" else if (kept) "kept" else if (wrong > 0 || switched > 0) "rejected" else "no gain"
  }

  val Right = "right"; val Wrong = "wrong"; val Switched = "switched"; val Lost = "lost"; val Unjudged = "unjudged"

  /** The marker line in `ExtraTitleRules.scala` the kept proposals are inserted above. */
  val Marker = "// decoration-discovery: proposals are inserted above this line"
  val RulesSource: Path = Path.of("common", "src", "main", "scala", "services", "titlerules", "ExtraTitleRules.scala")

  // ── mining ────────────────────────────────────────────────────────────────────────────────

  /** Edge runs recurring around at least `minRemainders` different `titles` (each a venue and the title it searches),
   *  that neither `known` resolver decorations nor any `lexicon` title already carry, and that strip no real title. */
  def mine(titles: Seq[(String, String)], lexicon: Iterable[String], known: TitleDecorations, minRemainders: Int): Seq[Candidate] = {
    val edges = Edges(lexicon)
    val all = TitleDecorations.candidates(titles, lexicon, known, minRemainders)
      .map(d => Candidate(d.side, d.decoration.split(" ").toSeq, d.films, d.venues, d.examples))
      .filterNot(c => edges.stripsRealTitle(c.side, c.run))
    // a run that only ever bills as part of a longer one ("dismember the" inside "dismember the alamo") is that run
    all.filterNot(c => all.exists(o => o.side == c.side && o.run.size > c.run.size && o.films == c.films && o.venues == c.venues &&
      (if (c.side == "prefix") o.run.startsWith(c.run) else o.run.endsWith(c.run))))
  }

  /** Every known title's leading and trailing token runs, up to [[TitleDecorations.CandidateRun]] words. */
  final case class Edges(lexicon: Iterable[String]) {
    private val (prefixes, suffixes) = {
      val titles = lexicon.iterator.map(TitleContainment.tokens).filter(_.nonEmpty).toSet
      val n = TitleDecorations.CandidateRun
      (titles.flatMap(t => (1 to math.min(n, t.size)).map(t.take)), titles.flatMap(t => (1 to math.min(n, t.size)).map(t.takeRight)))
    }
    /** Whether a known title STARTS with `run` (a prefix) or ENDS with it (a suffix): stripping it there would cut
     *  that film's own title ("vol 2" off "Kill Bill: Vol. 2"). */
    def stripsRealTitle(side: String, run: Seq[String]): Boolean =
      if (side == "prefix") prefixes(run) else suffixes(run)
  }

  /** Whether `searched` bills `candidate`'s run at its edge, with words left beside it. */
  def carries(candidate: Candidate, searched: String): Boolean = {
    val ts = TitleContainment.tokens(searched)
    ts.size > candidate.run.size && (if (candidate.side == "prefix") ts.startsWith(candidate.run) else ts.endsWith(candidate.run))
  }

  // ── the rule ──────────────────────────────────────────────────────────────────────────────

  private val WordRun = """[\p{L}\p{N}\p{M}]+""".r

  /** `title`'s words as billed, when each is exactly one of its tokens. */
  private def surfaceWords(title: String): Option[Seq[String]] = {
    val words = WordRun.findAllIn(title).toSeq
    Option.when(words.map(TitleContainment.tokens) == words.map(w => Seq(TitleContainment.tokens(w).mkString)) &&
      words.flatMap(TitleContainment.tokens) == TitleContainment.tokens(title))(words)
  }

  /** `candidate` as a search strip built from the spellings `carriers` bill it in: its words in any of those
   *  spellings (or folded), any case, separated by anything but letters and digits, at the title's start followed by
   *  a separator (a prefix) or at its end after one (a suffix) — never a word's part, never the whole title. `None`
   *  when it does not strip the run off every carrier it was built on. */
  def ruleFor(candidate: Candidate, carriers: Seq[String]): Option[Proposal] = {
    val n = candidate.run.size
    val spellings = carriers.flatMap(surfaceWords).filter(_.size > n).map(ws => if (candidate.side == "prefix") ws.take(n) else ws.takeRight(n))
      .filter(_.map(w => TitleContainment.tokens(w).mkString) == candidate.run)
    Option.when(spellings.nonEmpty) {
      val words = candidate.run.indices.map { i =>
        (spellings.map(_(i).toLowerCase(java.util.Locale.ROOT)) :+ candidate.run(i)).distinct.sorted.map(Pattern.quote).mkString("(?:", "|", ")")
      }
      val body = words.mkString("""[^\p{L}\p{N}]+""")
      val pattern =
        if (candidate.side == "prefix") s"""(?iu)^[\\s\\[(]*$body(?![\\p{L}\\p{N}])[\\s\\-–—:|/.,;)\\]]+(?=[\\p{L}\\p{N}„"“'(\\[])"""
        else s"""(?iu)(?<=[\\p{L}\\p{N}\\p{M})\\]."'”!?])[\\s\\-–—:|/.,;(\\[]+$body[\\s)\\].!]*$$"""
      val id = s"xtra-discovered-${candidate.side}-${candidate.run.mkString("-")}"
      val rule = TitleRule(id, RuleScope.GlobalStructural, None, pattern, "", applyAll = false, order = 0,
        note = Some(s"'${if (candidate.side == "prefix") s"${candidate.text} <film>" else s"<film> ${candidate.text}"}' — discovered around " +
          s"${candidate.films} unmatched titles at ${candidate.venues} venue(s) (${candidate.examples.take(3).mkString(", ")})"))
      Proposal(candidate, rule, carriers.distinct.sorted.map(c => c -> rule(c).trim))
    }.filter(p => p.examples.forall { case (from, to) =>
      val ts = TitleContainment.tokens(from)
      TitleContainment.tokens(to) == (if (candidate.side == "prefix") ts.drop(n) else ts.dropRight(n))
    })
  }

  // ── judging ───────────────────────────────────────────────────────────────────────────────

  /** What became of one listing: `before` and `after` its takes ("" for none) — `None` when unmoved. A take where
   *  there was none is right or wrong by `labelled` (a double programme's is wrong whatever the labels say), else
   *  unjudged; a lost take is lost; a different film is switched. */
  def judge(before: String, after: String, billsSeveral: => Boolean, labelled: => Option[Boolean]): Option[String] =
    if (before == after) None
    else if (after.isEmpty) Some(Lost)
    else if (before.nonEmpty) Some(Switched)
    else if (billsSeveral) Some(Wrong)
    else Some(labelled.fold(Unjudged)(if (_) Right else Wrong))

  // ── measuring ─────────────────────────────────────────────────────────────────────────────

  /** One country's capture, its baseline replay, and the TMDB answers asked live beyond it (shared by proposals). */
  final class Bench(replays: CaptureReplay, labels: Seq[UnmatchedClusters.Label]) {
    private val capture       = replays.capture
    val country: Country      = capture.country
    val normalizer            = replays.normalizer
    private val byKey         = capture.listings.map(l => l.key -> l).toMap
    val baseline: Seq[UnmatchedClusters.Take] = UnmatchedClusters.takes(capture, UnmatchedClusters.replay(capture))

    /** The listings no take covers: what mining reads, as (venue, title, the title searched). */
    def unmatched: Seq[(String, String, String)] = {
      val taken = baseline.map(t => (t.venue, t.rawTitle)).toSet
      capture.listings.filterNot(l => taken((l.venue, l.rawTitle))).map(l => (l.venue, l.title, normalizer.apiQuery(l.title).trim))
    }

    /** Every title the capture knows a film by: TMDB's records (title, original, alternatives) and search hits. */
    def lexicon: Seq[String] =
      capture.films.values.flatten.flatMap(f => Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles).toSeq ++
        capture.queries.values.flatten.flatMap(h => Seq(h.title) ++ h.originalTitle)

    /** `listings` searched under `with`, each as its title gives. */
    private def searched(withRules: TitleNormalizer): Map[ListingKey, Listing] = capture.listings.flatMap { l =>
      val q = Some(withRules.apiQuery(l.title).trim).filter(x => x.nonEmpty && x != l.title.trim)
      Option.when(q != l.searchTitle)(l.key -> l.copy(searchTitle = q))
    }.toMap

    private def replay(listings: Seq[Listing], touched: Set[ResolverDecision], n: TitleNormalizer): (Seq[UnmatchedClusters.Take], Int) =
      replays.replay(listings, touched, n)

    /** `proposal` measured alone against the control: `None` when it changes no listing's search title here. */
    def evaluate(proposal: Proposal): Option[Evaluation] = {
      val withRule = new TitleNormalizer(TitleRuleSet(normalizer.rules.rules :+ proposal.rule))
      val changed  = searched(withRule)
      Option.when(changed.nonEmpty) {
        val newKeys  = changed.values.flatMap(_.searchTitle).map(normalizer.sanitize).toSet
        val touched  = capture.decisions.filter(d => d.members.exists(changed.contains) ||
          d.members.flatMap(byKey.get).exists(l => newKeys(normalizer.sanitize(l.cleanTitle)))).toSet
        val (control, controlGaps) = replay(capture.listings, touched, normalizer)
        val (now, gaps)            = replay(capture.listings.map(l => changed.getOrElse(l.key, l)), touched, withRule)
        def filmOf(takes: Seq[UnmatchedClusters.Take]) = takes.groupMapReduce(t => (t.venue, t.rawTitle))(t => t)((a, _) => a)
        val (was, is) = (filmOf(control), filmOf(now))
        val changes = capture.listings.groupBy(l => (l.venue, l.rawTitle)).toSeq.sortBy(_._1).flatMap { case (k @ (venue, raw), billed) =>
          val before  = was.get(k).fold("")(_.film)
          val after   = is.get(k)
          val listing = billed.head
          judge(before, after.fold("")(_.film),
            ListingShape.billsSeveral(listing) || IdentityMeasures.billsTwoWorks(IdentityMeasures.Listing(listing.title, Some(listing.rawTitle), decorations = TitleDecorations.resolver)),
            after.flatMap(UnmatchedClusters.verdict(_, labels)))
            .map(v => Change(country.code, venue, raw, was.get(k).fold("")(t => s"${t.film} ${t.title}"), after.fold("")(t => s"${t.film} ${t.title}"), v))
        }
        Evaluation(proposal, changed.size, changes, math.max(0, gaps - controlGaps))
      }
    }
  }

  /** Every proposal measured over every bench it touches, its changes pooled. */
  def evaluateAll(proposals: Seq[Proposal], benches: Seq[Bench]): Seq[Evaluation] =
    proposals.map { p =>
      val each = benches.flatMap(_.evaluate(p))
      Evaluation(p, each.map(_.affected).sum, each.flatMap(_.changes), each.map(_.unanswered).sum)
    }

  // ── writing ───────────────────────────────────────────────────────────────────────────────

  private def scalaString(s: String): String = {
    val escaped = s.flatMap {
      case '\\' => "\\\\"; case '"' => "\\\""; case '\n' => "\\n"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    }
    "\"" + escaped + "\""
  }

  /** `source` (ExtraTitleRules.scala) with `kept` inserted above [[Marker]], each beside the titles it was measured on. */
  def splice(source: String, kept: Seq[Proposal]): String = {
    val at = source.indexOf(Marker)
    require(at >= 0, s"no '$Marker' line in the rules source")
    val lineStart = source.lastIndexOf('\n', at) + 1
    val indent    = source.substring(lineStart, at)
    val entries   = kept.map { p =>
      val r = p.rule
      val examples = p.examples.map { case (a, b) => s"${scalaString(a)} -> ${scalaString(b)}" }.mkString(", ")
      s"${indent}Discovered(TitleRule(${scalaString(r.id)}, GlobalStructural, None, ${scalaString(r.pattern)}, \"\", applyAll = false, order = 0,\n" +
        s"$indent  note = Some(${scalaString(r.note.getOrElse(""))})),\n$indent  Seq($examples)),\n"
    }.mkString
    source.substring(0, lineStart) + entries + source.substring(lineStart)
  }

  private def cell(s: String) = s.replace("|", "\\|").replace("\n", " ")

  /** The PR body: what was kept, every proposal's measure, and each kept rule's evidence. */
  def report(evaluations: Seq[Evaluation], fixtures: String): String = {
    val kept = evaluations.filter(_.kept)
    val sb = new StringBuilder
    sb ++= s"## Decoration discovery\n\nMined from $fixtures. ${evaluations.size} proposal(s) measured, ${kept.size} kept " +
      "(≥1 right, 0 wrong, 0 switched, nothing unanswered; no known film title starts or ends with what it strips).\n\n"
    sb ++= "| outcome | side | decoration | titles | venues | searched | right | wrong | switched | lost | unjudged |\n|---|---|---|---|---|---|---|---|---|---|---|\n"
    evaluations.sortBy(e => (!e.kept, -e.right, e.proposal.candidate.text)).foreach { e =>
      val c = e.proposal.candidate
      sb ++= s"| ${e.outcome} | ${c.side} | `${cell(c.text)}` | ${c.films} | ${c.venues} | ${e.affected} | ${e.right} | ${e.wrong} | ${e.switched} | ${e.lost} | ${e.unjudged} |\n"
    }
    kept.foreach { e =>
      sb ++= s"\n### `${e.proposal.rule.id}`\n\n`${cell(e.proposal.rule.pattern)}`\n\n| country | venue | listing | before | after | verdict |\n|---|---|---|---|---|---|\n"
      e.changes.foreach(ch => sb ++= s"| ${ch.country} | ${cell(ch.venue)} | ${cell(ch.rawTitle)} | ${cell(ch.before)} | ${cell(ch.after)} | ${ch.verdict} |\n")
    }
    sb.toString
  }

  def main(args: Array[String]): Unit = {
    val apply  = args.contains("--apply")
    val opts   = args.sliding(2).collect { case Array(k, v) if k.startsWith("--") && !v.startsWith("--") => k.stripPrefix("--") -> v }.toMap
    val limit  = opts.get("limit").map(_.toInt).getOrElse(60)
    val min    = opts.get("min-remainders").map(_.toInt).getOrElse(TitleDecorations.MinCandidateRemainders)
    val labels = UnmatchedClusters.readLabels(UnmatchedClusters.Directory.resolve("labels.tsv"))
    val benches = CaptureReplay.all().map(new Bench(_, labels))
    println(s"TMDB gaps: ${if (CaptureReplay.asksLive) "asked live" else "NOT asked (no TMDB_API_KEY) — a proposal asking one is unmeasured"}")
    val lexicon  = benches.flatMap(_.lexicon).distinct
    val titles   = benches.flatMap(_.unmatched)
    val every    = TitleRules.all ++ ExtraTitleRules.all
    // `--only prefix:kino-konesera,suffix:edukacja-mlode-horyzonty` (words joined by anything but a letter: runMain splits on spaces): measure these runs instead of mining (a positive
    // control, or a human's own proposal) — still refused when a known title starts or ends with one
    val named = opts.get("only").map(_.split(",").toSeq.map(_.split(":", 2)).collect { case Array(side, run) =>
      Candidate(side.trim, TitleContainment.tokens(run), 0, 0, Nil) }.filterNot(c => Edges(lexicon).stripsRealTitle(c.side, c.run)))
    val candidates = named.getOrElse(mine(titles.map(t => t._1 -> t._3), lexicon, TitleDecorations.resolver, min).take(limit))
    val proposals = candidates.flatMap { c =>
      val carrying = titles.filter(t => carries(c, t._3))
      ruleFor(c, carrying.map(_._3).distinct).map { p =>
        // what DiscoveredDecorationsSpec holds the rule to: every rule beside it, against every rule without it
        val (withIt, without) = (new TitleNormalizer(TitleRuleSet(every :+ p.rule)), new TitleNormalizer(TitleRuleSet(every)))
        p.copy(examples = carrying.map(_._2).distinct.sorted.map(t => t -> withIt.apiQuery(t).trim).filter { case (t, q) => without.apiQuery(t).trim != q }.take(5))
      }.filter(_.examples.nonEmpty)
    }
    println(s"mined ${proposals.size} proposal(s) from ${titles.size} unmatched listing titles against ${lexicon.size} known titles")
    val evaluations = evaluateAll(proposals, benches)
    evaluations.foreach(e => println(f"${e.outcome}%-28s ${e.proposal.candidate.side}%-6s ${e.proposal.candidate.text}%-40s searched ${e.affected}%3d " +
      f"right ${e.right} wrong ${e.wrong} switched ${e.switched} lost ${e.lost} unjudged ${e.unjudged}"))
    val kept = evaluations.filter(_.kept).map(_.proposal)
    println(s"kept ${kept.size}: ${kept.map(_.rule.id).mkString(", ")}")
    opts.get("report").foreach(p => Files.writeString(Path.of(p), report(evaluations, "test/resources/fixtures/identity-unmatched"), StandardCharsets.UTF_8))
    if (apply && kept.nonEmpty) {
      Files.writeString(RulesSource, splice(Files.readString(RulesSource, StandardCharsets.UTF_8), kept), StandardCharsets.UTF_8)
      println(s"wrote ${kept.size} rule(s) into $RulesSource")
    }
  }
}
