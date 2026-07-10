package services.movies

import java.util.Locale

/**
 * The single source of truth for peeling a screen-format / language tag off a
 * film title — `(Napisy PL)`, `[2D DUBBING]`, `- 2D dubbing`, `/napisy`,
 * `…_3D`, a bare trailing `NAPISY 2D` — into display tokens (2D / 3D / IMAX /
 * 4DX / ATMOS / DUB / NAP / LEK) while rewriting the title to its clean base (the format
 * words AND their adjacent brackets / slashes / dashes removed).
 *
 * Lives in `common` so BOTH the ingest choke point
 * (`MovieCache.recordCinemaScrape`, which every scrape path shares) and the
 * worker cinema clients (via `ScraperParse`, which delegates here) use one
 * implementation, instead of format handling being scattered across per-client
 * code and the title-rule set.
 *
 * ONE carve-out: a Ukrainian-language screening ("… ukraiński dubbing") is a
 * distinct audience that stays its own card, so a `dub`/`lektor` word is NOT
 * stripped when it directly follows `ukraiński`/`ukrainian` — the title is kept
 * whole. Only dub/lektor are guarded; `napisy`/`2D`/etc. still strip.
 */
object FormatTags {

  /** Lower-case format/version word → the display token it maps to. The token
   *  vocabulary is fixed across the app so all sources agree. Words droppable
   *  from a title but with no version meaning (premiera, dolby, …) are NOT here;
   *  they're stripped without yielding a token. ("dolby" is the bare carrier for
   *  "Dolby Atmos" — it yields no token of its own; the "atmos" word carries the
   *  ATMOS badge, so "Dolby Atmos" and a bare "Atmos" both surface one ATMOS.) */
  val FormatToken: Map[String, String] = Map(
    "napisy" -> "NAP", "nap" -> "NAP", "dubbing" -> "DUB", "dub" -> "DUB", "dubb" -> "DUB",
    "lektor" -> "LEK", "2d" -> "2D", "3d" -> "3D", "imax" -> "IMAX", "4dx" -> "4DX",
    "atmos" -> "ATMOS"
  )

  // FORMAT/version words only — screen format (2D/3D/IMAX/4DX/Dolby/Atmos) and
  // language version (dub/napisy/lektor). Deliberately NOT screening-type words
  // (premiera, pokaz, seans, specjalny, przedpremierowy, …): a premiere / special /
  // pre-premiere / senior-club screening is a distinct EVENT that stays its own
  // card (like a programme prefix or "+ event"), so those words are never stripped
  // — and keeping them out also avoids partial-stripping a multi-word event label
  // ("UROCZYSTA POLSKA PREMIERA" would otherwise lose only its last word).
  private val FormatVersionWords = Set(
    "2d", "3d", "imax", "4dx", "dolby", "atmos",
    "dubbing", "dubb", "dub", "napisy", "nap", "lektor", "lek")
  private val FormatSeparators = Set("-", "–", "—", "|", "/", ":")
  private val FormatBracketTag = """\s*\[[^\]]*\]\s*$""".r
  private val FormatParenTag   =
    """(?i)\s*\((?:[^)]*\b(?:2D|3D|IMAX|DOLBY|ATMOS|4DX|dubbing|napisy|lektor)\b[^)]*)\)\s*$""".r
  // Underscore-glued format/version tag — some bilety24 portals (Forum Bolesławiec)
  // join the version word straight to the title with an underscore:
  // "Supergirl_dubbing", "Spider-Man. Całkiem nowy dzień_3D". Un-glue ONLY before a
  // known format/version word so a legitimate underscore ("Seans w ciemno_7.26",
  // the "_DKF"/"_FKS" programme tags) is left intact and those titles aren't re-split.
  private val GluedFormatUnderscore =
    ("(?i)_(?=(?:" + FormatVersionWords.toSeq.sortBy(-_.length).mkString("|") + """)\b)""").r
  // Slash-glued version tag with no surrounding spaces — some bilety24 venues
  // (Kino Oskard) join it straight on: "Supergirl/dubbing", "…dzień/napisy".
  // Un-glue ONLY before a known version word (via the look-ahead), so a real
  // slashed title ("AC/DC", "Face/Off", "either/or") is never split.
  private val GluedFormatSlash =
    ("(?i)/(?=(?:" + FormatVersionWords.toSeq.sortBy(-_.length).mkString("|") + """)\b)""").r

  // Ukrainian-screening guard (see class doc). `DubLektorWord` are the guarded
  // trailing tokens; `UaWord` is the exact preceding token; `UaGuardedTag` matches
  // a whole `[..]`/`(..)` tag naming the UA dub/lektor version so it's kept intact.
  private val DubLektorWord = Set("dub", "dubb", "dubbing", "lektor", "lek")
  private val UaWord        = """(?i)^ukrai(?:[ńn]ski|nian)$""".r
  private val UaGuardedTag  = """(?i)ukrai(?:[ńn]ski|nian)\W+(?:dubb?(?:ing)?|lektor|lek)\b""".r

  /** The bare lower-cased word of a title token (paren/bracket/punctuation
   *  peeled), or `""` for an empty token. */
  private def bareWord(tok: String): String =
    tok.toLowerCase(Locale.ROOT).replaceAll("""[\[\]().,]""", "")

  private def isDroppableTag(tok: String): Boolean = {
    val w = bareWord(tok)
    w.isEmpty || FormatSeparators.contains(w) || FormatVersionWords.contains(w)
  }

  /** True when the trailing token is a dub/lektor word directly preceded by
   *  `ukraiński`/`ukrainian` — the Ukrainian version marker, kept (not stripped). */
  private def uaGuardedTail(toks: Vector[String]): Boolean =
    toks.length >= 2 && DubLektorWord.contains(bareWord(toks.last)) &&
      UaWord.findFirstIn(bareWord(toks(toks.length - 2))).isDefined

  /** Strip the trailing format/version tags a ticketing-portal title carries, so
   *  the same film's screening variants collapse to one title and merge. */
  def stripFormatTags(raw: String): String = extractFormatTags(raw)._1

  /** Like [[stripFormatTags]], but also returns the display [[FormatToken]]s
   *  recognised among the stripped tags, so a screening's version can surface as a
   *  `Showtime.format` badge. Tokens de-duplicated, ordered by first appearance
   *  left-to-right. Words with no version meaning (dolby, premiera, …) yield no
   *  token. A Ukrainian dub/lektor tag is kept whole (guard) and yields no token. */
  def extractFormatTags(raw: String): (String, List[String]) = {
    var t    = GluedFormatSlash.replaceAllIn(
                 GluedFormatUnderscore.replaceAllIn(raw.replaceAll("\\s+", " ").trim, " "), " ")
    var previous = ""
    val dropped = scala.collection.mutable.Set.empty[String]
    def uaGuarded(tag: String): Boolean = UaGuardedTag.findFirstIn(tag).isDefined
    while (t != previous) {
      previous = t
      // Tokens peeled inside a [..]/(..) tag also carry version meaning, so
      // capture them before deleting the tag — UNLESS the tag names the Ukrainian
      // dub/lektor version, which is kept whole so it stays its own card.
      FormatBracketTag.findFirstIn(t).filterNot(uaGuarded).foreach { tag =>
        captureTagWords(tag, dropped)
        t = FormatBracketTag.replaceFirstIn(t, "").trim
      }
      FormatParenTag.findFirstIn(t).filterNot(uaGuarded).foreach { tag =>
        captureTagWords(tag, dropped)
        t = FormatParenTag.replaceFirstIn(t, "").trim
      }
      var toks = t.split(" ").filter(_.nonEmpty).toVector
      while (toks.length > 1 && isDroppableTag(toks.last) && !uaGuardedTail(toks)) {
        captureTagWords(toks.last, dropped)
        toks = toks.dropRight(1)
      }
      // Some portals glue the format tag to the title with a separator and a
      // space ("Straszny Film- 2D dubbing"): dropping the format words leaves a
      // dangling "Film-". Trim a trailing separator so the variants collapse.
      t = toks.mkString(" ").replaceAll("""\s*[-–—|/:]+\s*$""", "").trim
    }
    val tokens = raw.toLowerCase(Locale.ROOT)
      .split("""[\s\[\]().,/|:_-]+""")
      .iterator
      .filter(dropped.contains)
      .flatMap(FormatToken.get)
      .distinct
      .toList
    (t, tokens)
  }

  /** Record every [[FormatToken]]-bearing word in `chunk` (a single token or a
   *  whole `(...)`/`[...]` tag) into `out`. */
  private def captureTagWords(chunk: String, out: scala.collection.mutable.Set[String]): Unit =
    chunk.split("""[\s\[\]()]+""").iterator
      .map(w => bareWord(w))
      .filter(FormatToken.contains)
      .foreach(out.add)

  /** The [[FormatToken]]s named as whole words anywhere in `text` — a
   *  language/version line like "polski lektor" or "napisy polskie", which some
   *  cinemas expose as a detail-page field rather than a title suffix.
   *  De-duplicated and sorted for a stable order. */
  def formatTokensIn(text: String): List[String] = {
    val words = text.toLowerCase(Locale.ROOT).split("""[^\p{L}\p{N}]+""").toSet
    FormatToken.iterator.collect { case (w, t) if words.contains(w) => t }.toList.distinct.sorted
  }
}
