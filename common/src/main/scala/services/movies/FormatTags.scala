package services.movies

import java.util.Locale

/**
 * The single source of truth for peeling a screen-format / language tag off a
 * film title — `(Napisy PL)`, `[2D DUBBING]`, `- 2D dubbing`, `/napisy`,
 * `…_3D`, a bare trailing `NAPISY 2D` — into display tokens (2D / 3D / IMAX /
 * 4DX / ATMOS / DUB / NAP / LEK) while rewriting the title to its clean base (the format
 * words AND their adjacent brackets / slashes / dashes removed). With them, stacked in any order, the showing's
 * notes that carry no badge: a box office's certificate and running time, a note between asterisks, a German
 * re-release mark.
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
    "atmos" -> "ATMOS",
    // English-language venues' version words, and the print/projection a repertory screening names
    // ("Spirited Away (Dubbed)", "Collateral (35mm)"); SUB/DUB take the country's own spelling in
    // `ScreeningTokens` ("2d sub" in Poland is NAP).
    "sub" -> "SUB", "subs" -> "SUB", "subtitled" -> "SUB", "subtitles" -> "SUB", "dubbed" -> "DUB",
    "35mm" -> "35MM", "70mm" -> "70MM", "4k" -> "4K",
    // Germany's language versions, which its venues write into the title as often as into a field.
    "ov" -> "OV", "omu" -> "OmU", "omeu" -> "OmeU", "df" -> "DF", "originalversion" -> "OV"
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
    "dubbing", "dubb", "dub", "napisy", "nap", "lektor", "lek",
    "sub", "subs", "subtitled", "subtitles", "dubbed", "35mm", "70mm", "4k",
    "ov", "omu", "omeu", "df", "engl", "originalversion")
  private val FormatSeparators = Set("-", "–", "—", "|", "/", ":")
  // "… - ON 35MM", "… in 70mm", Kino Kolory's "REQUIEM DLA SNU w 4K", "Carmen in 3D", "… The Tour Live In 3D": a
  // print or a screen is billed with a preposition, which goes with it (left behind, the title ended "REQUIEM DLA
  // SNU w", "… The Tour Live In").
  private val FormatPreposition = """(?i)\s+(?:on|in|w)\s+(\d{2}mm|4k|[23]d|imax|4dx)\s*$""".r
  private val FormatBracketTag = """\s*\[[^\]]*\]\s*$""".r
  private val FormatParenTag   =
    """(?i)\s*\((?:[^)]*\b(?:2D|3D|IMAX|DOLBY|ATMOS|4DX|dubbing|napisy|lektor|dubbed|subtitled|subtitles|35mm|70mm|4K(?!\s*restor)|OV|OmU|OmeU|Originalversion|D-?BOX)\b[^)]*)\)\s*$""".r
  // German venues spell the original version out in brackets — "Verity (Original English Version)", "The Rocky
  // Horror Picture Show (1975/Original English Version)" — and bracket a hyphenated screen, "Forgotten Island
  // (3-D)". Both are rewritten to their token's own word before the strips, inside brackets only: a bare "3-D" is
  // a film's own title ("Jaws 3-D").
  private val BracketedOriginalVersion  = """(?i)(?<=[(/])\s*original\s+(?:english\s+)?version\s*(?=\))""".r
  private val BracketedHyphenatedScreen = """(?i)\(\s*([234])-D\s*\)""".r
  // Corso Stuttgart bills the version as a phrase, "Forgotten Island in (Original English Version)": its "in" goes
  // with it. Lower-case only — a title's own last word is capitalised ("Let Me In").
  private val OriginalVersionPreposition = """(?<=\S)\s+in\s+(?=\((?i:\s*original\s+(?:english\s+)?version\s*)\))""".r
  // The showing's notes a box office bills behind the title, none of them a badge. A BBFC certificate — "Digger
  // (15)", "Digger - Cert 15", "EOS - Monet (Cert. TBC)" — with the running time or short year it runs on into ("(15)
  // 129 mins", "(15) 26"); a release year between them stays the title's ("Bad Apples (15) (2026) 100 mins" → "Bad
  // Apples (2026)"), for the year reading and the canonical year strip. And a German re-release mark — "(WA)", "(WA:
  // 2026)", "(Wiederaufführung)", or a box office's "(WA:" cut short. Each is a card's worth of split (Lanternhouse's
  // "Digger (15)", CineStar's "Catching Fire (OV) (WA)") if it stays.
  private val Certificate = services.titlerules.TitleRulePlaceholders.all("CERT")
  private val CertificateNote =
    (s"""(?i)(?<=\\S)\\s*(?:[-–—]\\s*Cert(?:ificate)?\\.?\\s+$Certificate|\\(\\s*(?:Cert(?:ificate)?\\.?\\s*)?$Certificate(?:\\s+TBC)?\\s*\\))""" +
      """(\s*\(\s*(?:19|20)\d{2}\s*\))?(?:\s+\d{2,3}\s*mins?|\s+\d{2})?\s*$""").r
  // A showing's note a box office closes between asterisks after the rest — Electric Palace Bridport's "Wicker (15) 105
  // mins *MATINEE*", "… *SUBTITLED MATINEE*": words only, spaced off the title, so a title's own asterisks ("M*A*S*H",
  // "Monet*", "B****") stay. Its version words still badge the showing.
  private val AsteriskNote = """(?<=\S)\s+\*\s*\p{L}[\p{L}\s'&-]*\*\s*$""".r
  private val ReReleaseMark = """(?i)(?<=\S)\s*\(\s*(?:WA(?:\s*:?\s*\d{2,4}|\s*:)?|Wiederauff(?:ü|ue)hrung)\s*\)?\s*$""".r
  // The same tags written IN FRONT of the title — "OV: Digger", "(OmU) Verity", "ATMOS/OV: Verity", "Dolby
  // Cinema: Die Odyssee", "3D: Forgotten Island", "engl. OV: Der Herr der Ringe", "OMU - Searching for Sugar Man":
  // brackets, a colon, or a spaced dash closing a run of nothing but format and version words (a version's
  // language qualifier, "engl.", among them). Only those words, so "Dune: Part Two" keeps its colon.
  private val LeadingWord = """(?:dolby\s+cinema|dolby|atmos|imax|4dx|[23]d|ov|omu|omeu|df|engl\.?)"""
  private val LeadingWords = LeadingWord + """(?:(?:\s*[/,]\s*|\s+)""" + LeadingWord + """)*"""
  private val LeadingFormatTag =
    ("""(?i)^\s*(?:\(\s*""" + LeadingWords + """\s*\)\s*:?|""" + LeadingWords + """(?:\s*:|\s+[-–—]\s))\s*(?=\S)""").r
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

  // A dash glued to the version word after a space — "DIGGER -OmU" (a Cineamo venue). Spaced out so the word
  // drops like "Digger - OmU"; the space before it keeps a hyphenated title ("Spider-Man") whole.
  private val GluedFormatDash =
    ("(?i)\\s[-–](?=(?:" + FormatVersionWords.toSeq.sortBy(-_.length).mkString("|") + """)\b)""").r
  // …or by a bare hyphen, with no space at all — "Glaube-OmU" (Kino Utopia, DE recording 2026-10-12). Only a German
  // version word in its own casing and at the very end, so "Spider-Man" and "X-Men" stay whole.
  private val GluedVersionHyphen = """(?<=\p{L})-(?=(?:OmeU|OmU|OV)$)""".r

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
    (w.isEmpty || FormatSeparators.contains(w) || FormatVersionWords.contains(w)) && !closesEarlierBracket(tok)
  }

  /** A token closing a bracket it did not open ("SUBS)" of "Lalka (+ ENG SUBS)") is the END of a bracketed
   *  tag the bracket strips did not take whole; dropping it alone left "Lalka (+ ENG" as the title. */
  private def closesEarlierBracket(tok: String): Boolean =
    (tok.count(c => c == ')' || c == ']')) > (tok.count(c => c == '(' || c == '['))

  /** "PL" and Kino Cinema N's "pol" ("Lalka 2d pol"). */
  private val LanguageQualifiers = Set("pl", "pol", "english")

  /** True when the trailing token is a bare "PL" qualifying the version word
   *  before it ("Lalka 2D PL", "… | DUBBING PL", "Lalka | 2D | PL" — ekobilet and
   *  MSI venues). Only then: a title that merely ends in "PL" ("Kino PL") has no
   *  format word in front and is left whole. Separators between the two are
   *  skipped; the PL itself yields no token. */
  private def isLanguageQualifier(toks: Vector[String]): Boolean =
    LanguageQualifiers.contains(bareWord(toks.last)) &&
      toks.init.reverseIterator.map(bareWord).find(w => w.nonEmpty && !FormatSeparators.contains(w))
        .exists(FormatVersionWords.contains)

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
    val spelled = BracketedHyphenatedScreen.replaceAllIn(
      BracketedOriginalVersion.replaceAllIn(OriginalVersionPreposition.replaceAllIn(raw, " "), "OV"), "($1D)")
    var t    = GluedFormatDash.replaceAllIn(GluedFormatSlash.replaceAllIn(
                 GluedFormatUnderscore.replaceAllIn(GluedVersionHyphen.replaceAllIn(spelled.replaceAll("\\s+", " ").trim, " - "), " "), " "), " - ")
    var previous = ""
    val dropped = scala.collection.mutable.Set.empty[String]
    def uaGuarded(tag: String): Boolean = UaGuardedTag.findFirstIn(tag).isDefined
    while (t != previous) {
      previous = t
      LeadingFormatTag.findFirstIn(t).foreach { tag =>
        captureTagWords(tag, dropped)
        t = t.substring(tag.length).trim
      }
      AsteriskNote.findFirstIn(t).foreach { note =>
        captureTagWords(note, dropped)
        t = AsteriskNote.replaceFirstIn(t, "").trim
      }
      t = ReReleaseMark.replaceFirstIn(CertificateNote.replaceFirstIn(t, "$1"), "").trim
      // Tokens peeled inside a [..]/(..) tag also carry version meaning, so
      // capture them before deleting the tag — UNLESS the tag names the Ukrainian
      // dub/lektor version, which is kept whole so it stays its own card.
      // A tag that is the WHOLE title is the title — "[REC]", Balagueró's film, came out as no
      // title at all (US corpus 2026-10-01, served in Detroit and Omaha as an empty card).
      FormatBracketTag.findFirstIn(t).filterNot(uaGuarded).filter(_.trim != t).foreach { tag =>
        captureTagWords(tag, dropped)
        t = FormatBracketTag.replaceFirstIn(t, "").trim
      }
      FormatParenTag.findFirstIn(t).filterNot(uaGuarded).filter(_.trim != t).foreach { tag =>
        captureTagWords(tag, dropped)
        t = FormatParenTag.replaceFirstIn(t, "").trim
      }
      // After the bracket peels, which can expose a format behind its preposition ("… in 2D (OV)" → "… in 2D").
      t = FormatPreposition.replaceAllIn(t, " $1")
      var toks = t.split(" ").filter(_.nonEmpty).toVector
      while (toks.length > 1 && (isDroppableTag(toks.last) || isLanguageQualifier(toks)) && !uaGuardedTail(toks)) {
        captureTagWords(toks.last, dropped)
        toks = toks.dropRight(1)
      }
      // Some portals glue the format tag to the title with a separator and a
      // space ("Straszny Film- 2D dubbing"): dropping the format words leaves a
      // dangling "Film-". Trim a trailing separator so the variants collapse.
      t = toks.mkString(" ").replaceAll("""\s*[-–—|/:,]+\s*$""", "").trim
    }
    val tokens = spelled.toLowerCase(Locale.ROOT)
      .split("""[\s\[\]().,/|:_*-]+""")
      .iterator
      .filter(dropped.contains)
      .flatMap(FormatToken.get)
      .distinct
      .toList
    // A peel can leave a library-style filing at the end it bared — "Rocky Horror Picture Show, The (WA)" — which the
    // tidy-up's fronting, run before the peel, saw with the mark still behind it.
    (TitleText.frontArticle(t), tokens)
  }

  /** Record every [[FormatToken]]-bearing word in `chunk` (a single token or a
   *  whole `(...)`/`[...]` tag) into `out`. */
  private def captureTagWords(chunk: String, out: scala.collection.mutable.Set[String]): Unit =
    chunk.split("""[\s\[\]()/:,*]+""").iterator
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
