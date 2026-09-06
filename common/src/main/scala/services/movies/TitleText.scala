package services.movies

import java.util.Locale
import java.util.regex.Pattern

/**
 * The rule-INDEPENDENT half of title normalisation: pure string functions
 * (entity decoding, Roman-numeral folding, script detection, casing mechanics,
 * the well-formedness check) that no country's rule set can disagree about.
 * They live here, static, rather than being duplicated per [[TitleNormalizer]]
 * instance — every one is a function of its input alone, so there is nothing
 * to scope by country.
 */
object TitleText {

  // Precompiled hot-path patterns. `sanitize` / `stripPunct` run per movie ×
  // per cinema × per tick (plus every staging row and read-model projection);
  // `String.replaceAll` recompiles its `Pattern` on every call, so we compile
  // these once. `CombiningMarks` mirrors the NFD combining-mark strip; the
  // `NonAlnum*` pair drops the residual punctuation/whitespace, one Unicode-aware
  // (keeps Cyrillic/Greek/CJK letters) and one ASCII-only.
  private val CombiningMarks  = Pattern.compile("\\p{M}")
  private val NonAlnumUnicode = Pattern.compile("[^\\p{L}\\p{N}]+")
  private val NonAlnumAscii   = Pattern.compile("[^a-z0-9]+")

  // "Mortal Kombat 2" and "Mortal Kombat II" should collapse — onto the ARABIC
  // form (the spelling cinemas + TMDB actually use), so keys read `mortalkombat2`,
  // not `mortalkombatii`. Only MULTI-letter Roman numerals are converted: the
  // single letters I, V, X collide with real title words ("I Am Legend",
  // "Malcolm X", "V for Vendetta", Polish "i" = and), so converting them would
  // corrupt those titles. The cost is not unifying a bare Roman single-digit
  // ("Rocky V") with its Arabic form ("Rocky 5"), which cinema listings
  // effectively never use.
  private val RomanToArabic = Map(
    "II" -> "2", "III" -> "3", "IV" -> "4", "VI" -> "6", "VII" -> "7",
    "VIII" -> "8", "IX" -> "9", "XI" -> "11", "XII" -> "12", "XIII" -> "13",
    "XIV" -> "14", "XV" -> "15", "XVI" -> "16", "XVII" -> "17", "XVIII" -> "18",
    "XIX" -> "19", "XX" -> "20"
  )

  // Always-applied transformation: standalone (space-delimited) multi-letter Roman
  // numerals → Arabic, CASE-INSENSITIVELY so "Mortal Kombat II" (chains) and
  // "Mortal kombat ii" (a lower-casing cinema) fold to the same `mortalkombat2`
  // rather than splitting. `sanitize` runs this AFTER `canonical` (not before): a
  // decoration glued to a numeral with no separating space ("Mortal Kombat II-
  // dubbing" → token "II-") hides the numeral until canonical strips the
  // decoration, so normalising first stranded it as Roman while the stripped
  // display form ("Mortal Kombat II") deromanised it — the two then sanitized to
  // different keys and the film never settled (the staging re-divert loop).
  /** The clean-up every scraped title needs before anything reads it — decoding
   *  first, then spacing, because a decoded `&quot;` can itself end up flush
   *  against the next word.
   *
   *  Rule-independent by construction: no country's rule set disagrees that
   *  `&quot;` is a quote mark or that a sentence mark is followed by a space. */
  def tidy(title: String): String = spaceAfterSentenceMark(decodeEntities(title))

  // Numeric (`&#233;` / `&#xE9;`) and the named entities that actually turn up in
  // cinema listings. `&amp;` is decoded LAST so a double-encoded `&amp;quot;`
  // resolves to `&quot;` rather than being collapsed straight to a quote mark.
  private val NumericEntity = """&#(x?)([0-9a-fA-F]+);""".r
  private val NamedEntities = Seq(
    "&quot;" -> "\"", "&apos;" -> "'", "&nbsp;" -> " ", "&lt;" -> "<", "&gt;" -> ">",
    "&laquo;" -> "«", "&raquo;" -> "»", "&ldquo;" -> "\u201c", "&rdquo;" -> "\u201d",
    "&bdquo;" -> "\u201e", "&hellip;" -> "…", "&ndash;" -> "–", "&mdash;" -> "—")

  def decodeEntities(s: String): String = {
    if (!s.contains('&')) return s
    val numeric = NumericEntity.replaceAllIn(s, m =>
      scala.util.Try {
        val code = Integer.parseInt(m.group(2), if (m.group(1).isEmpty) 10 else 16)
        java.util.regex.Matcher.quoteReplacement(new String(Character.toChars(code)))
      }.getOrElse(java.util.regex.Matcher.quoteReplacement(m.matched)))
    NamedEntities.foldLeft(numeric) { case (acc, (e, c)) => acc.replace(e, c) }.replace("&amp;", "&")
  }

  /** Put back the space a listing dropped after a sentence mark: Kino Apollo
   *  publishes `…Maastricht!”Retransmisja letniego koncertu`, and the display
   *  title read `Andre rieu.niech żyje maastricht`.
   *
   *  Deliberately narrow — it fires only when at least two lower-case letters
   *  precede the mark and a letter follows, so initialisms ("S.W.A.T."), decimals
   *  ("Vol.2") and single-letter abbreviations keep their spacing. */
  private val MissingSentenceSpace = """(?<=\p{Ll}{2})([.!?][\u201d\u00bb"']?)(?=\p{L})""".r

  def spaceAfterSentenceMark(s: String): String =
    MissingSentenceSpace.replaceAllIn(s, m => java.util.regex.Matcher.quoteReplacement(m.group(1) + " "))

  def normalize(title: String): String =
    title.split(" ").map(word => RomanToArabic.getOrElse(word.toUpperCase(Locale.ROOT), word)).mkString(" ")

  // A token made only of roman-numeral letters — kept in caps when a shout is
  // down-cased so "Rocky BALBOA II" cases the name but leaves the sequel ("II").
  private val RomanNumeral = "^[IVXLCDM]+$".r

  private def isAllCapsWord(token: String): Boolean = {
    val ls = token.filter(_.isLetter)
    ls.nonEmpty && ls.forall(_.isUpper)
  }

  private[movies] def caseSegment(s: String): String = {
    val letters = s.filter(_.isLetter)
    if (letters.isEmpty) s
    else if (letters.forall(_.isUpper) || letters.forall(_.isLower)) tools.TextNormalization.sentenceCase(s)
    else recaseShoutedRuns(s) // partly-shouted → down-case the shouted run(s)
  }

  /** Display-casing for a MIXED-case segment: when a scraper SHOUTS part of an
   *  otherwise properly-cased title ("FEDERICO FELLINI: Ciao a tutti!"), down-case
   *  the shouted words while leaving the already-cased words byte-identical.
   *
   *  The trigger is a RUN of two or more *consecutive* all-caps words — that's
   *  what tells a shout ("FEDERICO FELLINI", "GWIEZDNE WOJNY: MANDALORIAN") apart
   *  from a lone acronym/initialism that must stay ("Liga Mistrzów UEFA",
   *  "NT Live"). Once a segment is found to be shouting, EVERY all-caps word in it
   *  is down-cased — including ones a lowercase connective stranded out of the run
   *  ("…MANDALORIAN i GROGU" → "…Mandalorian i Grogu", not a half-shouted
   *  "…Mandalorian i GROGU" that would also key as a brand-new spelling and
   *  churn the staging fold). Multi-letter roman numerals keep their caps
   *  ("BALBOA II" → "Balboa II"). */
  private def recaseShoutedRuns(s: String): String = {
    // Alternating whitespace / non-whitespace tokens, preserved exactly so an
    // untouched input round-trips byte-identical.
    val tokens    = "\\s+|\\S+".r.findAllIn(s).toVector
    val capsWords = tokens.indices.filter(i => isAllCapsWord(tokens(i)))
    // A shout = at least one ADJACENT pair of all-caps words. Tokens strictly
    // alternate whitespace/non-whitespace, so two consecutive caps words sit
    // exactly two indices apart (one whitespace token between them).
    val shouting  = capsWords.sliding(2).exists { case Seq(a, b) => b - a == 2; case _ => false }
    if (!shouting) s
    else tokens.zipWithIndex.map {
      case (t, i) if isAllCapsWord(t) && RomanNumeral.findFirstIn(t.filter(_.isLetter)).isEmpty =>
        tools.TextNormalization.titleCaseIfAllCaps(t)
      case (t, _) => t
    }.mkString
  }

  // Last-resort collapse for titles that share words + order but differ only
  // in punctuation/whitespace ("Top Gun Maverick" vs "Top Gun: Maverick").
  // Lowercased, accents stripped, every non-alphanumeric char dropped. Used
  // by `mergeKeyLookup` ONLY when at least two distinct corpus titles reduce
  // to the same form — so it never collapses a standalone film into siblings
  // that merely share a prefix.
  private[movies] def stripPunct(t: String): String = {
    val deburred = CombiningMarks.matcher(
      java.text.Normalizer.normalize(t, java.text.Normalizer.Form.NFD)
    ).replaceAll("").toLowerCase(Locale.ROOT)
    NonAlnumAscii.matcher(deburred).replaceAll("")
  }

  private[movies] def strippedKey(t: String): String =
    NonAlnumUnicode.matcher(
      tools.TextNormalization.deburr(normalize(t)).toLowerCase(Locale.ROOT)
    ).replaceAll("")

  // Deterministic preference ladder for same-identity title spellings. Pure
  // function of the string — the pick never depends on scrape/merge order.
  // Axes, best-first (the `-` makes "more is better" sort first under ascending
  // `sortBy`):
  //   1. richer punctuation — "Top Gun: Maverick" over "Top Gun Maverick"
  //   2. diacritics present — "Diabeł" over a scraper-flattened "Diabel"
  //   3. mixed case, not ALL-CAPS — "Top Gun" over "TOP GUN"
  //   4. least leading/trailing junk — "Werdykt" over "Werdykt." / "„Arco”"
  //   5. shorter — demoted below the quality axes so it can't strip the colon
  //   6. the string itself — total, order-independent final fallback
  private[movies] def displayLadderKey(c: String): (Int, Int, Int, Int, Int, String) = {
    // Strip leading/trailing non-alphanumerics so a stray trailing "." or
    // wrapping „quotes" count as junk (axis 4), NOT as richer interior
    // punctuation (axis 1) — otherwise "Werdykt." would outrank "Werdykt".
    val trimmed   = c.dropWhile(!_.isLetterOrDigit)
                     .reverse.dropWhile(!_.isLetterOrDigit).reverse
    val punct     = trimmed.count(ch => !ch.isLetterOrDigit && !ch.isWhitespace)
    val diacritic = if (c.exists(ch => ch.isLetter && ch.toInt > 127)) 1 else 0
    val mixedCase = if (c.exists(_.isUpper) && c.exists(_.isLower)) 1 else 0
    val junk      = c.length - trimmed.length
    (-punct, -diacritic, -mixedCase, junk, c.length, c)
  }

  /** Whether a title is clean enough to display verbatim. Used to gate the
   *  TMDB-Polish-title preference in `MovieRecord.displayTitle`: TMDB's
   *  crowd-sourced titles are usually the canonical form, but a minority are
   *  malformed — ALL-CAPS ("ALL YOU NEED IS KILL"), double-spaced ("Super
   *  Mario  Galaxy Film"), or carrying edge junk ("Zaproszenie."). When TMDB's
   *  title fails this check we fall back to the cinema spelling ladder, which
   *  has the well-formed form the cinemas advertise. */
  def wellFormedTitle(t: String): Boolean = {
    val letters       = t.filter(_.isLetter)
    val notAllCaps    = letters.isEmpty || letters.exists(_.isLower)
    val noDoubleSpace = !t.contains("  ")
    val noEdgeJunk    = t.headOption.exists(_.isLetterOrDigit) &&
                        t.lastOption.exists(_.isLetterOrDigit)
    notAllCaps && noDoubleSpace && noEdgeJunk
  }

  /** True when most of `s`'s letters are in the Latin Unicode script.
   *  Polish diacritics (`ł`, `ś`, `ą`, …) count as Latin; Cyrillic and CJK
   *  do not. Used to favour the Polish/Latin variant of a film over the
   *  Ukrainian/Cyrillic one, and to filter cross-script entries out of
   *  `cinemaTitles` so a single row never accumulates spellings in two
   *  scripts. */
  def isLatinDominant(s: String): Boolean = {
    val letters = s.filter(_.isLetter)
    if (letters.isEmpty) false
    else letters.count(c =>
      Character.UnicodeScript.of(c.toInt) == Character.UnicodeScript.LATIN
    ) * 2 >= letters.length
  }

  /** Two titles share a "primary script" when both are Latin-dominant or
   *  both are not. We treat scripts as a binary distinction (Latin /
   *  non-Latin) because the only cross-script collisions we actually see
   *  in cinema data are Polish-vs-Ukrainian — finer-grained script splits
   *  would just create unnecessary rows. */
  def sameScript(a: String, b: String): Boolean =
    isLatinDominant(a) == isLatinDominant(b)

}
