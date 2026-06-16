package tools

import java.text.Normalizer
import java.util.Locale
import org.jsoup.Jsoup

/**
 * Shared text-normalization primitives. CLAUDE.md threshold-2 extraction:
 * the same NFD-decompose + strip-diacritics + Polish-`ł`-fold chain was
 * duplicated in `TitleNormalizer`, `MetacriticClient`, `RottenTomatoesClient`,
 * `FilmwebClient`, and a few others. Centralised here so future edits to
 * the rule (e.g. additional digraph folds) propagate everywhere at once.
 */
object TextNormalization {

  /**
   * NFD-decompose `s`, drop combining marks, then fold Polish `ł`/`Ł` → `l`
   * (NFD doesn't decompose `ł` so a separate replace is required). Case is
   * preserved — callers that want lowercase append `.toLowerCase` themselves
   * so this helper is reusable in display-side paths too.
   */
  def deburr(s: String): String =
    Normalizer.normalize(s, Normalizer.Form.NFD)
      .replaceAll("\\p{M}", "")
      .replace('ł', 'l').replace('Ł', 'l')

  /**
   * Convert an ALL-CAPS string to Title Case (each word's first letter
   * uppercase, rest lowercase). Returns the input unchanged when any
   * lowercase letter is present — that signals the source already sent
   * proper casing and we shouldn't second-guess.
   *
   * Word boundary is "any non-letter starts a fresh word", so dotted
   * abbreviations (`C.J.`), apostrophes (`O'BRIEN` → `O'Brien`), and
   * hyphenated names (`MARY-ANN` → `Mary-Ann`) all do the right thing
   * without special-casing. Falls short on Gaelic / Mc-/Mac- (`MCNAMEE`
   * → `Mcnamee` not `McNamee`); acceptable trade-off vs. a custom rule
   * table for one prefix family.
   */
  def titleCaseIfAllCaps(s: String): String = {
    if (s.isEmpty || s.exists(c => c.isLetter && c.isLower)) return s
    val sb = new StringBuilder(s.length)
    var atWordStart = true
    var i = 0
    while (i < s.length) {
      val character = s.charAt(i)
      if (character.isLetter) {
        sb.append(if (atWordStart) character.toUpper else character.toLower)
        atWordStart = false
      } else {
        sb.append(character)
        atWordStart = true
      }
      i += 1
    }
    sb.toString
  }

  /** Title-case each whitespace-separated word — "science fiction" →
   *  "Science Fiction". Leaves already-mixed-case strings alone (so
   *  "Sci-Fi" stays "Sci-Fi"). Used to normalise Helios' all-lowercase
   *  genre labels at the write boundary. */
  def titleCaseIfAllLower(s: String): String = {
    if (s.isEmpty || s.exists(c => c.isLetter && c.isUpper)) return s
    titleCaseIfAllCaps(s.toUpperCase(Locale.ROOT))
  }

  /** Sentence-case `s`: lowercase everything, capitalise the first letter, and
   *  capitalise the letter after a ". " only when the preceding token looks like
   *  a sentence end — a digit, or a 4+ letter word. So "MAVKA. PRAWDZIWY MIT" →
   *  "Mavka. Prawdziwy mit" and "SKARPETEK 3. ALE KOSMOS" capitalises after the
   *  "3.", while Polish 2–3 letter abbreviations ("ANG. NAPISAMI", "REŻ. JANA")
   *  stay lower. Unconditional — callers gate on all-caps/all-lower themselves
   *  (see `TitleNormalizer.recase`). Shared by the central scraper casing stage
   *  (was duplicated in RialtoClient / MultikinoParser / ScraperParse). */
  def sentenceCase(s: String): String = {
    if (s.isEmpty) return s
    val chars = s.toLowerCase(Locale.ROOT).toCharArray
    chars(0) = chars(0).toUpper
    var i = 0
    while (i + 2 < chars.length) {
      if (chars(i) == '.' && chars(i + 1) == ' ' && precedingTokenEndsSentence(chars, i))
        chars(i + 2) = chars(i + 2).toUpper
      i += 1
    }
    new String(chars)
  }

  /** True if the character run ending at `dotIdx - 1` looks like a sentence-
   *  ending token: a digit (sequel/chapter number) or a 4+ letter word. */
  private def precedingTokenEndsSentence(chars: Array[Char], dotIdx: Int): Boolean = {
    if (dotIdx == 0) return false
    val prev = chars(dotIdx - 1)
    if (prev.isDigit) return true
    if (!prev.isLetter) return false
    var letters = 0
    var j = dotIdx - 1
    while (j >= 0 && chars(j).isLetter) { letters += 1; j -= 1 }
    letters >= 4
  }

  /**
   * Strip the trailing partial name from a likely-truncated cast string.
   *
   * Cinema City's filmDetails JSON hard-caps `cast` at ~232 chars of
   * content, severing the last name mid-word (`…, MAX HUANG, C.J. BLOOMFI`
   * — should be Bloomfield). When the string is at or above `threshold`,
   * assume the upstream cut it short and drop everything after the last
   * comma. Below the threshold the string is left alone (a genuinely
   * long-but-complete cast keeps its tail).
   */
  def stripHtml(s: String): String =
    Jsoup.parse(s).text().replaceAll("\\s+", " ").trim

  /**
   * Strip URL tokens out of free text. Cinema detail pages occasionally
   * inline a "watch the trailer" link inside the synopsis paragraph, so
   * Jsoup's `.text()` folds the bare URL into the blurb — e.g. Kino Muza's
   * "Orły Republiki" page yields
   * `"https://www.youtube.com/watch?v=… Nowy film laureata…"`. A raw URL
   * reads badly AND, being one unbreakable token, overflows the flex column
   * on the film-detail page and shoves the synopsis below the poster.
   *
   * Removes every `http(s)://…` / `www.…` run up to the next whitespace, then
   * tidies only the *horizontal* gap left behind. Paragraph breaks (`\n\n`,
   * which Kino Muza uses to join `<p>` blocks and the iOS/Android apps render
   * via `/api/details`) are deliberately preserved.
   */
  def stripUrls(s: String): String =
    s.replaceAll("(?i)(?:https?://|www\\.)\\S+", " ")
      .replaceAll("[ \\t\\x0B\\f\\r]+", " ")  // collapse horizontal runs, keep '\n'
      .replaceAll(" ?\\n ?", "\n")            // drop spaces hugging a newline
      .replaceAll("\\n{3,}", "\n\n")          // cap blank-line runs at one
      .trim

  def dropTrailingPartialNameIfLong(s: String, threshold: Int = 230): String =
    if (s.length < threshold) s
    else s.lastIndexOf(',') match {
      case -1  => s
      case index => s.substring(0, index).trim
    }
}
