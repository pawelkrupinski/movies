package tools

import java.text.Normalizer

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
      val c = s.charAt(i)
      if (c.isLetter) {
        sb.append(if (atWordStart) c.toUpper else c.toLower)
        atWordStart = false
      } else {
        sb.append(c)
        atWordStart = true
      }
      i += 1
    }
    sb.toString
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
  def dropTrailingPartialNameIfLong(s: String, threshold: Int = 230): String =
    if (s.length < threshold) s
    else s.lastIndexOf(',') match {
      case -1  => s
      case idx => s.substring(0, idx).trim
    }
}
