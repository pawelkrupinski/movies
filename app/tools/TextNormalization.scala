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
}
