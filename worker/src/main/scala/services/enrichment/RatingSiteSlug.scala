package services.enrichment

import tools.TextNormalization

/**
 * The shared fold `RottenTomatoesClient.slugify`/`MetacriticClient.slugify`
 * use to build a rating-site URL slug: deburr → lowercase → drop apostrophes
 * (so "Schindler's List" fuses to "schindlers-list", not "schindler-s-list")
 * → collapse every other run of non-alphanumeric characters — except
 * anything in `preserve` — into one `separator` → trim leading/trailing
 * separators.
 *
 * Deliberately its own object rather than an addition to `tools.Slugify`:
 * that one romanizes Cyrillic and collapses an apostrophe like any other
 * punctuation — a fold two OTHER callers (`TitleRuleKey`'s frozen key space,
 * `FilmHref`'s permalinks) depend on staying exactly as it is. This is the
 * two rating sites' own, narrower fold — same mechanism, different separator
 * and preserved characters — kept apart so neither drifts the other's
 * behavior by sharing a mechanism that was never actually shared with them.
 *
 * `separator` and `preserve` are meant for the small, fixed set of literal
 * characters each site's own slug policy needs (RT: `_`; MC: `-`, preserving
 * `!`) — not arbitrary regex syntax.
 */
object RatingSiteSlug {
  def apply(title: String, separator: Char, preserve: String = ""): String = {
    val stripped = TextNormalization.deburr(title).toLowerCase.replaceAll("[']", "")
    val sep      = separator.toString
    stripped.replaceAll(s"[^a-z0-9$preserve]+", sep).replaceAll(s"^\\Q$sep\\E+|\\Q$sep\\E+$$", "")
  }
}
