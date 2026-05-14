package services

import services.enrichment.EnrichmentService

/**
 * Manual `(title, year) → IMDb id` overrides for films that TMDB's Polish
 * search can't resolve. TMDB's translations are community-maintained, so
 * niche films (festival docs, small releases) often lack a Polish title
 * entirely — searching for the Polish exhibitor's title returns either
 * nothing or an unrelated film. Each entry pins the correct IMDb id; the
 * enrichment pipeline then fetches the matching TMDB record via /find and
 * proceeds with the normal Filmweb / IMDb chain.
 *
 * Keys are matched against `EnrichmentService.normalize` (lower-case +
 * diacritic-stripped), so cinema casing differences don't matter. Year is
 * matched verbatim against what the cinema reports (usually the local release
 * year, which can differ from TMDB's production year).
 */
object TitleOverrides {
  // (normalizedTitle, year) → IMDb id.
  // Wspinaczka (2025): TMDB has no Polish title for tt36437006 / Girl Climber,
  // so the Polish search returns Skyscraper Live / Free Solo / Dawn Wall /
  // L'Ascension — none of them the right film.
  private val overrides: Map[(String, Int), String] = Map(
    ("wspinaczka", 2025) -> "tt36437006"
  )

  def lookup(title: String, year: Option[Int]): Option[String] =
    year.flatMap(y => overrides.get(EnrichmentService.normalize(title) -> y))
}
