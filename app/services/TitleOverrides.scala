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
  // (normalizedTitle, year) → IMDb id. Year-specific entries are checked
  // first; an `(title, None)` entry serves as a fallback that matches the
  // title regardless of the year the cinema reports. Use the year-less form
  // for films whose cinema-side year is inconsistent (different cinemas
  // report different years, or it flips between None/2025/2026 across
  // schedule refreshes) — the IMDb id alone is enough to pin the film.
  //
  // Wspinaczka (2025): TMDB has no Polish title for tt36437006 / Girl Climber,
  // so the Polish search returns Skyscraper Live / Free Solo / Dawn Wall /
  // L'Ascension — none of them the right film.
  //
  // On drive: the cinema lists the Ukrainian war drama "На драйві"
  // (tt38780559, TMDB 1566982, 2026-04-09 release) under the literal English
  // phrase "On drive". TMDB's title search with that query lands on the
  // unrelated LEGO F1 doc "Bricks on Track: Building the LEGO F1 Drivers'
  // Parade" (tt39074205) because its TMDB title contains "On Drive"; with
  // year=None it lands on the 1992 TV movie "Doing Time on Maple Drive"
  // (tt0104118). Year-less override pins the right film regardless.
  private val overrides: Map[(String, Option[Int]), String] = Map(
    ("wspinaczka", Some(2025)) -> "tt36437006",
    ("on drive",   None)       -> "tt38780559"
  )

  def lookup(title: String, year: Option[Int]): Option[String] = {
    val n = EnrichmentService.normalize(title)
    // Year-specific match first (more precise), then year-agnostic fallback.
    overrides.get((n, year)).orElse(overrides.get((n, None)))
  }
}
