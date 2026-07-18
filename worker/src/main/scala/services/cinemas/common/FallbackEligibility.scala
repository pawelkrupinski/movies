package services.cinemas.common

import services.cinemas.pl.FilmwebShowtimesClient
/**
 * Decides which scrapers get a Filmweb fallback when their primary fetch throws
 * or comes back empty.
 *
 * A scraper qualifies when it's a non-chain venue whose primary feed ISN'T
 * already Filmweb:
 *   - Chains (Cinema City / Helios / Multikino, flagged via `CinemaScraper.chain`)
 *     are excluded — their own networks are the source of truth, are reliably
 *     fed, and Filmweb doesn't model them venue-for-venue.
 *   - A `FilmwebShowtimesClient` primary is excluded too — falling a Filmweb
 *     feed back to Filmweb is moot.
 *
 * Everything else — the ordinary single-venue HTML/JSON scrapers — is eligible.
 */
object FallbackEligibility {
  def eligible(s: CinemaScraper): Boolean =
    !s.chain && !s.isInstanceOf[FilmwebShowtimesClient]
}
