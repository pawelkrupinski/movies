package services.cinemas

import models.Cinema

/**
 * UK chain venues whose own-site scraper (Cineworld / Vue / Showcase / Everyman /
 * Odeon) is the catalogue primary, mapped to the flicks.co.uk slug they keep as
 * their aggregator FALLBACK — the mirror of the Polish own-site→Filmweb setup.
 *
 * `CinemaScraperCatalog.flicksFallbackSlugs` exposes this to `WorkerWiring`, which
 * builds a fallback `FlicksClient` on demand when a chain venue's own scrape has
 * been failing for the grace window.
 *
 * Populated alongside the chain-primary catalogue entries (each cinema listed here
 * MUST be catalogued under its chain client, not under `flicks(...)`, or it would
 * be both primary and fallback). Empty until the chain-wiring step fills it, so the
 * catalogue behaves exactly as the pre-chain flicks-primary one until then.
 */
object ChainFlicksFallback {
  val slugs: Map[Cinema, String] = Map.empty
}
