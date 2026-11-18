package services.tasks

import services.cinemas.common.{ChunkPlan, ChunkedCinemaScraper, CinemaScraper, ListingStructure, PreScrapedCinemaScraper}

/**
 * A chunked plan that found no days in the horizon: a clean parse of an empty repertoire, published at
 * once as the scrape's outcome through `publishScrape` (uptime white + Filmweb fallback for eligible venues)
 * rather than starting an empty run.
 *
 * It bypasses the terminal reduce, so it records the outcome against the cinema's due schedule ITSELF via
 * [[ScrapeFreshnessPolicy]] — an empty plan is a successful scrape of an empty repertoire and stamps
 * freshness like any other. Omitting that was the German outage: a venue advertising zero showtime days
 * never got stamped, so it read as never-fetched, sorted to the front of every [[ScrapeReaper]] tick, and
 * ~40 of them ate the entire per-tick budget for days while ~1000 working cinemas were never enqueued.
 */
final class EmptyChunkPlan(publishScrape: CinemaScraper => Unit, scrapeFreshness: ScrapeFreshnessPolicy) {

  /** Publish `scraper`'s empty `plan`, read whole when `complete`. It counts as a SUCCESSFUL scrape and
   *  advances the due schedule. (Uptime still records it white — empty is visible, just not overdue.)
   *  Whether the page SAID it has nothing on travels with it to the archive (`emptyListing`). */
  def publish(scraper: ChunkedCinemaScraper, plan: ChunkPlan, complete: Boolean): Unit = {
    try publishScrape(PreScrapedCinemaScraper.of(scraper, () => Seq.empty, ListingStructure.wholeWhen(complete), plan.emptyListing))
    catch { case _: Exception => () }
    // Deliberately NOT fed to VenueScrapeCadence: that mechanism is for a venue
    // with SOME showtimes about to run dry, not one advertising nothing at all.
    // A seasonally-closed venue (a drive-in shut all winter, a site between
    // programmes) can sit here for weeks; the MinInterval floor would hammer its
    // site every 30 minutes for as long as that lasts, for no benefit — the country
    // default already re-checks it often enough to notice when it reopens.
    scrapeFreshness.succeeded(ScrapeFreshnessPolicy.scrapeKey(scraper.cinema))
  }
}
