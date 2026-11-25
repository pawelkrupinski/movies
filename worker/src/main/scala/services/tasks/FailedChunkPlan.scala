package services.tasks

import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, PreScrapedCinemaScraper}

/**
 * A chunked plan whose nav fetch threw: published at once as the scrape's outcome through `publishScrape`
 * (uptime red + Filmweb fallback for eligible venues) rather than starting an empty run, and recorded
 * against the cinema's due schedule ITSELF via [[ScrapeFreshnessPolicy]], as it bypasses the terminal reduce.
 */
final class FailedChunkPlan(publishScrape: CinemaScraper => Unit, scrapeFreshness: ScrapeFreshnessPolicy, pages: VenuePageHistory) {

  /** `scraper`'s plan threw `e`. */
  def publish(scraper: ChunkedCinemaScraper, e: Exception): Unit = {
    pages.reportPlanFailure(scraper.cinema, e)
    // The recording wrapper re-raises the failure — unless the venue's fallback
    // served it instead, which is the scrape succeeding, as on the plain path. Kept
    // due, it would be re-run on the next ticks and re-walk the fallback each time.
    val fallbackServed = scala.util.Try(publishScrape(PreScrapedCinemaScraper.of(scraper, () => throw e))).isSuccess
    val key            = ScrapeFreshnessPolicy.scrapeKey(scraper.cinema)
    if (fallbackServed) scrapeFreshness.succeeded(key) else scrapeFreshness.failed(key, Some(e))
  }
}
