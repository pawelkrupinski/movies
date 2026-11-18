package services.tasks

import services.cinemas.common.{CinemaScrapeRunner, CinemaScraper}

import java.time.Clock

/**
 * One unchunked cinema's scrape, run through `CinemaScrapeRunner` (which also enqueues any deferred
 * per-film detail tasks), and its outcome recorded: stamped with the runway its listing leaves and its
 * cost on success; on failure left un-stamped for the reaper to retry at its cadence, until
 * [[ScrapeFreshnessPolicy]] parks it.
 *
 * @param costs where a plain scrape records its cost (one task) for the cost-spaced schedule; a chunked
 *              venue's is recorded by the planner, which knows its fan-out.
 */
final class PlainCinemaScrape(
  runner:  CinemaScrapeRunner,
  outcome: ScrapeFreshnessPolicy,
  pages:   VenuePageHistory,
  costs:   ScrapeCostStore,
  clock:   Clock
) {

  /** Scrape `scraper`'s venue under the scrape key `key`. A failure is reported and recorded, never raised. */
  def run(key: String, scraper: CinemaScraper): Unit = {
    val startedAt = clock.instant()
    val t0        = tools.Stopwatch.start()
    try {
      outcome.scraped(key, scraper.cinema, runner.run(scraper))
      costs.recordScrape(scraper.cinema, tasks = 1)
    } catch {
      case e: Exception =>
        pages.reportScrapeFailure(scraper.cinema, e, startedAt, t0.millis)
        // Left stale → the reaper re-enqueues on its next tick, until the retry
        // budget is spent and the policy parks it on the normal window instead.
        outcome.failed(key, Some(e))
    }
  }
}
