package services.tasks

import models.Cinema
import play.api.Logging
import services.cinemas.{CinemaScrapeRunner, CinemaScraper, ScrapeErrors}
import services.freshness.{FreshnessKind, FreshnessStore}

/**
 * Handles a `ScrapeCinema` task: scrape one cinema, unless it was scraped within
 * the freshness window (then skip). The reaper enqueues by freshness; this
 * re-check guards against a duplicate task or a stale enqueue (per the queue's
 * "the consumer decides redundancy" contract).
 *
 * The scrape itself runs through `CinemaScrapeRunner`, which also enqueues any
 * deferred per-film detail tasks — so detail enrichment happens the same way
 * regardless of whether scraping is queue-driven or the legacy loop.
 *
 * A scrape failure is swallowed and reported as `Done` rather than `Reschedule`:
 * because we don't mark the cinema fresh on failure, the reaper re-enqueues it on
 * its next tick — so retries happen at the reaper's cadence (minutes), not the
 * worker's tight poll loop. This matches the old continuous-loop behaviour, where
 * a failed scrape simply waited for the next pass.
 */
class ScrapeCinemaHandler(
  scrapersByKey: Map[String, CinemaScraper],
  runner:        CinemaScrapeRunner,
  freshness:     FreshnessStore
) extends TaskHandler with Logging {
  import ScrapeCinemaHandler._
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ScrapeCinema

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (freshness.isFresh(key, FreshnessKind.CinemaScrape)) return Skipped

    scrapersByKey.get(task.payload.getOrElse(CinemaKey, "")) match {
      case None =>
        // Cinema no longer in the catalogue (deploy changed it) — drop the task.
        logger.warn(s"No scraper for task $key; dropping.")
        Done
      case Some(scraper) =>
        val cinema = scraper.cinema
        val t0     = System.currentTimeMillis()
        try {
          runner.run(scraper)
          freshness.markFresh(key, FreshnessKind.CinemaScrape)
          Done
        } catch {
          case e: Exception =>
            val elapsed = System.currentTimeMillis() - t0
            if (ScrapeErrors.isTransientHttpError(e))
              logger.warn(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms: ${e.getMessage}")
            else
              logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
            // Not marked fresh → the reaper re-enqueues on its next tick.
            Done
        }
    }
  }
}

object ScrapeCinemaHandler {
  /** Payload field carrying the cinema's display name (the scraper-map key). */
  val CinemaKey = "cinema"

  /** The dedup + freshness key for a cinema's scrape task. The same string keys
   *  the queue (so duplicate scrape tasks collapse) and the freshness store. */
  def dedupKey(cinema: Cinema): String = s"scrape|${cinema.displayName}"

  /** The scraper-map key the handler looks a task up by. */
  def scraperKey(cinema: Cinema): String = cinema.displayName
}
