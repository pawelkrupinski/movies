package services.tasks

import models.Cinema
import play.api.Logging
import services.cinemas.{CinemaScrapeRunner, CinemaScraper, ScrapeErrors}
import services.freshness.{Freshness, FreshnessKind, FreshnessStore}

import java.time.Clock

/**
 * Handles a `ScrapeCinema` task: scrape one cinema, unless it is no longer due
 * under the shared [[DueWindow]] (then skip). The reaper enqueues a cinema when
 * the window says it's due; this re-check uses the SAME `DueWindow`, so it skips
 * only a duplicate/stale task whose cinema was already scraped this window — never
 * a still-due one (per the queue's "the consumer decides redundancy" contract).
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
  freshness:     FreshnessStore,
  dueWindow:     DueWindow = new DueWindow(Freshness.defaultScrapeTtl),
  clock:         Clock = Clock.systemUTC(),
  chunkPlanner:  Option[ChunkScrapePlanner] = None
) extends TaskHandler with Logging {
  import ScrapeCinemaHandler._
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ScrapeCinema

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!dueWindow.isDue(key, freshness.lastFetchedAt(key), clock.instant())) return Skipped

    val cinemaName = task.payload.getOrElse(CinemaKey, "")
    chunkPlanner.filter(_.isChunked(cinemaName)) match {
      case Some(planner) =>
        // Chunked cinema: fan out into ScrapeChunk tasks (or no-op if a run is
        // already active). Freshness is marked by the terminal reduce, not here —
        // the run doc is the per-cinema mutex, so a duplicate ScrapeCinema while a
        // run is in flight just no-ops. A plan throw can't escape (it records the
        // outcome itself), but guard anyway so the queue never reschedules.
        try { val _ = planner.plan(cinemaName) }
        catch { case e: Exception => logger.error(s"chunked plan for $cinemaName threw", e) }
        return Done
      case None =>
    }

    scrapersByKey.get(cinemaName) match {
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
