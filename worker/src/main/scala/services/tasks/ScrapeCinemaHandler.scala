package services.tasks

import java.time.Clock

import models.Cinema
import play.api.Logging
import services.cinemas.common.CinemaScraper
import services.tasktypes.TaskType

/**
 * Handles a `ScrapeCinema` task: scrape one cinema, unless it is no longer due
 * under the shared [[DueWindow]] (then skip). The reaper enqueues a cinema when
 * the window says it's due; this re-check uses the SAME `DueWindow`, so it skips
 * only a duplicate/stale task whose cinema was already scraped this window — never
 * a still-due one (per the queue's "the consumer decides redundancy" contract).
 *
 * The scrape itself is [[PlainCinemaScrape]]'s, run through `CinemaScrapeRunner`, which also enqueues any
 * deferred per-film detail tasks — so detail enrichment happens the same way
 * regardless of whether scraping is queue-driven or the legacy loop.
 *
 * A scrape failure is swallowed and reported as `Done` rather than `Reschedule`:
 * for the first few attempts the cinema is left un-stamped, so the reaper
 * re-enqueues it on its next tick — retries happen at the reaper's cadence
 * (minutes), not the worker's tight poll loop. Past that budget
 * [[ScrapeFreshnessPolicy]] parks it on the normal window so a permanently broken
 * venue can't camp at the head of every tick and starve healthy cinemas.
 */
class ScrapeCinemaHandler(
  scrapersByKey: Map[String, CinemaScraper],
  // Re-checks the SAME due schedule the reaper enqueued by.
  due:           DueScrapes,
  plain:         PlainCinemaScrape,
  chunkPlanner:  Option[ChunkScrapePlanner],
  // Says whether a venue's page is GONE (404ing for over a day) rather than broken.
  pages:         GonePageScrapePolicy,
  // The SHARED policy, so a venue's failure streak is counted once however it happens to be scraped.
  outcome:       ScrapeFreshnessPolicy,
  clock:         Clock
) extends TaskHandler with Logging {
  import ScrapeCinemaHandler._
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ScrapeCinema

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!due.isDue(key, clock.instant())) return Skipped

    val cinemaName = task.payload.getOrElse(CinemaKey, "")
    val chunked    = chunkPlanner.filter(_.scraper(cinemaName).isDefined)
    chunkPlanner.flatMap(_.scraper(cinemaName)).orElse(scrapersByKey.get(cinemaName)) match {
      case None =>
        // Cinema no longer in the catalogue (deploy changed it) — drop the task.
        logger.warn(s"No scraper for task $key; dropping.")
      // A gone venue is stamped as it skips — an unstamped cinema is what makes the reaper
      // re-enqueue it every tick and jump the queue ahead of healthy ones. Checked before the
      // chunked branch: every Filmstarts venue is chunked, and planning first kept 28 gone
      // German pages probed ~7 times a day (2026-10).
      case Some(scraper) if pages.skipsScrape(scraper) => outcome.skipped(key)
      // Chunked cinema: fan out into ScrapeChunk tasks (or no-op if a run is already active).
      // Freshness is marked by whichever step TERMINATES the scrape — the reduce on success, the
      // planner itself on an empty or failed plan — not here. The run doc is the per-cinema
      // mutex, so a duplicate ScrapeCinema while a run is in flight just no-ops.
      case Some(_) if chunked.isDefined => chunked.foreach(_.planLogged(cinemaName))
      case Some(scraper) => plain.run(key, scraper)
    }
    Done
  }
}

object ScrapeCinemaHandler {
  /** Payload field carrying the cinema's display name (the scraper-map key). */
  val CinemaKey = "cinema"

  /** The scraper-map key the handler looks a task up by. */
  def scraperKey(cinema: Cinema): String = cinema.displayName
}
