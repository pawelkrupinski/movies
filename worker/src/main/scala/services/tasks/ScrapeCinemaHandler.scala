package services.tasks

import play.api.Logging
import services.freshness.{Freshness, FreshnessStore}
import models.Cinema
import services.cinemas.common.{CinemaScrapeRunner, CinemaScraper, ScrapeErrors}
import services.scrapes.{GoneUpstream, ScrapeArchiveRepository}

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
 * for the first few attempts the cinema is left un-stamped, so the reaper
 * re-enqueues it on its next tick — retries happen at the reaper's cadence
 * (minutes), not the worker's tight poll loop. Past that budget
 * [[ScrapeFreshnessPolicy]] parks it on the normal window so a permanently broken
 * venue can't camp at the head of every tick and starve healthy cinemas.
 */
class ScrapeCinemaHandler(
  scrapersByKey: Map[String, CinemaScraper],
  runner:        CinemaScrapeRunner,
  freshness:     FreshnessStore,
  dueWindow:     DueWindow = new DueWindow(Freshness.defaultScrapeTtl),
  clock:         Clock = Clock.systemUTC(),
  chunkPlanner:  Option[ChunkScrapePlanner] = None,
  // Production passes the SHARED policy, so a venue's failure streak is counted once
  // however it happens to be scraped. None → a private one over this handler's own
  // store/clock, which is the same rule, just not shared with the chunked path.
  scrapeFreshness: Option[ScrapeFreshnessPolicy] = None,
  // Read to tell a venue that is BROKEN from one that is GONE — a page 404ing for
  // over a day. Defaults to the no-op archive, under which nothing is ever gone
  // and this handler behaves exactly as it did before.
  scrapeArchive: ScrapeArchiveRepository = ScrapeArchiveRepository.empty
) extends TaskHandler with Logging {
  import ScrapeCinemaHandler._
  import HandlerOutcome._

  private val outcome = scrapeFreshness.getOrElse(new ScrapeFreshnessPolicy(freshness, clock = clock))

  override val taskType: TaskType = TaskType.ScrapeCinema

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!dueWindow.isDue(key, freshness.lastFetchedAt(key), clock.instant())) return Skipped

    val cinemaName = task.payload.getOrElse(CinemaKey, "")
    chunkPlanner.filter(_.isChunked(cinemaName)) match {
      case Some(planner) =>
        // Chunked cinema: fan out into ScrapeChunk tasks (or no-op if a run is
        // already active). Freshness is marked by whichever step TERMINATES the
        // scrape — the reduce on success, the planner itself on an empty or failed
        // plan — not here. The run doc is the per-cinema mutex, so a duplicate
        // ScrapeCinema while a run is in flight just no-ops. A plan throw can't
        // escape (it records the outcome itself), but guard anyway so the queue
        // never reschedules.
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
        // A venue whose page has 404'd for over a day is re-probed once a day, not
        // once a window: nothing about the roster can bring it back (both
        // aggregators keep dead venues in the sitemaps we harvest from), so every
        // attempt in between is a request that cannot succeed. Stamped as it skips
        // — an unstamped cinema is what makes the reaper re-enqueue it every tick
        // and jump the queue ahead of healthy ones.
        val now = clock.instant()
        if (scrapeArchive.find(cinema).exists(row => GoneUpstream.skipScrape(row, now))) {
          logger.info(s"Skipping ${cinema.displayName}: its page has been 404ing for over " +
            s"${GoneUpstream.MinimumAge.toHours}h; next probe in ${GoneUpstream.RecheckInterval.toHours}h.")
          outcome.skipped(key)
          return Done
        }
        val t0     = System.currentTimeMillis()
        try {
          runner.run(scraper)
          outcome.succeeded(key)
          Done
        } catch {
          case e: Exception =>
            val elapsed = System.currentTimeMillis() - t0
            if (ScrapeErrors.isTransientHttpError(e))
              logger.warn(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms: ${e.getMessage}")
            else
              logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
            // Left stale → the reaper re-enqueues on its next tick, until the retry
            // budget is spent and the policy parks it on the normal window instead.
            outcome.failed(key)
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
