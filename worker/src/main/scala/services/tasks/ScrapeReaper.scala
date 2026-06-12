package services.tasks

import play.api.Logging
import services.Stoppable
import services.cinemas.CinemaScraper
import services.freshness.{FreshnessKind, FreshnessStore}
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Try}

/**
 * Periodically enqueues a `ScrapeCinema` task for every cinema whose last
 * successful scrape is older than the 15-minute freshness window (or that has
 * never been scraped). Enqueue is deduped by the queue, so a cinema with a task
 * already waiting/working isn't queued twice; the handler re-checks freshness and
 * skips if a concurrent run already refreshed it.
 *
 * Instead of re-scraping every cinema back-to-back in a continuous loop, the
 * worker scrapes a cinema at most once per 15 minutes, and a failed scrape
 * (which doesn't mark freshness) is naturally retried on the next reaper tick.
 */
class ScrapeReaper(
  scrapers:  Seq[CinemaScraper],
  queue:     TaskQueue,
  freshness: FreshnessStore,
  interval:  FiniteDuration = 1.minute,
  // A small extra spacing before the (now post-hydrate) first tick, so it doesn't
  // land on the same instant as the cache hydrate finishing. Defaults to 0 so the
  // tests that drive `tick()` directly are unaffected.
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on how long the first tick waits for the freshness mirror to load its
  // scrape stamps. Past it we tick anyway — degrading to the old re-scrape-all
  // behaviour — rather than never scraping if a hydrate wedges.
  readyTimeout: FiniteDuration = 30.seconds
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("scrape-reaper")

  def start(): Unit = {
    if (scrapers.isEmpty) { logger.info("ScrapeReaper: no cinemas; not starting."); return }
    // Defer onto the scheduler thread so we can block it on the freshness hydrate
    // without holding up boot wiring; it then schedules the periodic ticks.
    scheduler.execute(() => Try(awaitReadyThenStart()))
    logger.info(s"ScrapeReaper started over ${scrapers.size} cinemas, first tick after freshness hydrate then ${initialDelay.toSeconds}s, every ${interval.toSeconds}s.")
  }

  // Block until the scrape freshness stamps are loaded (capped at `readyTimeout`),
  // THEN begin the periodic ticks. Without this the first tick can read a
  // not-yet-hydrated mirror, see every cinema as stale, and enqueue all ~300 at
  // once — the boot storm that drained the shared-CPU credit balance.
  private def awaitReadyThenStart(): Unit = {
    Try(Await.ready(freshness.whenReady(FreshnessKind.CinemaScrape), readyTimeout)) match {
      case Failure(_) =>
        logger.warn(s"ScrapeReaper: freshness stamps not ready after ${readyTimeout.toSeconds}s; first tick may re-scrape every cinema.")
      case _ => ()
    }
    scheduler.scheduleWithFixedDelay(() => Try(tick()), initialDelay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
  }

  /** Enqueue every stale cinema. Package-private so tests can drive it directly. */
  private[tasks] def tick(): Int = {
    var enqueued = 0
    scrapers.foreach { s =>
      val key = ScrapeCinemaHandler.dedupKey(s.cinema)
      if (!freshness.isFresh(key, FreshnessKind.CinemaScrape)) {
        if (queue.enqueue(TaskType.ScrapeCinema, key,
              Map(ScrapeCinemaHandler.CinemaKey -> s.cinema.displayName)) == EnqueueResult.Added)
          enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"ScrapeReaper enqueued $enqueued stale cinema(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
