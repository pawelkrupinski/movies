package services.tasks

import play.api.Logging
import services.Stoppable
import services.cinemas.CinemaScraper
import services.freshness.{FreshnessKind, FreshnessStore}
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically enqueues a `ScrapeCinema` task for every cinema whose last
 * successful scrape is older than the 15-minute freshness window (or that has
 * never been scraped). Enqueue is deduped by the queue, so a cinema with a task
 * already waiting/working isn't queued twice; the handler re-checks freshness and
 * skips if a concurrent run already refreshed it.
 *
 * This replaces ShowtimeCache's continuous loop: instead of re-scraping every
 * cinema back-to-back, the worker scrapes a cinema at most once per 15 minutes,
 * and a failed scrape (which doesn't mark freshness) is naturally retried on the
 * next reaper tick.
 */
class ScrapeReaper(
  scrapers:  Seq[CinemaScraper],
  queue:     TaskQueue,
  freshness: FreshnessStore,
  interval:  FiniteDuration = 1.minute,
  // Hold the first tick back from boot: on a cold worker every cinema is stale,
  // so the first tick enqueues all ~300 at once and the TaskWorker drains them
  // immediately — the queue-mode equivalent of ShowtimeCache's boot scrape burst.
  // Firing that on a cold JVM, on top of the cache hydrate, is what drained the
  // shared-CPU credit balance. Wiring injects a boot delay; defaults to 0 so the
  // tests that drive `tick()` directly are unaffected.
  initialDelay: FiniteDuration = 0.seconds
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("scrape-reaper")

  def start(): Unit = {
    if (scrapers.isEmpty) { logger.info("ScrapeReaper: no cinemas; not starting."); return }
    scheduler.scheduleWithFixedDelay(() => Try(tick()), initialDelay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"ScrapeReaper started over ${scrapers.size} cinemas, first tick in ${initialDelay.toSeconds}s, then every ${interval.toSeconds}s.")
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
