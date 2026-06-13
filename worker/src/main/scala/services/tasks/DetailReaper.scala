package services.tasks

import play.api.Logging
import services.Stoppable
import services.cinemas.DetailEnricher
import services.freshness.FreshnessStore
import services.movies.MovieCacheReader
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically re-enqueues `EnrichDetails` tasks for every deferred cinema's
 * current films whose detail is stale — the refresh/retry backstop the
 * event-driven [[DetailTaskEnqueuer]] can't provide. `CinemaMovieAdded` fires
 * only on a film's first appearance, so without this a detail fetch that failed
 * the first time would never retry, and a cinema whose showtime-bearing fetch is
 * itself deferred (Rialto) would never refresh its showtimes after the first
 * fetch. This is the detail-side analogue of [[ScrapeReaper]] /
 * [[EnrichmentReaper]].
 *
 * Walks the cache like `EnrichmentReaper`: for each row carrying a slot for a
 * deferred cinema with a `filmUrl`, enqueue (deduped + freshness-gated, so a
 * film already fresh — or already waiting/working — isn't re-queued).
 */
class DetailReaper(
  enrichers: Seq[DetailEnricher],
  cache:     MovieCacheReader,
  queue:     TaskQueue,
  freshness: FreshnessStore,
  interval:  FiniteDuration = 15.minutes
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("detail-reaper")

  def start(): Unit = {
    if (enrichers.isEmpty) { logger.info("DetailReaper: no deferred cinemas; not starting."); return }
    scheduler.scheduleWithFixedDelay(() => Try(tick()), interval.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"DetailReaper started over ${enrichers.size} deferred cinema(s), every ${interval.toMinutes}min.")
  }

  /** Enqueue every stale `(deferred-cinema, film)` detail, keyed off the row's
   *  CURRENT CacheKey (so it's robust to a row that was re-keyed since its
   *  `CinemaMovieAdded` fired). Public so tests / the fixture harness can drive
   *  one pass directly. Returns how many tasks were enqueued. */
  def tick(): Int = {
    var enqueued = 0
    cache.entries.foreach { case (key, record) =>
      enrichers.foreach { e =>
        record.data.get(e.cinema).flatMap(_.filmUrl).foreach { ref =>
          if (EnrichDetailsTasks.enqueueIfStale(queue, freshness, e, key, ref)) enqueued += 1
        }
      }
    }
    if (enqueued > 0) logger.info(s"DetailReaper enqueued $enqueued stale detail(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
