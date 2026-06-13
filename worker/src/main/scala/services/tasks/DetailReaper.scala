package services.tasks

import models.MovieRecord
import play.api.Logging
import services.Stoppable
import services.cinemas.DetailEnricher
import services.events.{EventBus, MovieDetailsComplete}
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCache}
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.Clock
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
 * It is ALSO the backstop for the `detailPending` gate: a film a deferred cinema
 * scrapes is held back (`detailPending`, out of the read model + the TMDB stage)
 * until its detail lands and `EnrichDetailsHandler` publishes
 * `MovieDetailsComplete`. If that detail can NEVER complete — the page is gone,
 * the row has no deferred slot/`filmUrl` anymore, or the completion event was
 * lost across a restart while the detail is already fresh — the row would
 * otherwise stay invisible forever (the daily TMDB sweep deliberately skips
 * `detailPending` rows). `reapStuckPending` releases any `detailPending` row with
 * no outstanding (enqueueable, not-yet-fresh) detail: it clears the flag and
 * publishes `MovieDetailsComplete`, so the row finally resolves.
 *
 * Walks the cache like `EnrichmentReaper`: for each row carrying a slot for a
 * deferred cinema with a `filmUrl`, enqueue (deduped + freshness-gated, so a
 * film already fresh — or already waiting/working — isn't re-queued).
 */
class DetailReaper(
  enrichers: Seq[DetailEnricher],
  cache:     MovieCache,
  queue:     TaskQueue,
  freshness: FreshnessStore,
  bus:       EventBus,
  interval:  FiniteDuration = 15.minutes,
  runStore:  ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:     Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("detail-reaper")

  def start(): Unit = {
    if (enrichers.isEmpty) { logger.info("DetailReaper: no deferred cinemas; not starting."); return }
    scheduler.scheduleWithFixedDelay(() => Try(tickIfClaimed()), interval.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"DetailReaper started over ${enrichers.size} deferred cinema(s), every ${interval.toMinutes}min.")
  }

  /** Run the detail tick + stuck-pending release only if this machine wins the
   *  current window's occurrence claim — otherwise another machine is handling
   *  this window, so skip. Returns the number of detail tasks enqueued (0 when
   *  the claim was lost). Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("detail", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { val n = tick(); reapStuckPending(); n } else 0
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

  /** Release any `detailPending` row that has no outstanding detail to fetch —
   *  its detail is already fresh (the completion event was lost) or it has no
   *  deferred slot/`filmUrl` to enrich at all (orphaned flag). Clears the flag
   *  and re-triggers TMDB via `MovieDetailsComplete`, so the row stops being held
   *  out of the read model. Returns how many were released. Scheduled (not run by
   *  the fixture harness's `enrichDetailsSync`, which only calls `tick`). */
  def reapStuckPending(): Int = {
    var released = 0
    cache.entries.foreach { case (key, record) =>
      if (record.detailPending && !detailOutstanding(key, record)) {
        cache.putIfPresent(key, _.copy(detailPending = false))
        val row = cache.get(key)
        bus.publish(MovieDetailsComplete(
          key.cleanTitle, key.year,
          row.flatMap(_.cinemaOriginalTitle),
          row.map(_.director).filter(_.nonEmpty).map(_.mkString(", "))))
        released += 1
      }
    }
    if (released > 0) logger.info(s"DetailReaper released $released detail-pending row(s) with no outstanding detail.")
    released
  }

  /** True when a deferred cinema still owes this row a detail fetch — it has a
   *  `filmUrl` slot whose detail isn't fresh yet. While true the row legitimately
   *  stays `detailPending` (and `tick` keeps the fetch enqueued). */
  private def detailOutstanding(key: CacheKey, record: MovieRecord): Boolean =
    enrichers.exists { e =>
      record.data.get(e.cinema).flatMap(_.filmUrl).isDefined &&
        !freshness.isFresh(EnrichDetailsTasks.dedupKey(e.detailGroup, key), FreshnessKind.DetailEnrich)
    }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
