package services.tasks

import models.MovieRecord
import play.api.Logging
import services.Stoppable
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.MovieCache
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.util.Try

/**
 * Periodically enqueues rating-refresh tasks for stale rows — the queue-based
 * replacement for the `*Ratings` classes' 4h cache walks. Each source sweeps on
 * its own 4h schedule, staggered an hour apart exactly as the old walks were
 * (IMDb +1h, RT +2h, Metacritic +3h, Filmweb +4h), so only one source walks per
 * hour. Enqueue is deduped and freshness-gated, so a row refreshed inside the
 * window isn't re-queued — the same 4h cadence, now shared across servers.
 *
 * A sweep only enqueues rows the source can actually act on (IMDb needs an
 * imdbId; the others need a resolved tmdbId), matching the old walks' scope.
 */
class EnrichmentReaper(cache: MovieCache, queue: TaskQueue, freshness: FreshnessStore) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("enrichment-reaper")

  private case class Sweep(taskType: TaskType, kind: FreshnessKind, eligible: MovieRecord => Boolean, offsetHours: Long)

  private val sweeps = Seq(
    Sweep(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined, 1L),
    Sweep(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined, 2L),
    Sweep(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined, 3L),
    Sweep(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, _.tmdbId.isDefined, 4L)
  )

  def start(): Unit = {
    sweeps.foreach { s =>
      scheduler.scheduleAtFixedRate(
        () => Try(sweep(s)), s.offsetHours * 3600, 4 * 3600, TimeUnit.SECONDS)
    }
    logger.info(s"EnrichmentReaper started: ${sweeps.size} rating sweeps every 4h, staggered hourly.")
  }

  /** Run every source's sweep once. Package-private so tests can drive the
   *  reaper synchronously. Returns total tasks enqueued. */
  private[tasks] def sweepOnce(): Int = sweeps.map(sweep).sum

  /** Enqueue every eligible, non-fresh row for one source. Returns how many
   *  tasks were enqueued. */
  private def sweep(s: Sweep): Int = {
    var enqueued = 0
    cache.entries.foreach { case (key, record) =>
      if (s.eligible(record)) {
        val dk = RatingTasks.dedupKey(s.kind, key)
        if (!freshness.isFresh(dk, s.kind) &&
            queue.enqueue(s.taskType, dk, RatingTasks.payload(key)) == EnqueueResult.Added)
          enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"EnrichmentReaper enqueued $enqueued stale ${s.kind.label} row(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
