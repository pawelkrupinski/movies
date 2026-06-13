package services.tasks

import models.MovieRecord
import play.api.Logging
import services.Stoppable
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.MovieCacheReader
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
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
 *
 * On `start()` a one-shot boot sweep runs all sources promptly (after
 * `bootSweepDelaySeconds`), so rows hydrated from Mongo but never enriched get
 * queued without waiting for the staggered first periodic fire (IMDb +1h …
 * Filmweb +4h). That uptime-gated first fire is never reached on a worker that
 * redeploys/restarts more often than once an hour — exactly the production
 * condition that left ~31% of imdbId-bearing rows without an IMDb rating: the
 * in-memory scheduler's clock resets every restart, so the +1h sweep never ran.
 * The boot sweep mirrors how `MovieService.start()` already re-runs its TMDB
 * retry ~10s after every boot. Freshness gating (Mongo-persisted) dedups the
 * overlap with the periodic sweeps and keeps the boot sweep churn-safe across
 * rapid restarts — a row attempted inside the 4h window isn't re-queued.
 *
 * With the worker running on more than one machine, each source's sweep is
 * gated by a cluster-wide occurrence claim ([[ScheduledRunStore]]): the sweep's
 * 4h window (keyed by source + boundary) is claimed by whichever machine fires
 * first in it, and the others skip — so a window's walk runs on one machine,
 * rotating, rather than on every machine. The boot sweep and the periodic fire
 * share the same per-source claim, so they also dedup each other.
 */
class EnrichmentReaper(
  cache:                MovieCacheReader,
  queue:                TaskQueue,
  freshness:            FreshnessStore,
  bootSweepDelaySeconds: Long = EnrichmentReaper.DefaultBootSweepDelaySeconds,
  runStore:             ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:                Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("enrichment-reaper")

  private val SweepPeriod: FiniteDuration = 4.hours

  private case class Sweep(taskType: TaskType, kind: FreshnessKind, eligible: MovieRecord => Boolean, offsetHours: Long)

  private val sweeps = Seq(
    Sweep(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined, 1L),
    Sweep(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined, 2L),
    Sweep(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined, 3L),
    Sweep(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, _.tmdbId.isDefined, 4L)
  )

  def start(): Unit = {
    // Boot backfill: kick every source once shortly after boot so hydrated-but-
    // unenriched rows get queued without waiting for the uptime-gated first
    // periodic fire below (which a frequently-restarting worker never reaches).
    scheduler.schedule(
      (() => Try(bootSweepClaimed())): Runnable, bootSweepDelaySeconds, TimeUnit.SECONDS)
    sweeps.foreach { s =>
      scheduler.scheduleAtFixedRate(
        () => Try(sweepIfClaimed(s)), s.offsetHours * 3600, 4 * 3600, TimeUnit.SECONDS)
    }
    logger.info(s"EnrichmentReaper started: boot sweep in ${bootSweepDelaySeconds}s, " +
                s"then ${sweeps.size} rating sweeps every 4h, staggered hourly.")
  }

  /** Run every source's sweep once. Package-private so tests can drive the
   *  reaper synchronously — bypasses the occurrence claim (single-caller). */
  private[tasks] def sweepOnce(): Int = sweeps.map(sweep).sum

  /** The claim-gated boot backfill: try to claim each source's current window
   *  and sweep the ones this machine wins. Package-private for tests. */
  private[tasks] def bootSweepClaimed(): Int = sweeps.map(sweepIfClaimed).sum

  /** Sweep one source only if this machine wins its current 4h occurrence —
   *  otherwise another machine is handling this window, so skip. Returns the
   *  number of tasks enqueued (0 when the claim was lost). */
  private def sweepIfClaimed(s: Sweep): Int = {
    val key = OccurrenceKey.at(s"enrich-${s.kind.label}", clock.millis(), SweepPeriod, s.offsetHours.hours)
    if (runStore.claim(key)) sweep(s) else 0
  }

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

object EnrichmentReaper {
  /** How long after `start()` the boot backfill sweep fires. Short enough to be
   *  reached before a restart-churning worker reboots, but past the cold-boot
   *  window so the corpus-wide enqueue doesn't pile onto hydration. */
  val DefaultBootSweepDelaySeconds: Long = 60L
}
