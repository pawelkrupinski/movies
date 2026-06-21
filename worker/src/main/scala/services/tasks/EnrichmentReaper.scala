package services.tasks

import models.MovieRecord
import play.api.Logging
import services.Stoppable
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.MovieCacheReader
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically enqueues rating-refresh tasks for stale rows — the queue-based
 * replacement for the `*Ratings` classes' 4h cache walks.
 *
 * Each rating source refreshes every eligible row once per the [[DueWindow]]
 * period (4h, matching the rating TTL in [[services.freshness.Freshness]]). But
 * instead of walking the whole corpus in one burst — which used to drop ~750 tasks
 * at once, four times every 4h, and pin the shared-CPU credit (the midday `steal`
 * spikes) — the walk is SMEARED across the period: each `(row, source)` gets a
 * deterministic phase offset in `[0, period)` from a hash of its dedup key,
 * and a frequent tick (`tickInterval`, default 1min) enqueues only the rows
 * whose personal period boundary has passed since their last refresh. Over a
 * period the same ~750 tasks per source go out, but ~`N · tickInterval /
 * period` per tick (≈3 for 750 rows at 1min/4h) — a flat per-minute trickle, not
 * the ~5min-wide burst a coarser cadence dumps in one tick. The four sources
 * self-decorrelate because the dedup key embeds the
 * source label, so a film's phase differs per source — no manual stagger needed.
 *
 * The spread is self-sustaining and self-correcting: a row refreshed at its
 * boundary is next due exactly one period later (phases stay fixed, so the
 * spread persists), and a missed tick (GC pause, restart) is caught by the next
 * tick because due-ness is computed from the persisted last-refresh stamp, not
 * an in-memory timer. A never-refreshed row is due immediately, so rows hydrated
 * from Mongo but never enriched are queued on the first tick after boot without
 * a separate boot sweep — bounded by `maxEnqueuePerTick` so a cold corpus
 * doesn't enqueue everything at once (the leftover stays due and drains over the
 * next few ticks).
 *
 * A sweep only enqueues rows the source can act on (IMDb needs an imdbId; the
 * others need a resolved tmdbId), matching the old walks' scope. Enqueue is
 * deduped by the queue's unique index and re-gated by [[RatingHandler]] at
 * pickup using the SAME [[DueWindow]], so the handler skips a task iff this
 * reaper would no longer enqueue it (a race where another machine refreshed it
 * first) — never a task that is still due. On a multi-machine worker each tick is gated by a cluster-wide
 * occurrence claim ([[ScheduledRunStore]]) keyed by the tick window, so one
 * machine walks the corpus per tick, rotating — not every machine every tick.
 */
class EnrichmentReaper(
  cache:     MovieCacheReader,
  queue:     TaskQueue,
  freshness: FreshnessStore,
  // The shared due schedule (each row refreshed once per its period, phase-spread
  // across it). The SAME instance must back `RatingHandler` so this enqueue gate
  // and that execution gate agree on what counts as due — see [[DueWindow]].
  dueWindow: DueWindow = new DueWindow(4.hours),
  // How often the reaper wakes to enqueue the slice of the corpus now due — the
  // spread granularity. Smaller = flatter trickle, at the cost of more (cheap,
  // in-memory) corpus scans. Defaults to 1min (≈240 ticks per 4h period). BY-NAME
  // + a self-rescheduling tick (not fixed-delay) so an `/admin/config` flip of the
  // interval applies mid-flight, on the next cycle, without a restart.
  tickInterval: => FiniteDuration = EnrichmentReaper.DefaultTickInterval,
  // A small spacing before the first tick (0 in tests that drive `tick` directly).
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on enqueues per tick. The phase spread keeps steady-state ticks small,
  // but a cold or long-down corpus has every row due at once; capping bounds that
  // recovery burst the same way `ScrapeReaper` does — the leftover stays due and
  // drains over the next ticks. Default unbounded so tests driving `tick` are
  // unaffected; the wiring sets a finite cap comfortably above the steady-state.
  // BY-NAME: read live each tick, so an `/admin/config` flip applies mid-flight.
  maxEnqueuePerTick: => Int = Int.MaxValue,
  // While the worker is CPU-credit throttled, cap enqueue to this trickle. Ratings
  // are the dominant non-scrape load, so backing this off is what actually lets the
  // pool idle + rebuild credit (the scrape-only watchdog couldn't). Default unbounded.
  throttledMaxEnqueuePerTick: => Int = Int.MaxValue,
  throttle: ScrapeThrottleSignal = ScrapeThrottleSignal.AlwaysHealthy,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("enrichment-reaper")

  private case class Sweep(taskType: TaskType, kind: FreshnessKind, eligible: MovieRecord => Boolean)

  private val sweeps = Seq(
    Sweep(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined),
    Sweep(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined),
    Sweep(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined),
    Sweep(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, _.tmdbId.isDefined)
  )

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"EnrichmentReaper started: ${sweeps.size} rating source(s), each row refreshed once per " +
                s"${dueWindow.period.toHours}h, phase-spread over ticks every ${tickInterval.toSeconds}s.")
  }

  /** Self-rescheduling tick: run, then schedule the next one reading `tickInterval`
   *  afresh — so an interval flip applies on the next cycle (a fixed-delay schedule
   *  would freeze the boot-time value). */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(tickInterval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Tick only if this machine wins the current tick window's occurrence claim —
   *  otherwise another machine is walking the corpus for this window, so skip.
   *  Package-private for tests. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("enrich-sweep", clock.millis(), tickInterval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Enqueue every eligible, now-due `(row, source)`, up to `maxEnqueuePerTick`.
   *  Package-private, with an injectable `nowMillis`, so tests can drive time. */
  private[tasks] def tick(nowMillis: Long = clock.millis()): Int = {
    val cap      = ScrapeThrottleSignal.cap(throttle, maxEnqueuePerTick, throttledMaxEnqueuePerTick)
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      val sources = sweeps.iterator
      while (sources.hasNext && enqueued < cap) {
        val s = sources.next()
        if (s.eligible(record)) {
          val dedupKey = RatingTasks.dedupKey(s.kind, key, record.tmdbId)
          // Honour a stamp left under the legacy title-based key so switching to
          // tmdbId-keyed freshness doesn't re-queue the whole resolved corpus once
          // on deploy: a row fresh under its old title key isn't re-enqueued, and
          // seeds the tmdbId key on its next due refresh. (Drop the fallback once no
          // legacy stamps remain.)
          val lastFetched = freshness.lastFetchedAt(dedupKey)
            .orElse(freshness.lastFetchedAt(RatingTasks.dedupKey(s.kind, key)))
          if (dueWindow.isDue(dedupKey, lastFetched, Instant.ofEpochMilli(nowMillis)) &&
              queue.enqueue(s.taskType, dedupKey, RatingTasks.payload(key)) == EnqueueResult.Added)
            enqueued += 1
        }
      }
    }
    if (enqueued > 0) logger.info(s"EnrichmentReaper enqueued $enqueued due rating-refresh task(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object EnrichmentReaper {
  /** How often the reaper wakes to enqueue the now-due slice of the corpus. At
   *  1min over a 4h period the corpus spreads across ~240 ticks, so each tick
   *  enqueues only ~1/240 of each source — a flat per-minute trickle rather than
   *  the ~5min-wide bursts a coarser cadence dumps in one tick (the residual
   *  `kinowo_worker_tasks` rating spikes the panel still showed). The walk is a
   *  cheap in-memory corpus scan, so the finer cadence costs little. */
  val DefaultTickInterval: FiniteDuration = 1.minute
}
