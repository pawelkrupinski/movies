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
import scala.util.hashing.MurmurHash3

/**
 * Periodically enqueues rating-refresh tasks for stale rows — the queue-based
 * replacement for the `*Ratings` classes' 4h cache walks.
 *
 * Each rating source refreshes every eligible row once per `sweepPeriod` (4h,
 * matching the rating TTL in [[services.freshness.Freshness]]). But instead of
 * walking the whole corpus in one burst — which used to drop ~750 tasks at once,
 * four times every 4h, and pin the shared-CPU credit (the midday `steal` spikes)
 * — the walk is SMEARED across the period: each `(row, source)` gets a
 * deterministic phase offset in `[0, sweepPeriod)` from a hash of its dedup key,
 * and a frequent tick (`tickInterval`, default 5min) enqueues only the rows
 * whose personal period boundary has passed since their last refresh. Over a
 * period the same ~750 tasks per source go out, but ~`N · tickInterval /
 * sweepPeriod` per tick (≈16 for 750 rows at 5min/4h) — a flat trickle, not a
 * spike. The four sources self-decorrelate because the dedup key embeds the
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
 * pickup (a freshness re-check), so over-enqueueing is cheap and safe across
 * servers. On a multi-machine worker each tick is gated by a cluster-wide
 * occurrence claim ([[ScheduledRunStore]]) keyed by the tick window, so one
 * machine walks the corpus per tick, rotating — not every machine every tick.
 */
class EnrichmentReaper(
  cache:     MovieCacheReader,
  queue:     TaskQueue,
  freshness: FreshnessStore,
  // Each (row, source) refreshes once per this period, phase-spread across it.
  // Must match the rating TTL in `Freshness.ttlFor` so this enqueue gate and
  // `RatingHandler`'s execution gate agree on what counts as "fresh".
  sweepPeriod: FiniteDuration = 4.hours,
  // How often the reaper wakes to enqueue the slice of the corpus now due — the
  // spread granularity. Smaller = flatter trickle, at the cost of more (cheap,
  // in-memory) corpus scans. Defaults to 5min (≈48 ticks per 4h period).
  tickInterval: FiniteDuration = EnrichmentReaper.DefaultTickInterval,
  // A small spacing before the first tick (0 in tests that drive `tick` directly).
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on enqueues per tick. The phase spread keeps steady-state ticks small,
  // but a cold or long-down corpus has every row due at once; capping bounds that
  // recovery burst the same way `ScrapeReaper` does — the leftover stays due and
  // drains over the next ticks. Default unbounded so tests driving `tick` are
  // unaffected; the wiring sets a finite cap comfortably above the steady-state.
  maxEnqueuePerTick: Int = Int.MaxValue,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("enrichment-reaper")
  private val periodMillis: Long = sweepPeriod.toMillis

  private case class Sweep(taskType: TaskType, kind: FreshnessKind, eligible: MovieRecord => Boolean)

  private val sweeps = Seq(
    Sweep(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined),
    Sweep(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined),
    Sweep(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined),
    Sweep(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, _.tmdbId.isDefined)
  )

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(
      () => Try(tickIfClaimed()), initialDelay.toMillis, tickInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"EnrichmentReaper started: ${sweeps.size} rating source(s), each row refreshed once per " +
                s"${sweepPeriod.toHours}h, phase-spread over ticks every ${tickInterval.toSeconds}s.")
  }

  /** Tick only if this machine wins the current tick window's occurrence claim —
   *  otherwise another machine is walking the corpus for this window, so skip.
   *  Package-private for tests. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("enrich-sweep", clock.millis(), tickInterval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Per-`(row, source)` phase offset in `[0, sweepPeriod)`, deterministic from
   *  the dedup key — stable across restarts and needing no storage. The dedup key
   *  embeds the source label, so a film's four sources land on independent phases. */
  private def phaseMillis(dedupKey: String): Long =
    Math.floorMod(MurmurHash3.stringHash(dedupKey).toLong, periodMillis)

  /** Which period-length window (counted from this key's own phase) `atMillis`
   *  falls in. A new window means the key is due for another refresh. */
  private def windowIndex(atMillis: Long, dedupKey: String): Long =
    Math.floorDiv(atMillis - phaseMillis(dedupKey), periodMillis)

  /** Due iff never refreshed, or a personal period boundary has passed since the
   *  last refresh (the key has entered a new window). */
  private def isDue(dedupKey: String, nowMillis: Long): Boolean =
    freshness.lastFetchedAt(dedupKey) match {
      case None    => true
      case Some(t) => windowIndex(nowMillis, dedupKey) > windowIndex(t.toEpochMilli, dedupKey)
    }

  /** Enqueue every eligible, now-due `(row, source)`, up to `maxEnqueuePerTick`.
   *  Package-private, with an injectable `nowMillis`, so tests can drive time. */
  private[tasks] def tick(nowMillis: Long = clock.millis()): Int = {
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < maxEnqueuePerTick) {
      val (key, record) = rows.next()
      val sources = sweeps.iterator
      while (sources.hasNext && enqueued < maxEnqueuePerTick) {
        val s = sources.next()
        if (s.eligible(record)) {
          val dedupKey = RatingTasks.dedupKey(s.kind, key)
          if (isDue(dedupKey, nowMillis) &&
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
   *  5min over a 4h period the corpus is spread across ~48 ticks. */
  val DefaultTickInterval: FiniteDuration = 5.minutes
}
