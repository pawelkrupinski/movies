package services.tasks

import play.api.Logging
import services.Stoppable
import services.movies.{CacheKey, MovieCacheReader}
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Re-tries TMDB resolution for rows still missing a `tmdbId`, SMEARED across the
 * [[DueWindow]] period the same way [[EnrichmentReaper]] smears rating refresh.
 * This is the queue-era replacement for `MovieService`'s daily, all-at-once
 * `retryUnresolvedTmdb` sweep, which re-dispatched the WHOLE unresolved backlog
 * 10s after boot and once every 24h — the boot/period `ResolveTmdb` burst that
 * pinned the shared-CPU credit balance (every re-key / title-rule wave leaves a
 * batch of rows un-concluded, and a restart re-fed them all at once).
 *
 * Each unresolved row is re-tried once per `dueWindow.period` (24h, matching the
 * old daily cadence) at a deterministic phase offset hashed from its resolve
 * dedup key, so a synchronized backlog goes out as a flat trickle (~`N ·
 * tickInterval / period` per tick) instead of one spike. A frequent tick
 * (`tickInterval`, 5min) re-tries only the rows whose personal period boundary
 * fell in the last tick: `dueWindow.isDue(key, Some(now − tickInterval), now)`
 * is true exactly when a window boundary was crossed in that slot. Contiguous
 * ticks (`since` = the previous tick's `now`) cover the period once, so every
 * key fires exactly once per period.
 *
 * Unlike [[EnrichmentReaper]] there is no persisted "last retry" stamp to lean
 * on, so a missed tick (GC pause, restart straddling the boundary) makes that
 * row wait until its next period boundary rather than being caught on the next
 * tick. That's acceptable for a best-effort retry of genuinely-stuck rows (the
 * cadence was already once-per-24h); it trades EnrichmentReaper's
 * stamp-backed robustness for needing no new freshness write.
 *
 * The per-row re-try itself (clear that row's negative marker, then dispatch a
 * `ResolveTmdb`) lives behind the `retry` seam — `MovieService.retryResolve` —
 * so the corpus/negative/hint business logic stays in `MovieService` and this
 * class owns only the schedule, the eligibility scan, the phase gate, the
 * per-tick cap, and the cluster occurrence claim. On a multi-machine worker each
 * tick is gated by a cluster-wide [[ScheduledRunStore]] claim keyed by the tick
 * window, so one machine sweeps per tick.
 */
class UnresolvedTmdbReaper(
  cache:     MovieCacheReader,
  // Re-try one still-unresolved row (clear its negative + dispatch ResolveTmdb).
  // Wired to `MovieService.retryResolve`; a recording fn in tests.
  retry:     CacheKey => Unit,
  // The shared due schedule — each unresolved row re-tried once per its period,
  // phase-spread across it. 24h matches the old daily sweep cadence.
  dueWindow: DueWindow = new DueWindow(24.hours),
  // How often the reaper wakes to re-try the slice now due — the spread
  // granularity. Smaller = flatter trickle, at the cost of more (cheap,
  // in-memory) corpus scans. 5min ≈ 288 ticks per 24h period.
  tickInterval: FiniteDuration = UnresolvedTmdbReaper.DefaultTickInterval,
  // A small spacing before the first tick (0 in tests that drive `tick` directly).
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on re-tries per tick. The phase spread keeps steady-state ticks tiny, but
  // a clock jump or a period shorter than the backlog could bunch them; capping
  // bounds that the way `ScrapeReaper` / `EnrichmentReaper` do — the leftover
  // stays due (re-tried next period). Default unbounded so tests driving `tick`
  // are unaffected; the wiring sets a finite cap.
  maxEnqueuePerTick: Int = Int.MaxValue,
  // While the worker is CPU-credit throttled, cap enqueue to this trickle so the
  // whole pipeline quiets and the pool can idle to rebuild credit. Default unbounded.
  throttledMaxEnqueuePerTick: Int = Int.MaxValue,
  throttle: ScrapeThrottleSignal = ScrapeThrottleSignal.AlwaysHealthy,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("unresolved-tmdb-reaper")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(
      () => Try(tickIfClaimed()), initialDelay.toMillis, tickInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"UnresolvedTmdbReaper started: unresolved rows re-tried once per ${dueWindow.period.toHours}h, " +
                s"phase-spread over ticks every ${tickInterval.toSeconds}s.")
  }

  /** Tick only if this machine wins the current tick window's occurrence claim —
   *  otherwise another machine is sweeping for this window. Package-private for tests. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("tmdb-retry-sweep", clock.millis(), tickInterval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Re-try every unresolved (`tmdbId` empty, detail not pending), now-due row,
   *  up to `maxEnqueuePerTick`. Package-private, with an injectable `nowMillis`,
   *  so tests can drive time. */
  private[tasks] def tick(nowMillis: Long = clock.millis()): Int = {
    val now   = Instant.ofEpochMilli(nowMillis)
    val since = Instant.ofEpochMilli(nowMillis - tickInterval.toMillis)
    val cap   = ScrapeThrottleSignal.cap(throttle, maxEnqueuePerTick, throttledMaxEnqueuePerTick)
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      if (record.tmdbId.isEmpty && !record.detailPending &&
          dueWindow.isDue(EnrichTaskKeys.resolveTmdbDedup(key.cleanTitle, key.year), Some(since), now)) {
        retry(key)
        enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"UnresolvedTmdbReaper re-tried $enqueued unresolved row(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object UnresolvedTmdbReaper {
  /** How often the reaper wakes to re-try the now-due slice. At 5min over a 24h
   *  period the backlog is spread across ~288 ticks. */
  val DefaultTickInterval: FiniteDuration = 5.minutes
}
