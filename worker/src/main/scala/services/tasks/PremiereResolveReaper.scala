package services.tasks

import models.MovieRecord
import play.api.Logging
import services.Stoppable
import services.movies.{CacheKey, MovieCacheReader}
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.{Clock, Instant, LocalDateTime, ZoneId}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Re-activates TMDB resolution for still-unresolved rows in the WEEK LEADING UP
 * TO their first screening — the window when a film finally lands in TMDB / IMDb
 * / Filmweb. This replaces the old blanket "retry every id-less row once per 24h
 * FOREVER" sweep: a film is now resolved once at ingest (`MovieDetailsComplete`)
 * and again on any resolution-input field change (`MergeRetrigger`); the only
 * remaining SCHEDULED re-try is this premiere window. A row whose premiere is
 * still weeks away, or has already passed (beyond a small grace), is left alone —
 * re-hitting an unfindable event every day bought nothing but shared-CPU credit.
 *
 * "First screening" is `MovieRecord.firstScreeningDate` — the min-ever premiere
 * maintained at the persist boundary. A row is eligible while `now` sits in
 * `[premiere − graceDays, premiere + leadDays)`, compared in the cinema-listing
 * zone (Europe/Warsaw) that the showtimes are stated in. The window is sized to
 * the ~6–9 day cinema listing horizon: a film usually enters the corpus only days
 * before its premiere, exactly when the external databases gain its entry.
 *
 * Within that window the row is still SMEARED across the [[DueWindow]] period the
 * way [[EnrichmentReaper]] smears rating refresh — re-tried once per
 * `dueWindow.period` (24h) at a deterministic phase offset hashed from its resolve
 * dedup key, so a synchronized premiere cohort trickles out instead of bursting.
 * A frequent tick (`tickInterval`, 5min) re-tries only the rows whose personal
 * period boundary fell in the last tick.
 *
 * The per-row re-try itself (clear that row's negative marker, then dispatch a
 * `ResolveTmdb`) lives behind the `retry` seam — `MovieService.retryResolve` — so
 * the corpus/negative/hint business logic stays in `MovieService` and this class
 * owns only the schedule, the eligibility scan (premiere window + not-resolved +
 * detail-done), the phase gate, the per-tick cap, and the cluster occurrence
 * claim. On a multi-machine worker each tick is gated by a cluster-wide
 * [[ScheduledRunStore]] claim keyed by the tick window, so one machine sweeps.
 */
class PremiereResolveReaper(
  cache:     MovieCacheReader,
  // Re-try one still-unresolved row (clear its negative + dispatch ResolveTmdb).
  // Wired to `MovieService.retryResolve`; a recording fn in tests.
  retry:     CacheKey => Unit,
  // The premiere window: a row is eligible from `leadDays` before its first
  // screening until `graceDays` after it.
  leadDays:  Long = 7,
  graceDays: Long = 1,
  // The zone the cinema-listing showtimes (and thus `firstScreeningDate`) are
  // stated in; `now` is converted to it before the window comparison.
  zone:      ZoneId = ZoneId.of("Europe/Warsaw"),
  // The shared due schedule — each in-window row re-tried once per its period,
  // phase-spread across it.
  dueWindow: DueWindow = new DueWindow(24.hours),
  // How often the reaper wakes to re-try the slice now due — the spread
  // granularity. 5min ≈ 288 ticks per 24h period.
  tickInterval: FiniteDuration = PremiereResolveReaper.DefaultTickInterval,
  // A small spacing before the first tick (0 in tests that drive `tick` directly).
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on re-tries per tick. The phase spread keeps steady-state ticks tiny, but
  // a clock jump could bunch them; the leftover stays due (re-tried next period).
  maxEnqueuePerTick: Int = Int.MaxValue,
  // While the worker is CPU-credit throttled, cap enqueue to this trickle.
  throttledMaxEnqueuePerTick: Int = Int.MaxValue,
  throttle: ScrapeThrottleSignal = ScrapeThrottleSignal.AlwaysHealthy,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("premiere-resolve-reaper")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(
      () => Try(tickIfClaimed()), initialDelay.toMillis, tickInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"PremiereResolveReaper started: unresolved rows re-tried once per ${dueWindow.period.toHours}h " +
                s"while within [-$graceDays, +$leadDays] days of their first screening, " +
                s"phase-spread over ticks every ${tickInterval.toSeconds}s.")
  }

  /** Tick only if this machine wins the current tick window's occurrence claim —
   *  otherwise another machine is sweeping for this window. Package-private for tests. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("premiere-resolve-sweep", clock.millis(), tickInterval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** True when `record`'s first screening falls within the premiere window around
   *  `nowLocal`: from `leadDays` before it up to `graceDays` after. A row with no
   *  known first-screening date is never in any window. */
  private[tasks] def inPremiereWindow(record: MovieRecord, nowLocal: LocalDateTime): Boolean =
    record.firstScreeningDate.exists(premiere =>
      !premiere.isBefore(nowLocal.minusDays(graceDays)) && premiere.isBefore(nowLocal.plusDays(leadDays)))

  /** Re-try every unresolved (`tmdbId` empty, detail not pending), in-premiere-window,
   *  now-due row, up to `maxEnqueuePerTick`. Package-private, with an injectable
   *  `nowMillis`, so tests can drive time. */
  private[tasks] def tick(nowMillis: Long = clock.millis()): Int = {
    val now      = Instant.ofEpochMilli(nowMillis)
    val since    = Instant.ofEpochMilli(nowMillis - tickInterval.toMillis)
    val nowLocal = LocalDateTime.ofInstant(now, zone)
    val cap      = ScrapeThrottleSignal.cap(throttle, maxEnqueuePerTick, throttledMaxEnqueuePerTick)
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      if (record.tmdbId.isEmpty && !record.detailPending &&
          inPremiereWindow(record, nowLocal) &&
          dueWindow.isDue(EnrichTaskKeys.resolveTmdbDedup(key.cleanTitle, key.year), Some(since), now)) {
        retry(key)
        enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"PremiereResolveReaper re-tried $enqueued unresolved pre-premiere row(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object PremiereResolveReaper {
  /** How often the reaper wakes to re-try the now-due slice. At 5min over a 24h
   *  period the in-window backlog is spread across ~288 ticks. */
  val DefaultTickInterval: FiniteDuration = 5.minutes
}
