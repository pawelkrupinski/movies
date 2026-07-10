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
 * Re-tries TMDB resolution for rows still missing a `tmdbId`, but ONLY while the
 * film is in its PREMIERE WINDOW — the days leading up to its first screening —
 * rather than re-trying every unresolved row forever. Resolution otherwise
 * happens once at ingest (`MovieDetailsComplete`) and again whenever a driver
 * field changes (`MergeRetrigger` re-fires `ResolveTmdb` on a title / year /
 * originalTitle / director change), so a blanket daily sweep of the whole
 * unresolved backlog mostly re-tried the event / opera / NT-Live long tail that
 * will never be in TMDB. Concentrating the re-try on the release week — when
 * TMDB / Filmweb / IMDb actually finalise the film's entry — is where a re-try
 * has a real chance of newly resolving, and it stops the futile churn (the
 * `ResolveTmdb` burst that pinned the shared-CPU credit balance on restart).
 *
 * A row is eligible when its first screening (`MovieRecord.firstScreeningDate`,
 * the earliest currently-listed showtime, derived live from the record) falls in
 * `[now − graceDays, now + leadDays)`, compared in the cinema-listing zone
 * (Europe/Warsaw) the showtimes are stated in. The date is read fresh from the
 * record each tick — NOT persisted — because the live showtimes converge
 * regardless of arrival order, whereas a stored value recomputed at a single
 * write path drifted with order (see `StagingOrderDeterminismSpec`). Once the
 * premiere showtime ages out of `data` the date advances to the next-earliest
 * listed showtime; harmless for this reaper, since by then the window is behind
 * us for that premiere (a still-upcoming premiere's showtime is by definition
 * still listed).
 *
 * Eligible rows are still SMEARED across the [[DueWindow]] period the same way
 * [[EnrichmentReaper]] smears rating refresh: each is re-tried once per
 * `dueWindow.period` (24h) at a deterministic phase offset hashed from its
 * resolve dedup key, so a synchronized in-window batch goes out as a flat
 * trickle instead of one spike. A frequent tick (`tickInterval`, 5min) re-tries
 * only the rows whose personal period boundary fell in the last tick:
 * `dueWindow.isDue(key, Some(now − tickInterval), now)` is true exactly when a
 * window boundary was crossed in that slot. Contiguous ticks (`since` = the
 * previous tick's `now`) cover the period once, so every eligible key fires once
 * per period.
 *
 * Unlike [[EnrichmentReaper]] there is no persisted "last retry" stamp to lean
 * on, so a missed tick (GC pause, restart straddling the boundary) makes that
 * row wait until its next period boundary rather than being caught on the next
 * tick. That's acceptable for a best-effort retry of genuinely-stuck rows; it
 * trades EnrichmentReaper's stamp-backed robustness for needing no new freshness
 * write.
 *
 * The per-row re-try itself (clear that row's negative marker, then dispatch a
 * `ResolveTmdb`) lives behind the `retry` seam — `MovieService.retryResolve` —
 * so the corpus/negative/hint business logic stays in `MovieService` and this
 * class owns only the schedule, the eligibility scan, the premiere-window gate,
 * the phase gate, the per-tick cap, and the cluster occurrence claim. On a
 * multi-machine worker each tick is gated by a cluster-wide [[ScheduledRunStore]]
 * claim keyed by the tick window, so one machine sweeps per tick.
 */
class PremiereResolveReaper(
  cache:     MovieCacheReader,
  // Re-try one still-unresolved row (clear its negative + dispatch ResolveTmdb).
  // Wired to `MovieService.retryResolve`; a recording fn in tests.
  retry:     CacheKey => Unit,
  // The premiere window: a row is eligible from `leadDays` before its first
  // screening until `graceDays` after it. Sized to the ~week before release when
  // the film's entries get finalised; Env-configurable at the wiring.
  leadDays:  Long = 7,
  graceDays: Long = 1,
  // The zone the cinema-listing showtimes (and thus `firstScreeningDate`) are
  // stated in, so "now" is compared to them apples-to-apples.
  zone:      ZoneId = ZoneId.of("Europe/Warsaw"),
  // The shared due schedule — each eligible row re-tried once per its period,
  // phase-spread across it. 24h matches the old daily sweep cadence.
  dueWindow: DueWindow = new DueWindow(24.hours),
  // How often the reaper wakes to re-try the slice now due — the spread
  // granularity. Smaller = flatter trickle, at the cost of more (cheap,
  // in-memory) corpus scans. 5min ≈ 288 ticks per 24h period.
  tickInterval: FiniteDuration = PremiereResolveReaper.DefaultTickInterval,
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

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("premiere-resolve-reaper")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(
      () => Try(tickIfClaimed()), initialDelay.toMillis, tickInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"PremiereResolveReaper started: unresolved rows re-tried in their premiere window " +
                s"[−$graceDays, +$leadDays] days, once per ${dueWindow.period.toHours}h, " +
                s"phase-spread over ticks every ${tickInterval.toSeconds}s.")
  }

  /** True when the film's first screening (earliest currently-listed showtime)
   *  falls in `[nowLocal − graceDays, nowLocal + leadDays)` — i.e. from `leadDays`
   *  before its premiere up to `graceDays` after. A row with no showtimes (no
   *  premiere date) is never in window. Package-private for tests. */
  private[tasks] def inPremiereWindow(record: MovieRecord, nowLocal: LocalDateTime): Boolean =
    record.firstScreeningDate.exists(premiere =>
      !premiere.isBefore(nowLocal.minusDays(graceDays)) && premiere.isBefore(nowLocal.plusDays(leadDays)))

  /** Tick only if this machine wins the current tick window's occurrence claim —
   *  otherwise another machine is sweeping for this window. Package-private for tests. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("tmdb-retry-sweep", clock.millis(), tickInterval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Re-try every unresolved (`tmdbId` empty, detail not pending), in-premiere-window,
   *  now-due row, up to `maxEnqueuePerTick`. Package-private, with an injectable
   *  `nowMillis`, so tests can drive time. */
  private[tasks] def tick(nowMillis: Long = clock.millis()): Int = {
    val now      = Instant.ofEpochMilli(nowMillis)
    val nowLocal = LocalDateTime.ofInstant(now, zone)
    val since    = Instant.ofEpochMilli(nowMillis - tickInterval.toMillis)
    val cap      = ScrapeThrottleSignal.cap(throttle, maxEnqueuePerTick, throttledMaxEnqueuePerTick)
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      if (record.tmdbId.isEmpty && !record.detailPending && inPremiereWindow(record, nowLocal) &&
          dueWindow.isDue(EnrichTaskKeys.resolveTmdbDedup(key.cleanTitle, key.year), Some(since), now)) {
        retry(key)
        enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"PremiereResolveReaper re-tried $enqueued unresolved row(s) in premiere window.")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object PremiereResolveReaper {
  /** How often the reaper wakes to re-try the now-due slice. At 5min over a 24h
   *  period the backlog is spread across ~288 ticks. */
  val DefaultTickInterval: FiniteDuration = 5.minutes
}
