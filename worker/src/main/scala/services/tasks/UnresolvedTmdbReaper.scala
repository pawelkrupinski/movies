package services.tasks

import models.{Country, MovieRecord, Tmdb}
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
 * Re-runs TMDB resolution for the two kinds of row whose `Tmdb` slot is not what it
 * should be: rows still missing a `tmdbId` (never resolved), and rows RESOLVED IN THE
 * WRONG LANGUAGE — a slot stamped with a tag that isn't this deployment's. The second
 * exists because `fullDetails` is fetched only at resolve time, so a row resolved
 * before its deployment learned to enrich in its own language keeps Polish title /
 * synopsis / genres forever; Berlin was listing "Familijny, Komedia, Przygodowy" and
 * a Polish Star Wars title long after the fetch itself was fixed. Both kinds are
 * SMEARED across the
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
  // Force a re-resolve of an ALREADY-resolved row, so its `Tmdb` slot is re-fetched.
  // Wired to `MovieService.forceResolve`; a recording fn in tests. A no-op default
  // keeps the construction sites that don't exercise the stale-language path (specs,
  // scripts) unchanged — paired with the `Country.default` below, which matches every
  // legacy slot's language, so a defaulted reaper never reaches this seam anyway.
  forceRetry: CacheKey => Unit = _ => (),
  // This deployment's country — its `language` is the tag every `Tmdb` slot is
  // expected to carry. Poland by default, matching the historical enrichment
  // language, so a defaulted construction sweeps nothing extra.
  country:   Country = Country.default,
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
    var forced   = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      val unresolved = record.tmdbId.isEmpty && !record.detailPending
      val staleLang  = staleLanguage(record)
      if ((unresolved || staleLang) &&
          dueWindow.isDue(EnrichTaskKeys.resolveTmdbDedup(key.cleanTitle, key.year), Some(since), now)) {
        // A stale-language row is already resolved, so the plain retry (which
        // only fires on `tmdbId.isEmpty`) would no-op — it needs the forced path.
        if (unresolved) retry(key) else { forceRetry(key); forced += 1 }
        enqueued += 1
      }
    }
    if (enqueued > 0)
      logger.info(s"UnresolvedTmdbReaper re-tried ${enqueued - forced} unresolved row(s)" +
                  (if (forced > 0) s" and force-re-resolved $forced row(s) enriched in the wrong language." else "."))
    enqueued
  }

  /** True when this row's `Tmdb` slot holds text fetched in a language that is no
   *  longer the deployment's — the frozen-slot case. `fullDetails` is fetched only
   *  at resolve time, so without this sweep a row resolved before its deployment
   *  learned to enrich in its own language shows Polish text forever (Berlin
   *  listing "Familijny, Komedia" and a Polish title). An unstamped slot reads as
   *  `pl-PL`, the historical hardcoded default — exactly what those rows hold — so
   *  the Polish deployment matches on every row and never churns.
   *
   *  Gated on a slot that actually carries localized text: a slot the details
   *  fetch never filled has nothing stale to correct, and forcing it would just
   *  re-run a search that already concluded. */
  private[tasks] def staleLanguage(record: MovieRecord): Boolean =
    record.data.get(Tmdb).exists { slot =>
      val carriesLocalizedText =
        slot.genres.nonEmpty || slot.synopsis.nonEmpty || slot.countries.nonEmpty || slot.title.nonEmpty
      carriesLocalizedText &&
        slot.language.getOrElse(UnresolvedTmdbReaper.LegacyLanguageTag) != country.language.toLanguageTag
    }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object UnresolvedTmdbReaper {
  /** How often the reaper wakes to re-try the now-due slice. At 5min over a 24h
   *  period the backlog is spread across ~288 ticks. */
  val DefaultTickInterval: FiniteDuration = 5.minutes

  /** What an unstamped `Tmdb` slot was fetched in: every resolve before the
   *  per-country enrichment language landed hardcoded `pl-PL`. Reading `None` as
   *  this (rather than "unknown, re-resolve") keeps the Polish corpus — where the
   *  stamp is absent and the text is already right — completely still. */
  val LegacyLanguageTag: String = "pl-PL"
}
