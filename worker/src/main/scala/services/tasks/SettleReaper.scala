package services.tasks

import play.api.Logging
import services.Stoppable
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically re-asserts the cache's one-row-per-film invariant by running the
 * whole-corpus settle (`MovieService.settle` → `MovieCache.canonicalizeBySanitize`)
 * over the in-memory cache. This is the SOLE periodic production caller of the
 * settle, decoupled from the cache hydrate.
 *
 * The hydrate used to run the settle on every Mongo load (boot + the 30-min
 * backstop reload). But a load re-derives every key as `displayTitle` via
 * `StoredMovieRecord.fromStorage`, so settling right after it re-keyed any row
 * whose canonical spelling differed — the per-deploy re-key/retrigger flap. Moving
 * the settle onto its own tick (over the already-loaded cache, no key
 * re-derivation) leaves the load a pure read and lets the settle act only on a
 * GENUINE split — cross-title or cross-year same-`tmdbId` rows, the collapse only
 * the whole-corpus `groupByFilm` does, which the scoped staging fold can't.
 *
 * Cluster-safe: a multi-machine worker gates each tick on a window occurrence
 * claim ([[ScheduledRunStore]]) so one machine settles per window. Settling an
 * empty/cold cache is a harmless no-op, so no hydrate-readiness gate is needed.
 */
class SettleReaper(
  settle: () => Unit,
  // The settle cadence — once per this period, preserving the old hydrate-backstop
  // frequency (`KINOWO_CACHE_REHYDRATE_SECONDS` = 1800s). BY-NAME so an
  // `/admin/config` flip applies on the next cycle without a restart.
  interval:     => FiniteDuration = SettleReaper.DefaultInterval,
  // A small spacing before the first tick so the synchronous hydrate has populated
  // the cache; the settle is a no-op on a cold cache anyway. 0 in tests that drive
  // `tickIfClaimed` directly.
  initialDelay: FiniteDuration = SettleReaper.DefaultInitialDelay,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("settle-reaper")

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"SettleReaper started: whole-corpus settle once per ${interval.toSeconds}s.")
  }

  /** Self-rescheduling tick: settle, then schedule the next reading `interval`
   *  afresh — so an interval flip applies on the next cycle. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(interval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Settle only if this machine wins the current window's occurrence claim —
   *  otherwise another machine is settling this window, so skip. Returns true when
   *  it settled. Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at("settle", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { settle(); true } else false
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object SettleReaper {
  /** Matches the old hydrate-backstop cadence (`KINOWO_CACHE_REHYDRATE_SECONDS` =
   *  1800s): the settle ran once per 30 min via the reload; it now runs once per
   *  30 min on its own tick. */
  val DefaultInterval: FiniteDuration = 30.minutes
  /** First settle ~mid-window after boot, so the synchronous hydrate has long
   *  populated the cache before the first whole-corpus settle pass. (This delay used
   *  to also stagger the settle off the read-model *reproject's* heavy tick — the two
   *  full-corpus passes co-ticking is what tipped the 320m heap into OOM — but that
   *  periodic reproject has been retired, so the only remaining scheduled read-model
   *  sweep is the cheap id-only orphan prune, which the settle need not dodge.) */
  val DefaultInitialDelay: FiniteDuration = 16.minutes
}
