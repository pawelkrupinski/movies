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
  /** First settle ~mid-window after boot. Two reasons for 16 min rather than 1:
   *  the synchronous hydrate has long populated the cache, AND — the load-bearing
   *  one — it staggers the whole-corpus settle OFF the read-model reconcile's tick.
   *  Both run a full-corpus pass every 30 min; the reconcile fires at boot+~1 min
   *  then every 30 (1/31/61/91…), so a 1-min settle delay put the two heaviest
   *  memory passes on the SAME minute, and their combined transient is what tipped
   *  the worker's 320m heap into OOM. At 16 min the settle ticks at 16/46/76… —
   *  ~15 min clear of every reconcile tick, so the peaks never overlap. */
  val DefaultInitialDelay: FiniteDuration = 16.minutes
}
