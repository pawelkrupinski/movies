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
 * Daily, cluster-claimed ENQUEUER for the OMDb identifier backfill. The sweep
 * itself runs as a coarse worker task (`TaskType.RefreshAllOmdb`, handled by a
 * `BulkRefreshHandler` → [[services.enrichment.OmdbBackfill]]`.refreshAllNow`);
 * this reaper just puts ONE sweep task on the queue per window (the task's
 * constant dedup key collapses any overlap), so the work runs on the TaskWorker
 * with the rest of the pipeline's metrics/retries rather than a private thread.
 * The sweep itself no-ops (no HTTP) for any row already carrying both
 * identifiers, and backs off films OMDb couldn't resolve — so it only hits OMDb
 * for the fresh unresolved tail (well within the free 1000/day quota).
 *
 * Only constructed when `OMDB_API_KEY` is set (see `WorkerWiring`), so the whole
 * reaper is absent on the default key-less deployment.
 *
 * Cluster-safe: each tick is gated on a window occurrence claim
 * ([[ScheduledRunStore]]) so exactly one machine enqueues per window.
 */
class OmdbBackfillReaper(
  enqueueSweep: () => Unit,
  // Daily by default. BY-NAME so an `/admin/config` flip applies next cycle.
  interval:     => FiniteDuration = OmdbBackfillReaper.DefaultInterval,
  // A small spacing before the first enqueue so the synchronous hydrate has
  // populated the cache (the sweep is a no-op on a cold one anyway). 0 in tests
  // that drive `tickIfClaimed` directly.
  initialDelay: FiniteDuration = OmdbBackfillReaper.DefaultInitialDelay,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("omdb-backfill-reaper")

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"OmdbBackfillReaper started: enqueue OMDb backfill task once per ${interval.toSeconds}s.")
  }

  /** Self-rescheduling tick: sweep, then schedule the next reading `interval`
   *  afresh — so an interval flip applies on the next cycle. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(interval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Enqueue a sweep only if this machine wins the current window's occurrence
   *  claim — otherwise another machine has enqueued this window, so skip. Returns
   *  true when it enqueued. Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at("omdb-backfill", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { enqueueSweep(); true } else false
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object OmdbBackfillReaper {
  /** Daily: OMDb is a slow-moving gap-filler — the unresolved tail only shrinks
   *  as new films land, and the free key is capped at 1000 lookups/day. */
  val DefaultInterval: FiniteDuration = 24.hours
  /** First sweep a few minutes after boot, once the hydrate has populated the
   *  cache. OMDb work is network-bound (no big heap pass), so it needn't stagger
   *  off the memory-heavy reconcile/settle ticks the way SettleReaper does. */
  val DefaultInitialDelay: FiniteDuration = 5.minutes
}
