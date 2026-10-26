package services.tasks

import settings.OmdbBackfillInterval

import services.schedule.{AlwaysClaimScheduledRunStore, ScheduledRunStore}

import java.time.Clock
import scala.concurrent.duration._

/**
 * Hourly, cluster-claimed ENQUEUER for the OMDb identifier backfill. The sweep
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
  enqueueSweep: () => Enqueued,
  // Hourly by default. BY-NAME so an `/admin/config` flip applies next cycle.
  interval:     => OmdbBackfillInterval = OmdbBackfillInterval(OmdbBackfillReaper.DefaultInterval),
  // A small spacing before the first enqueue so the synchronous hydrate has
  // populated the cache (the sweep is a no-op on a cold one anyway). 0 in tests
  // that drive `tickIfClaimed` directly.
  initialDelay: OmdbBackfillReaper.InitialDelay = OmdbBackfillReaper.InitialDelay(OmdbBackfillReaper.DefaultInitialDelay),
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock,
  ticks:        tools.TickRecorder = tools.TickRecorder.Unrecorded
) extends ClaimedEnqueueReaper("omdb-backfill", enqueueSweep, interval.value, initialDelay.value, runStore, clock, ticks = ticks)

object OmdbBackfillReaper {

  /** How long after `start()` the first sweep is enqueued. */
  final case class InitialDelay(value: FiniteDuration) extends AnyVal

  /** Hourly: the sweep's OMDb requests are bulk asks, which the fleet's budget ([[services.enrichment.OmdbBudget]])
   *  grants only as the day goes by, so a sweep takes the hour's share and stops; the rest of the tail waits for the
   *  next. A daily sweep took the whole day's quota in one go, five countries at once after a deploy (2026-10-26). A
   *  sweep with nothing to ask makes no request: identified rows and backed-off films are skipped in memory. */
  val DefaultInterval: FiniteDuration = 1.hour
  /** First sweep a few minutes after boot, once the hydrate has populated the
   *  cache. OMDb work is network-bound (no big heap pass), so it needn't stagger
   *  off the memory-heavy reconcile/settle ticks the way SettleReaper does. */
  val DefaultInitialDelay: FiniteDuration = 5.minutes
}
