package services.tasks

import settings.OmdbBackfillInterval

import services.schedule.{AlwaysClaimScheduledRunStore, ScheduledRunStore}

import java.time.Clock
import scala.concurrent.duration._

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
  interval:     => OmdbBackfillInterval = OmdbBackfillInterval(OmdbBackfillReaper.DefaultInterval),
  // A small spacing before the first enqueue so the synchronous hydrate has
  // populated the cache (the sweep is a no-op on a cold one anyway). 0 in tests
  // that drive `tickIfClaimed` directly.
  initialDelay: OmdbBackfillReaper.InitialDelay = OmdbBackfillReaper.InitialDelay(OmdbBackfillReaper.DefaultInitialDelay),
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock = Clock.systemUTC()
) extends ClaimedEnqueueReaper("omdb-backfill", enqueueSweep, interval.value, initialDelay.value, runStore, clock)

object OmdbBackfillReaper {

  /** How long after `start()` the first sweep is enqueued. */
  final case class InitialDelay(value: FiniteDuration) extends AnyVal

  /** Daily: OMDb is a slow-moving gap-filler — the unresolved tail only shrinks
   *  as new films land, and the free key is capped at 1000 lookups/day. */
  val DefaultInterval: FiniteDuration = 24.hours
  /** First sweep a few minutes after boot, once the hydrate has populated the
   *  cache. OMDb work is network-bound (no big heap pass), so it needn't stagger
   *  off the memory-heavy reconcile/settle ticks the way SettleReaper does. */
  val DefaultInitialDelay: FiniteDuration = 5.minutes
}
