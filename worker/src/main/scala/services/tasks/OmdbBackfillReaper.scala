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
 * Periodically runs the OMDb IDENTIFIER backfill ([[services.enrichment.OmdbBackfill]])
 * over the whole cache: recover a missing `imdbId` / `rottenTomatoesUrl` for any
 * row still lacking them, so the canonical rating refreshers can then fill the
 * scores. Kept on its own LOW-frequency tick (daily) rather than the per-row
 * rating queue: OMDb is a cheap one-shot gap-filler, and a per-row TaskType would
 * ripple a new enum through ~40 exhaustive matches for no benefit. A row that
 * already has both identifiers is a no-op with no HTTP call, so most of a sweep
 * costs nothing — only the still-unresolved tail hits OMDb (well within the free
 * 1000/day quota).
 *
 * Only constructed when `OMDB_API_KEY` is set (see `WorkerWiring`), so the whole
 * reaper is absent on the default key-less deployment.
 *
 * Cluster-safe: each tick is gated on a window occurrence claim
 * ([[ScheduledRunStore]]) so exactly one machine sweeps per window.
 */
class OmdbBackfillReaper(
  backfill: () => Unit,
  // Daily by default. BY-NAME so an `/admin/config` flip applies next cycle.
  interval:     => FiniteDuration = OmdbBackfillReaper.DefaultInterval,
  // A small spacing before the first sweep so the synchronous hydrate has
  // populated the cache (the sweep is a no-op on a cold one anyway). 0 in tests
  // that drive `tickIfClaimed` directly.
  initialDelay: FiniteDuration = OmdbBackfillReaper.DefaultInitialDelay,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("omdb-backfill-reaper")

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"OmdbBackfillReaper started: OMDb identifier backfill once per ${interval.toSeconds}s.")
  }

  /** Self-rescheduling tick: sweep, then schedule the next reading `interval`
   *  afresh — so an interval flip applies on the next cycle. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(interval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Sweep only if this machine wins the current window's occurrence claim —
   *  otherwise another machine is sweeping this window, so skip. Returns true when
   *  it swept. Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at("omdb-backfill", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { backfill(); true } else false
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
