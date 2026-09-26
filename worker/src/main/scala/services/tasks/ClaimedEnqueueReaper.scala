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
 * A recurring, cluster-claimed ENQUEUER: every `interval` (first after `initialDelay`) it puts
 * work on the task queue through `enqueue` — only if this machine wins the window's occurrence
 * claim ([[ScheduledRunStore]]), so each window enqueues once across every replica. The work
 * itself runs on the TaskWorker with the rest of the pipeline's metrics, retries and pool bound;
 * this only decides WHEN.
 *
 * `interval` is BY-NAME so an `/admin/config` flip applies on the next cycle.
 *
 * `timing` says when the ticks after the boot tick fire: `interval` after the last one
 * (`FromBoot`), or at the start of each window, `Aligned(offset)` past the epoch-aligned boundary —
 * a daily job at a fixed time of day, whenever the replicas were deployed. An aligned reaper's boot
 * tick still claims the CURRENT window, so a window every replica slept through runs on boot.
 */
class ClaimedEnqueueReaper(
  occurrence:   String,
  enqueue:      () => Unit,
  interval:     => FiniteDuration,
  initialDelay: FiniteDuration,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock = Clock.systemUTC(),
  timing:       ClaimedEnqueueReaper.Timing = ClaimedEnqueueReaper.Timing.FromBoot
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler(s"$occurrence-reaper")

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"$occurrence reaper started: enqueue once per ${interval.toSeconds}s.")
  }

  /** Self-rescheduling tick: enqueue, then schedule the next reading `interval` afresh. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(nextDelay()) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Enqueue only if this machine wins the current window's occurrence claim. Returns true when
   *  it enqueued. Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at(occurrence, clock.millis(), interval, offset)
    if (runStore.claim(key)) { enqueue(); true } else false
  }

  /** How long until the tick after this one. Package-private so tests can read it. */
  private[tasks] def nextDelay(): FiniteDuration = timing match {
    case ClaimedEnqueueReaper.Timing.FromBoot => interval
    case ClaimedEnqueueReaper.Timing.Aligned(_) =>
      val now = clock.millis()
      (OccurrenceKey.windowStart(now, interval, offset) + interval.toMillis - now).millis + ClaimedEnqueueReaper.Timing.Margin
  }

  private def offset: FiniteDuration = timing match {
    case ClaimedEnqueueReaper.Timing.Aligned(offset) => offset
    case ClaimedEnqueueReaper.Timing.FromBoot        => 0.seconds
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ClaimedEnqueueReaper {
  sealed trait Timing
  object Timing {
    /** Each tick `interval` after the last, the first `initialDelay` after boot. */
    case object FromBoot extends Timing
    /** Each tick at the start of a window, `offset` past the epoch-aligned boundary. */
    final case class Aligned(offset: FiniteDuration) extends Timing
    /** How far past a window's start an aligned tick fires: a timer that fires a little early
     *  would otherwise claim the window just ended, already claimed, and skip a whole period. */
    val Margin: FiniteDuration = 1.second
  }
}
