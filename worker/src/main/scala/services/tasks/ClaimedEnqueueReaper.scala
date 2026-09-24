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
 */
class ClaimedEnqueueReaper(
  occurrence:   String,
  enqueue:      () => Unit,
  interval:     => FiniteDuration,
  initialDelay: FiniteDuration,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler(s"$occurrence-reaper")

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"$occurrence reaper started: enqueue once per ${interval.toSeconds}s.")
  }

  /** Self-rescheduling tick: enqueue, then schedule the next reading `interval` afresh. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(interval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Enqueue only if this machine wins the current window's occurrence claim. Returns true when
   *  it enqueued. Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at(occurrence, clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { enqueue(); true } else false
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
