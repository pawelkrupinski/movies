package services.tasks

import play.api.Logging
import services.Stoppable
import services.schedule.{OccurrenceKey, ScheduledRunStore}
import tools.{DaemonExecutors, ScheduledTick}

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._

/**
 * A job run once per `interval` window, first after `initialDelay`, on its own daemon thread —
 * and, on a multi-machine worker, only by the machine that wins the window's occurrence claim
 * (`ScheduledRunStore`, keyed `name`). Self-rescheduling: each tick reads `interval` afresh, so an
 * `/admin/config` flip applies on the next cycle without a restart. A failing run never stops the
 * schedule. The settle and the identity shadow run each ride their own.
 */
class ClaimedPeriodicTask(name: String, run: () => Unit, interval: => FiniteDuration, initialDelay: FiniteDuration,
                          runStore: ScheduledRunStore, clock: Clock,
                          // Builds the task's scheduler from its name; a spec hands in a stepped one.
                          newScheduler: String => ScheduledExecutorService = DaemonExecutors.scheduler(_))
    extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = newScheduler(name)

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"$name started: once per ${interval.toSeconds}s, first in ${initialDelay.toSeconds}s.")
  }

  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { ScheduledTick.logged(name, logger)(tickIfClaimed()); scheduleNext(intervalOr(delay)) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** The next run's delay: `interval` read afresh, or — when it cannot be read — the last delay,
   *  never under [[ClaimedPeriodicTask.MinimumFallbackDelay]], said out loud. Read bare, an
   *  `interval` that threw ended the schedule for good, silently; falling back to a first run's
   *  zero initial delay re-ran the tick back to back, forever. */
  private def intervalOr(lastDelay: FiniteDuration): FiniteDuration =
    try interval
    catch {
      case scala.util.control.NonFatal(e) =>
        val fallback = lastDelay.max(ClaimedPeriodicTask.MinimumFallbackDelay)
        logger.warn(s"$name: interval unreadable, next run in ${fallback.toMillis}ms: $e", e); fallback
    }

  /** Run only if this machine wins the current window's claim. True when it ran. */
  def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at(name, clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { run(); true } else false
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ClaimedPeriodicTask {
  /** The least delay a task waits when its interval cannot be read. */
  val MinimumFallbackDelay: FiniteDuration = 1.minute
}
