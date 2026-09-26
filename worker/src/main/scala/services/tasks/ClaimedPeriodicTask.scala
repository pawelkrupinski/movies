package services.tasks

import play.api.Logging
import services.Stoppable
import services.schedule.{OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * A job run once per `interval` window, first after `initialDelay`, on its own daemon thread —
 * and, on a multi-machine worker, only by the machine that wins the window's occurrence claim
 * (`ScheduledRunStore`, keyed `name`). Self-rescheduling: each tick reads `interval` afresh, so an
 * `/admin/config` flip applies on the next cycle without a restart. A failing run never stops the
 * schedule. The settle and the identity shadow run each ride their own.
 */
class ClaimedPeriodicTask(name: String, run: () => Unit, interval: => FiniteDuration, initialDelay: FiniteDuration,
                          runStore: ScheduledRunStore, clock: Clock) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler(name)

  def start(): Unit = {
    scheduleNext(initialDelay)
    logger.info(s"$name started: once per ${interval.toSeconds}s, first in ${initialDelay.toSeconds}s.")
  }

  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(interval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Run only if this machine wins the current window's claim. True when it ran. */
  def tickIfClaimed(): Boolean = {
    val key = OccurrenceKey.at(name, clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) { run(); true } else false
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
