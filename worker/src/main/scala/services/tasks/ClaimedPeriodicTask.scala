package services.tasks

import java.time.Clock
import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.duration.*

import play.api.Logging
import services.schedule.{OccurrenceKey, ScheduledRunStore}
import tools.Stoppable
import tools.{PeriodicLoop, ThreadPools, TickRecorder, TickRun}

/**
 * A job run once per `interval` window, first after `initialDelay`, on its own daemon thread —
 * and, on a multi-machine worker, only by the machine that wins the window's occurrence claim
 * (`ScheduledRunStore`, keyed `name`). Self-rescheduling: each tick reads `interval` afresh, so an
 * `/admin/config` flip applies on the next cycle without a restart. A failing run never stops the
 * schedule. `run` says how a run went: one that caught its own failure is [[TickRun.Incomplete]], and does not count
 * as completed. The settle and the identity shadow run each ride their own.
 */
class ClaimedPeriodicTask(name: String, run: () => TickRun, interval: => FiniteDuration, initialDelay: FiniteDuration,
                          runStore: ScheduledRunStore, clock: Clock,
                          ticks: TickRecorder,
                          // Where the task's scheduler comes from (named after it); a spec hands in a stepped one.
                          threadPools: ThreadPools)
    extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = threadPools.scheduler(name)

  private val loop = new PeriodicLoop(name, () => tickIfClaimed().getOrElse(TickRun.Completed), interval, initialDelay,
    ticks, logger, scheduler)

  def start(): Unit = {
    loop.start()
    logger.info(s"$name started: once per ${interval.toSeconds}s, first in ${initialDelay.toSeconds}s.")
  }

  /** Run only if this machine wins the current window's claim: how the run went, or `None` when another replica holds
   *  the window (a no-op, which completes). */
  def tickIfClaimed(): Option[TickRun] = {
    val key = OccurrenceKey.at(name, clock.millis(), interval, 0.seconds)
    Option.when(runStore.claim(key))(run())
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
