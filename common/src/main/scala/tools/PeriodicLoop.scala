package tools

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

import scala.concurrent.duration.*
import scala.util.control.NonFatal

import play.api.Logger

/**
 * A job run once per `interval`, first after `initialDelay`, on the `scheduler` it is handed: the one place the
 * periodic loops' scheduling, [[TickRecorder]] reporting and error handling live ([[ScheduledTick.recorded]]).
 * Self-rescheduling: each run reads `interval` afresh, so an `/admin/config` flip applies on the next cycle, and a
 * failing run (or an unreadable interval) never stops the schedule. `run` says how a run went: one that caught its own
 * failure is [[TickRun.Incomplete]] and does not count as completed. The caller owns the scheduler and shuts it.
 * `ClaimedPeriodicTask` is this loop whose run is gated by an occurrence claim.
 */
final class PeriodicLoop(name: String, run: () => TickRun, interval: => FiniteDuration, initialDelay: FiniteDuration,
                         ticks: TickRecorder, logger: Logger, scheduler: ScheduledExecutorService) {

  def start(): Unit = {
    ticks.started(name, interval, initialDelay)
    scheduleNext(initialDelay)
  }

  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = {
        ScheduledTick.recorded(name, intervalOr(delay), logger, ticks)(PeriodicLoop.this.run())
        scheduleNext(intervalOr(delay))
      }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** The next run's delay: `interval` read afresh, or — when it cannot be read — the last delay,
   *  never under [[PeriodicLoop.MinimumFallbackDelay]], said out loud. Read bare, an
   *  `interval` that threw ended the schedule for good, silently; falling back to a first run's
   *  zero initial delay re-ran the tick back to back, forever. */
  private def intervalOr(lastDelay: FiniteDuration): FiniteDuration =
    try interval
    catch {
      case NonFatal(e) =>
        val fallback = lastDelay.max(PeriodicLoop.MinimumFallbackDelay)
        logger.warn(s"$name: interval unreadable, next run in ${fallback.toMillis}ms: $e", e); fallback
    }
}

object PeriodicLoop {
  /** The least delay a loop waits when its interval cannot be read. */
  val MinimumFallbackDelay: FiniteDuration = 1.minute

  /** A loop whose run returns nothing of its own: every run that returns is a completed one. */
  def completing(name: String, run: () => Unit, interval: => FiniteDuration, initialDelay: FiniteDuration,
                 ticks: TickRecorder, logger: Logger, scheduler: ScheduledExecutorService): PeriodicLoop =
    new PeriodicLoop(name, () => { run(); TickRun.Completed }, interval, initialDelay, ticks, logger, scheduler)
}
