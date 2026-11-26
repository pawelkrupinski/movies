package tools

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

import scala.concurrent.duration.*
import scala.util.control.NonFatal

import play.api.Logger

/**
 * A job run once per `interval`, first after `initialDelay`, on the `scheduler` it is handed: the one place the
 * periodic loops' scheduling, [[TickRecorder]] reporting and error handling live ([[ScheduledTick.recorded]]). A failing
 * run never stops the schedule. `run` says how a run went: one that caught its own failure is [[TickRun.Incomplete]] and
 * does not count as completed. The caller owns the scheduler and shuts it. Built by [[PeriodicLoop.fixedRate]] (the
 * scheduler's `scheduleAtFixedRate`, the interval read once at `start`) or [[PeriodicLoop.rereading]] (each run
 * schedules the next, reading `interval` afresh, so an `/admin/config` flip applies on the next cycle; what
 * `ClaimedPeriodicTask` rides).
 */
final class PeriodicLoop private (name: String, run: () => TickRun, interval: => FiniteDuration, initialDelay: FiniteDuration,
                                  ticks: TickRecorder, logger: Logger, scheduler: ScheduledExecutorService,
                                  pacing: PeriodicLoop.Pacing) {

  def start(): Unit = {
    ticks.started(name, interval, initialDelay)
    pacing match {
      case PeriodicLoop.Pacing.FixedRate  => armFixedRate()
      case PeriodicLoop.Pacing.Rereading  => scheduleNext(initialDelay)
    }
  }

  private def armFixedRate(): Unit = {
    val every = interval
    scheduler.scheduleAtFixedRate(() => ScheduledTick.recorded(name, every, logger, ticks)(run()),
      initialDelay.toMillis, every.toMillis, TimeUnit.MILLISECONDS)
    ()
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
  enum Pacing { case FixedRate, Rereading }

  /** The least delay a rereading loop waits when its interval cannot be read. */
  val MinimumFallbackDelay: FiniteDuration = 1.minute

  /** Runs on the scheduler's fixed rate; `interval` is read once, at `start`. */
  def fixedRate(name: String, run: () => TickRun, interval: FiniteDuration, initialDelay: FiniteDuration,
                ticks: TickRecorder, logger: Logger, scheduler: ScheduledExecutorService): PeriodicLoop =
    new PeriodicLoop(name, run, interval, initialDelay, ticks, logger, scheduler, Pacing.FixedRate)

  /** Each run schedules the next one `interval` (read afresh) later. */
  def rereading(name: String, run: () => TickRun, interval: => FiniteDuration, initialDelay: FiniteDuration,
                ticks: TickRecorder, logger: Logger, scheduler: ScheduledExecutorService): PeriodicLoop =
    new PeriodicLoop(name, run, interval, initialDelay, ticks, logger, scheduler, Pacing.Rereading)
}
