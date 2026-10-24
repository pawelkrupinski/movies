package tools

import play.api.Logger

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
 * One run of a scheduled tick whose failure must not cancel the schedule — and must not vanish
 * either. The schedulers used to wrap each tick in a bare `Try(...)`, which kept the next run
 * coming but dropped the exception unlogged: a reaper failing on every tick looked exactly like
 * one with nothing to do. Fatal errors and interrupts still escape, as they did through `Try`.
 *
 * [[recorded]] also tells a [[TickRecorder]] each run that RETURNED, which is what the
 * `ScheduledTickStale` alerts (infra/nix/files/monitoring/rules/scheduled-ticks.rules) read: a loop
 * whose scheduler died, whose tick hangs, or whose tick throws every time stops completing, and
 * nothing else it publishes says so.
 */
object ScheduledTick {
  def logged[A](name: String, logger: Logger)(tick: => A): Unit =
    try { tick; () }
    catch { case NonFatal(e) => logger.warn(s"$name tick failed: $e", e) }

  /** [[logged]], and `ticks.completed(name, interval)` when the run returned rather than threw. */
  def recorded[A](name: String, interval: => FiniteDuration, logger: Logger, ticks: TickRecorder)(tick: => A): Unit =
    try { tick; ticks.completed(name, interval) }
    catch { case NonFatal(e) => logger.warn(s"$name tick failed: $e", e) }
}

/**
 * Where a periodic loop says it is alive: `started` once its schedule is armed, its first run due
 * `firstRunIn` from now; `completed` after every run that returned. `interval` is how often the
 * loop means to run, so one alert can hold every loop to a few of its OWN periods. Neither call may
 * throw.
 */
trait TickRecorder {
  def started(tick: String, interval: FiniteDuration, firstRunIn: FiniteDuration): Unit
  def completed(tick: String, interval: FiniteDuration): Unit
}

object TickRecorder {
  /** Records nothing: for a spec, a tool, or a dev-only loop no alert watches. */
  val Unrecorded: TickRecorder = new TickRecorder {
    def started(tick: String, interval: FiniteDuration, firstRunIn: FiniteDuration): Unit = ()
    def completed(tick: String, interval: FiniteDuration): Unit = ()
  }
}
