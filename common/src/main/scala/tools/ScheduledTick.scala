package tools

import play.api.Logger

import scala.util.control.NonFatal

/**
 * One run of a scheduled tick whose failure must not cancel the schedule — and must not vanish
 * either. The schedulers used to wrap each tick in a bare `Try(...)`, which kept the next run
 * coming but dropped the exception unlogged: a reaper failing on every tick looked exactly like
 * one with nothing to do. Fatal errors and interrupts still escape, as they did through `Try`.
 */
object ScheduledTick {
  def logged[A](name: String, logger: Logger)(tick: => A): Unit =
    try { tick; () }
    catch { case NonFatal(e) => logger.warn(s"$name tick failed: $e", e) }
}
