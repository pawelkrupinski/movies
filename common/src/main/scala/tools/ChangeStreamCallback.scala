package tools

/**
 * Work done inside a change-stream observer's `onNext`, the Mongo driver's own callback: it must not throw
 * (Reactive Streams 2.13), and a throw out of it ends the cursor. So whatever the work throws is handed to `failed`
 * (which logs it) and goes no further, a fatal error included -- the one place outside `tools.AnyThrowable` that
 * means to stop one: a cursor ended by it leaves every later change unseen until a reload or a restart, while the
 * process (`-XX:+ExitOnOutOfMemoryError`) still ends on an OutOfMemoryError by itself. An interrupt keeps its flag
 * for whoever owns the thread (`captured` restores it).
 */
object ChangeStreamCallback {

  def guarded(failed: Throwable => Unit)(work: => Unit): Unit =
    tools.AnyThrowable.captured(work).left.foreach(failed)
}
