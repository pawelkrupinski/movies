package tools

import scala.concurrent.duration.FiniteDuration

/**
 * The blocking sleep a wait or a back-off is built on. A class that waits takes `sleep: FiniteDuration => Unit` (and,
 * when it bounds the wait, a [[Stopwatch]]) from its composition root, so a spec passes a sleep that steps its
 * stopwatch or records the delay instead of spending real time. [[Sleep.Blocking]] is the process's: the one
 * production `Thread.sleep`, which only the roots hand down (`NoUnwiredThreadsAndWaitsSpec`).
 */
object Sleep {
  /** `Thread.sleep` for `duration`; an interrupt ends it with an `InterruptedException`, as `Thread.sleep` does. */
  val Blocking: FiniteDuration => Unit = duration => Thread.sleep(duration.toMillis)
}
