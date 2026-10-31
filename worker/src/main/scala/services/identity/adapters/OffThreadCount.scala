package services.identity
package adapters

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/**
 * A count of each written projection made on `runner`, never on the projection's path: [[submit]] hands it the latest
 * projection and goes on, only the latest submission waits (one submitted while another waits replaces it), and none
 * is counted while `pressed` (the heap held near its limit after its last collection): the next submission counts. It
 * runs under the submitting projection's log fields; a failing count is logged and the next one counts again.
 * What [[SuspectTakeCount]] and [[RuleFiringCount]] count with, under `name` in their log lines.
 */
final class OffThreadCount[A](name: String, logger: play.api.Logger, runner: Executor, pressed: () => Boolean)(count: A => Unit) {

  /** The latest submission waiting: what to count, and the log fields of the projection that submitted it. */
  private val waiting = new AtomicReference[Option[(A, Map[String, String])]](None)

  /** Count `value` on `runner`, once it is free. */
  def submit(value: A): Unit =
    if (waiting.getAndSet(Some((value, tools.LogFields.current()))).isEmpty) runner.execute(() => countWaiting())

  private def countWaiting(): Unit = synchronized {
    waiting.getAndSet(None).foreach { case (value, fields) => tools.LogFields.carrying(fields) {
      if (pressed()) logger.info(s"$name: not counted, the heap is near its limit; the next projection of the corpus counts")
      else try count(value)
      catch { case NonFatal(e) => logger.warn(s"$name: counting failed", e) }
    } }
  }
}
