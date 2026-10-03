package tools

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Threads a spec's subject started and did not end: what a composition root's `stop()` must leave none of. */
object ThreadLeaks {
  /** Every live thread now. */
  def live(): Set[Thread] = Thread.getAllStackTraces.keySet.asScala.toSet

  /** The non-daemon threads started since `before` that are still alive once each has had `grace` to end
   *  (joined, not slept on: a thread that ends at once costs nothing). */
  def survivors(before: Set[Thread], grace: FiniteDuration = 5.seconds): Seq[String] = {
    val started = (live() -- before).filterNot(_.isDaemon)
    started.foreach(_.join(grace.toMillis))
    started.filter(_.isAlive).map(_.getName).toSeq.sorted
  }
}
