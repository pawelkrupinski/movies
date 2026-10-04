package tools

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Threads a spec's subject started and did not end: what a composition root's `stop()` must leave none of. */
object ThreadLeaks {

  /** The non-daemon threads `body` started that are still alive once each has had `grace` to end (joined, not slept
   *  on: a thread that ends at once costs nothing). `body` runs on a thread of a group of its own, which every thread
   *  it starts inherits — and every pool thread a factory it built starts later — so a thread another suite starts in
   *  the same JVM meanwhile is not taken for one of its own: told apart only by "started since", a parallel suite's
   *  `pool-47-thread-1` failed WorkerWiringLifecycleSpec on main (run 37169457102). Throws what `body` throws. */
  def of(body: => Unit, grace: FiniteDuration = 5.seconds): Seq[String] = {
    val group   = new ThreadGroup("thread-leaks")
    var failure = Option.empty[Throwable]
    val runner  = new Thread(group, () => try body catch { case e: Throwable => failure = Some(e) }, "thread-leaks-body")
    runner.start()
    runner.join()
    failure.foreach(throw _)
    val started = Thread.getAllStackTraces.keySet.asScala.toSeq
      .filter(t => !t.isDaemon && t != runner && Option(t.getThreadGroup).exists(g => g == group || group.parentOf(g)))
    started.foreach(_.join(grace.toMillis))
    started.filter(_.isAlive).map(_.getName).sorted
  }
}
