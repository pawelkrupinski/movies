package modules

import play.api.Logging
import tools.Stopwatch

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/**
 * A worker boot's steps, each timed, and the slow ones run off the boot path.
 *
 * A US restart took 60–97 s to start its TaskWorker and 128–164 s to log "Worker up", and the log
 * could not say where: the read-model projector's seed ran 28–57 s with no line at all. Each step
 * is now timed and the boot ends with one line naming what each cost — so the next slow step is
 * read off the log, not guessed.
 *
 * `inBackground` runs a step on its own daemon thread and moves on: for a step nothing after it
 * in the boot needs, whose own work can catch up (the projector resumes its change stream from a
 * persisted token, so a write made while it seeds is replayed, not lost).
 */
final class BootSteps(scope: String, stopwatch: Stopwatch = Stopwatch.System) extends Logging {
  private val timings = mutable.ArrayBuffer.empty[(String, FiniteDuration)]

  def step[A](name: String)(body: => A): A = {
    val timed = stopwatch.timed(body)
    synchronized(timings += name -> timed.elapsed)
    timed.value
  }

  def inBackground(name: String)(body: => Unit): Thread = {
    val thread = new Thread(() => {
      val started = stopwatch.start()
      try { body; logger.info(f"[$scope] boot step $name (background) done in ${started.seconds}%.1fs") }
      catch { case scala.util.control.NonFatal(e) => logger.error(s"[$scope] boot step $name (background) failed: ${e.getMessage}", e) }
    }, s"boot-$scope-${name.replace(' ', '-')}")
    thread.setDaemon(true)
    thread.start()
    thread
  }

  /** Every foreground step, slowest first, and their total. */
  def summary: String = synchronized {
    val total = timings.iterator.map(_._2.toNanos).sum / 1e9
    val slowest = timings.sortBy(-_._2.toNanos).take(8).map { case (name, took) => f"$name ${took.toNanos / 1e9}%.1fs" }
    f"[$scope] boot steps took $total%.1fs: ${slowest.mkString(", ")}"
  }
}
