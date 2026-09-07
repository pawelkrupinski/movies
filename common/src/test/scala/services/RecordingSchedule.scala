package services

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

/** A `schedule` seam that records what was scheduled instead of sleeping; `fire()` runs
 *  every pending body. Stands in for the daemon scheduler behind
 *  [[services.movies.ChangeStreamReopen]] wherever a spec drives a reopen by hand. */
final class RecordingSchedule {
  val delays = mutable.Buffer.empty[FiniteDuration]
  private val queued = mutable.Queue.empty[() => Unit]
  val schedule: (FiniteDuration, () => Unit) => Unit = (d, run) => { delays += d; queued.enqueue(run) }
  def fire(): Unit = while (queued.nonEmpty) queued.dequeue().apply()
}
