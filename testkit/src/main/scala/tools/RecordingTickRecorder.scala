package tools

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

/** A [[TickRecorder]] that keeps every completion it is told of, for a spec to read: which loop runs counted as done. */
final class RecordingTickRecorder extends TickRecorder {
  private val completions = new java.util.concurrent.ConcurrentLinkedQueue[(String, FiniteDuration)]

  def started(tick: String, interval: FiniteDuration, firstRunIn: FiniteDuration): Unit = ()
  def completed(tick: String, interval: FiniteDuration): Unit = { completions.add(tick -> interval); () }

  /** Every completion so far, oldest first. */
  def completedRuns: Seq[(String, FiniteDuration)] = completions.asScala.toList
}
