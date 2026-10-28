package services.identity
package adapters

import play.api.Logging

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.duration._

/**
 * `inner`, written on a thread of its own: [[append]] only offers the entries to a bounded queue and returns — the
 * model's drain, the projection's takes and the TMDB store's writes never wait on the history's Mongo. The writer takes
 * up to [[AsyncIdentityHistory.Batch]] at a time. A queue full (Mongo slow or down) DROPS the entries, counted by
 * `dropped`, rather than holding the caller: the history explains the system, it never slows it — a blocking insert per
 * write made Germany's convergence tick run past its 30-minute timeout (red #2301, 379dec67b). [[stop]] writes what is
 * queued, within its budget.
 */
final class AsyncIdentityHistory(inner: IdentityHistory, dropped: Long => Unit, capacity: Int = AsyncIdentityHistory.Capacity)
    extends IdentityHistory with services.Stoppable with Logging {
  private val queue   = new ArrayBlockingQueue[HistoryEntry](capacity)
  @volatile private var running = true
  private val writer  = {
    val t = new Thread(() => while (running || !queue.isEmpty) writeSome(), "identity-history-writer")
    t.setDaemon(true); t.start(); t
  }

  def append(entries: Seq[HistoryEntry]): Unit = {
    var lost = 0L
    entries.foreach(e => if (!queue.offer(e)) lost += 1)
    if (lost > 0) dropped(lost)
  }

  private def writeSome(): Unit = {
    val first = queue.poll(200, java.util.concurrent.TimeUnit.MILLISECONDS)
    if (first != null) {
      val batch = new java.util.ArrayList[HistoryEntry](AsyncIdentityHistory.Batch)
      batch.add(first)
      queue.drainTo(batch, AsyncIdentityHistory.Batch - 1)
      try inner.append(scala.jdk.CollectionConverters.ListHasAsScala(batch).asScala.toSeq)
      catch { case scala.util.control.NonFatal(e) => logger.warn(s"identity history: ${batch.size} entr(ies) not written: $e") }
    }
  }

  /** How many entries wait to be written. */
  def queued: Int = queue.size

  def stop(): Unit = stopWithin(AsyncIdentityHistory.StopBudget)
  override def stopWithin(budget: FiniteDuration): Unit = {
    running = false
    writer.join(budget.toMillis.max(1L))
  }
}

object AsyncIdentityHistory {
  /** Entries held at most while the writer catches up: a take-up's worth of listing moves, a few MB. */
  val Capacity = 50000
  /** Entries one insert writes at most. */
  val Batch = 1000
  val StopBudget: FiniteDuration = 10.seconds
}
