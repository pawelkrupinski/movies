package services.identity
package adapters

import play.api.Logging

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.duration._

/**
 * `inner`, written on a thread of its own: [[append]] only offers the entries to a bounded queue and returns — the
 * model's drain and the TMDB store's writes never wait on the history's Mongo. The writer takes
 * up to [[AsyncIdentityHistory.Batch]] at a time. A queue full (Mongo slow or down) DROPS the entries, counted by
 * `dropped`, rather than holding the caller: the history explains the system, it never slows it — a blocking insert per
 * write made Germany's convergence tick run past its 30-minute timeout (red #2301, 379dec67b). [[stop]] writes what is
 * queued, within its budget.
 *
 * Except the TAKE entries ([[AsyncIdentityHistory.Durable]]): those are written on the caller's thread, in batches of
 * [[AsyncIdentityHistory.DurableBatch]], before [[append]] returns — right after the projection's own film write. A
 * worker killed by an OOM or a SIGKILL runs no stop(), so whatever the queue held is lost, and a take lost there leaves
 * identity_history a gap exactly around the restart (flip-timeline, the replays, [[TakeReversals]]' boot seed). The
 * cost: one insert per projection that moved a take (p50 1 take, p99 34-149, a few ms; 2026-11-06). A take batch whose
 * insert throws is queued instead.
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
    val (durable, queued) = entries.partition(e => AsyncIdentityHistory.Durable(e.kind))
    val unwritten = durable.grouped(AsyncIdentityHistory.DurableBatch).flatMap { batch =>
      try { inner.append(batch); Nil }
      catch { case scala.util.control.NonFatal(e) => logger.warn(s"identity history: ${batch.size} take(s) queued, not written: $e"); batch }
    }.toSeq
    var lost = 0L
    (unwritten ++ queued).foreach(e => if (!queue.offer(e)) lost += 1)
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
  /** The kinds written before [[AsyncIdentityHistory.append]] returns: the takes, which a restart must not lose. */
  val Durable: Set[String] = Set(HistoryEntry.Take)
  /** Take entries one synchronous insert writes at most (a German take entry is ~24 kB: ~2.4 MB an insert). */
  val DurableBatch = 100
}
