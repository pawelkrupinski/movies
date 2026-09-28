package services.observations

import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Await, Promise, TimeoutException}
import scala.concurrent.duration._
import scala.util.Try

/**
 * `inner` with its `current` reads coalesced: a read queues its key, and whichever reader finds no
 * batch in flight fetches every queued key in one `currents` round-trip, again until none is
 * queued. Many concurrent readers — the identity model's prefetch — cost one round-trip per batch,
 * not one each, over one connection; a lone reader leads its own batch of one, at no added latency.
 * Everything but `current` is `inner`'s.
 */
final class CoalescedObservationBackend(inner: ObservationBackend, maxBatch: Int = CoalescedObservationBackend.MaxBatch)
    extends ObservationBackend {

  private val queued  = new ConcurrentLinkedQueue[(String, Promise[Option[StoredObservation]])]()
  private val leading = new AtomicBoolean(false)

  def current(key: String): Option[StoredObservation] = {
    val read = Promise[Option[StoredObservation]]()
    queued.add(key -> read)
    while (!read.isCompleted) {
      if (leading.compareAndSet(false, true)) try fetchQueued() finally leading.set(false)
      // A reader queued just as the leader stopped leads the next batch itself, at the next check.
      else try Await.ready(read.future, CoalescedObservationBackend.Recheck) catch { case _: TimeoutException => () }
    }
    read.future.value.get.get
  }

  private def fetchQueued(): Unit = while (!queued.isEmpty) {
    val batch = Iterator.continually(queued.poll()).takeWhile(_ != null).take(maxBatch).toSeq
    Try(inner.currents(batch.map(_._1).distinct)).fold(
      failed => batch.foreach(_._2.tryFailure(failed)),
      found  => batch.foreach { case (key, read) => read.trySuccess(found.get(key)) })
  }

  override def currents(keys: Seq[String]): Map[String, StoredObservation] = inner.currents(keys)
  def history(key: String): Seq[StoredObservation]                         = inner.history(key)
  def allCurrent(): Seq[StoredObservation]                                 = inner.allCurrent()
  def insert(observation: StoredObservation): Unit                        = inner.insert(observation)
  def retire(key: String, expireAt: Instant): Unit                        = inner.retire(key, expireAt)
  def renew(key: String, lastSeenAt: Option[Instant], expireAt: Instant): Unit = inner.renew(key, lastSeenAt, expireAt)
  override def close(): Unit                                              = inner.close()
}

object CoalescedObservationBackend {
  /** The most keys one round-trip asks for: an `$in` of this many ids stays a small query. */
  val MaxBatch = 256
  /** How soon a waiting reader looks again whether its key still needs a leader. */
  private val Recheck = FiniteDuration(2, TimeUnit.MILLISECONDS)
}
