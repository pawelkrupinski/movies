package services.observations

import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore, TimeUnit}
import scala.concurrent.{Await, Promise, TimeoutException}
import scala.concurrent.duration._
import scala.util.Try

/**
 * `inner` with its `current` reads coalesced: a read queues its key, and a reader that finds a
 * batch slot free fetches up to `maxBatch` queued keys in one `currents` round-trip. Many
 * concurrent readers — the identity model's prefetch — cost one round-trip per batch, not one
 * each, with at most `inFlight` batches (connections) at once; a lone reader leads its own batch of
 * one, at no added latency. Everything but `current` is `inner`'s.
 *
 * Several batches, not one: a take-up profiled with ONE in flight left its 64 readers mostly
 * waiting while the store was far from busy.
 */
final class CoalescedObservationBackend(inner: ObservationBackend, maxBatch: Int = CoalescedObservationBackend.MaxBatch,
                                        inFlight: Int = CoalescedObservationBackend.InFlight)
    extends ObservationBackend {

  private val queued  = new ConcurrentLinkedQueue[(String, Promise[Option[StoredObservation]])]()
  private val slots  = new Semaphore(inFlight)

  def current(key: String): Option[StoredObservation] = {
    val read = Promise[Option[StoredObservation]]()
    queued.add(key -> read)
    while (!read.isCompleted) {
      if (!queued.isEmpty && slots.tryAcquire()) try fetchBatch() finally slots.release()
      // A reader whose key another batch took, or that found every slot taken, looks again shortly.
      else try Await.ready(read.future, CoalescedObservationBackend.Recheck) catch { case _: TimeoutException => () }
    }
    read.future.value.get.get
  }

  private def fetchBatch(): Unit = {
    val batch = Iterator.continually(queued.poll()).takeWhile(_ != null).take(maxBatch).toSeq
    if (batch.nonEmpty) Try(inner.currents(batch.map(_._1).distinct)).fold(
      failed => batch.foreach(_._2.tryFailure(failed)),
      found  => batch.foreach { case (key, read) => read.trySuccess(found.get(key)) })
  }

  override def currents(keys: Seq[String]): Map[String, StoredObservation] = inner.currents(keys)
  def history(key: String): Seq[StoredObservation]                         = inner.history(key)
  def allCurrent(): Seq[StoredObservation]                                 = inner.allCurrent()
  override def eachCurrent(keyPrefix: Option[String])(page: Seq[StoredObservation] => Unit): Unit = inner.eachCurrent(keyPrefix)(page)
  def insert(observation: StoredObservation): Unit                        = inner.insert(observation)
  def retire(key: String, expireAt: Instant): Unit                        = inner.retire(key, expireAt)
  def renew(key: String, lastSeenAt: Option[Instant], expireAt: Instant): Unit = inner.renew(key, lastSeenAt, expireAt)
  override def close(): Unit                                              = inner.close()
}

object CoalescedObservationBackend {
  /** The most keys one round-trip asks for: an `$in` of this many ids stays a small query. */
  val MaxBatch = 256
  /** How many batches may be in flight at once: each holds one of the worker's pooled connections. */
  val InFlight = 4
  /** How soon a waiting reader looks again whether its key still needs a leader. */
  private val Recheck = FiniteDuration(2, TimeUnit.MILLISECONDS)
}
