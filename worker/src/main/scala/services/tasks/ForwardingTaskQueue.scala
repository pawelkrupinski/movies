package services.tasks

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/**
 * A [[TaskQueue]] that hands every call to `delegate` — the base of a decorator,
 * which overrides only the calls it decorates (metering, dedup caching, re-ask
 * counting) and inherits a straight pass-through for the rest. A method added to
 * [[TaskQueue]] later is forwarded here once, instead of silently taking the
 * trait's default in every decorator that forgot it.
 */
abstract class ForwardingTaskQueue(delegate: TaskQueue) extends TaskQueue {
  override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant,
                       notBefore: Option[Instant], claimAhead: FiniteDuration): EnqueueResult =
    delegate.enqueue(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead)
  override def amendWaiting(dedupKey: String, fields: Map[String, String]): Boolean = delegate.amendWaiting(dedupKey, fields)
  override def raise(dedupKey: String, claimFrom: Instant): Boolean = delegate.raise(dedupKey, claimFrom)
  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = delegate.claim(workerId, lease, now)
  override def complete(id: String, workerId: String): Unit = delegate.complete(id, workerId)
  override def release(id: String, workerId: String, error: Option[String], notBefore: Option[Instant],
                       refundAttempt: Boolean): Unit =
    delegate.release(id, workerId, error, notBefore, refundAttempt)
  override def reapExpiredLeases(now: Instant): Int = delegate.reapExpiredLeases(now)
  override def countByState(): Map[String, Long] = delegate.countByState()
  override def waitingCount(taskType: TaskType): Int = delegate.waitingCount(taskType)
  override def monitor(activeLimit: Int): QueueSnapshot = delegate.monitor(activeLimit)
  override def watchWaiting(onWaiting: Option[Instant] => Unit): Option[AutoCloseable] = delegate.watchWaiting(onWaiting)
  override def close(): Unit = delegate.close()
}
