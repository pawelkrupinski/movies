package services.metrics

import services.tasks.{EnqueueResult, QueueSnapshot, Task, TaskQueue, TaskType}

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/**
 * A [[TaskQueue]] decorator that meters every enqueue onto a
 * [[WorkerTaskMetrics]]. `enqueue` is the one place all of a worker's deferred
 * work enters the system (every reaper/enqueuer calls it on the shared queue),
 * so wrapping it once at the composition root captures the full inflow — both
 * the tasks actually added and the ones collapsed onto an active duplicate —
 * without threading the metrics object through every caller.
 *
 * The claim/finish side is metered separately, by [[WorkerTaskMetrics]] acting
 * as the worker's `TaskObserver`, because only the worker knows the handler
 * outcome (done vs skipped vs reschedule) that a queue-level decorator can't see.
 * Every other method is a straight pass-through.
 */
class MeteredTaskQueue(delegate: TaskQueue, metrics: WorkerTaskMetrics) extends TaskQueue {
  import WorkerTaskMetrics.EnqueueResult.{Added, Deduped}

  override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant): EnqueueResult = {
    val result = delegate.enqueue(taskType, dedupKey, payload, submittedAt)
    metrics.recordEnqueue(taskType, result match {
      case EnqueueResult.Added     => Added
      case EnqueueResult.Duplicate => Deduped
    })
    result
  }

  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = delegate.claim(workerId, lease, now)
  override def complete(id: String, workerId: String): Unit = delegate.complete(id, workerId)
  override def release(id: String, workerId: String, error: Option[String], notBefore: Option[Instant]): Unit =
    delegate.release(id, workerId, error, notBefore)
  override def reapExpiredLeases(now: Instant): Int = delegate.reapExpiredLeases(now)
  override def countByState(): Map[String, Long] = delegate.countByState()
  override def monitor(activeLimit: Int): QueueSnapshot = delegate.monitor(activeLimit)
  override def watchWaiting(onWaiting: () => Unit): Option[AutoCloseable] = delegate.watchWaiting(onWaiting)
  override def close(): Unit = delegate.close()
}
