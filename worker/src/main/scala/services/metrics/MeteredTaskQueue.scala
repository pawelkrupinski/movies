package services.metrics

import services.tasks.{EnqueueResult, ForwardingTaskQueue, TaskQueue, TaskType}

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
 * Every other method is [[ForwardingTaskQueue]]'s straight pass-through.
 */
class MeteredTaskQueue(delegate: TaskQueue, metrics: WorkerTaskMetrics) extends ForwardingTaskQueue(delegate) {
  import WorkerTaskMetrics.EnqueueResult.{Added, Deduped, Failed}

  override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant, notBefore: Option[Instant], claimAhead: FiniteDuration): EnqueueResult = {
    val result = delegate.enqueue(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead)
    metrics.recordEnqueue(taskType, result match {
      case EnqueueResult.Added     => Added
      case EnqueueResult.Duplicate => Deduped
      case EnqueueResult.Failed(_) => Failed
    })
    result
  }
}
