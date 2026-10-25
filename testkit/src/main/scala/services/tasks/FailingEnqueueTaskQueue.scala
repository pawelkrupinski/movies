package services.tasks

import tools.contracts.FailsOnPurpose

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** A [[TaskQueue]] whose enqueues, while [[failing]], come back [[EnqueueResult.Failed]] — the answer `MongoTaskQueue`
 *  gives a write Mongo could not take — and otherwise land on the in-memory store, so a spec can fail a periodic
 *  enqueuer's writes and then let them through. */
class FailingEnqueueTaskQueue extends InMemoryTaskQueue with FailsOnPurpose {
  @volatile var failing: Boolean = true

  override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant,
                       notBefore: Option[Instant], claimAhead: FiniteDuration): EnqueueResult =
    if (failing) EnqueueResult.Failed("task queue unwritable")
    else super.enqueue(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead)
}
