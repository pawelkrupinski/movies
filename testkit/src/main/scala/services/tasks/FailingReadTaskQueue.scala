package services.tasks

import tools.contracts.FailsOnPurpose

/** A [[TaskQueue]] whose READS throw — the counts and the monitoring snapshot — while
 *  enqueue, claim and the rest still work on the in-memory store, so a spec can seed a
 *  queue and then blind only its readers. The shape an unreachable Mongo gives
 *  `MongoTaskQueue` now that its reads propagate (eebd3eef9): a caller holding one must
 *  report "unknown", never a depth of 0. */
class FailingReadTaskQueue(failure: => Throwable = new RuntimeException("task queue unreadable"))
  extends InMemoryTaskQueue with FailsOnPurpose {
  override def countByState(): Map[String, Long]           = throw failure
  override def waitingCount(taskType: TaskType): Int        = throw failure
  override def monitor(activeLimit: Int): QueueSnapshot     = throw failure
}
