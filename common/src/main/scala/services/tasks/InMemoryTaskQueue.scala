package services.tasks

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

/**
 * In-memory `TaskQueue` for tests and Mongo-less dev. Mirrors
 * [[MongoTaskQueue]]'s semantics exactly: per-dedupKey idempotent enqueue,
 * FIFO atomic claim, ownership-guarded complete/release, lease reaping. All
 * mutations are guarded by one monitor — fine at test/dev scale.
 */
class InMemoryTaskQueue extends TaskQueue {

  private case class Row(
    id:             String,
    taskType:       TaskType,
    dedupKey:       String,
    payload:        Map[String, String],
    state:          String,
    submittedAt:    Instant,
    attempts:       Int,
    workerId:       Option[String],
    leaseExpiresAt: Option[Instant]
  )

  private val rows = scala.collection.mutable.LinkedHashMap.empty[String, Row]
  private val lock = new Object

  override def enqueue(
    taskType:    TaskType,
    dedupKey:    String,
    payload:     Map[String, String],
    submittedAt: Instant
  ): EnqueueResult = lock.synchronized {
    val active = rows.values.exists(r => r.dedupKey == dedupKey && r.state != TaskState.Deleted)
    if (active) EnqueueResult.Duplicate
    else {
      val id = UUID.randomUUID().toString
      rows.put(id, Row(id, taskType, dedupKey, payload, TaskState.Waiting, submittedAt, 0, None, None))
      EnqueueResult.Added
    }
  }

  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = lock.synchronized {
    rows.values
      .filter(_.state == TaskState.Waiting)
      .toSeq
      .sortBy(_.submittedAt)
      .headOption
      .map { r =>
        val claimed = r.copy(
          state          = TaskState.WorkedOn,
          attempts       = r.attempts + 1,
          workerId       = Some(workerId),
          leaseExpiresAt = Some(now.plusMillis(lease.toMillis))
        )
        rows.put(r.id, claimed)
        Task(claimed.id, claimed.taskType, claimed.dedupKey, claimed.payload, claimed.attempts)
      }
  }

  override def complete(id: String, workerId: String): Unit = lock.synchronized {
    rows.get(id).foreach { r =>
      if (r.state == TaskState.WorkedOn && r.workerId.contains(workerId))
        rows.put(id, r.copy(state = TaskState.Deleted, workerId = None, leaseExpiresAt = None))
    }
  }

  override def release(id: String, workerId: String, error: Option[String]): Unit = lock.synchronized {
    rows.get(id).foreach { r =>
      if (r.state == TaskState.WorkedOn && r.workerId.contains(workerId))
        rows.put(id, r.copy(state = TaskState.Waiting, workerId = None, leaseExpiresAt = None))
    }
  }

  override def reapExpiredLeases(now: Instant): Int = lock.synchronized {
    var n = 0
    rows.values.toSeq.foreach { r =>
      if (r.state == TaskState.WorkedOn && r.leaseExpiresAt.exists(!_.isAfter(now))) {
        rows.put(r.id, r.copy(state = TaskState.Waiting, workerId = None, leaseExpiresAt = None))
        n += 1
      }
    }
    n
  }

  override def countByState(): Map[String, Long] = lock.synchronized {
    rows.values.groupBy(_.state).map { case (s, rs) => s -> rs.size.toLong }
  }

  override def monitor(activeLimit: Int): QueueSnapshot = lock.synchronized {
    val counts = rows.values.groupBy(_.state).map { case (s, rs) => s -> rs.size.toLong }
    val active = rows.values.toSeq
      .filter(r => r.state == TaskState.Waiting || r.state == TaskState.WorkedOn)
      .sortBy(_.submittedAt)
      .take(activeLimit)
      .map(r => TaskSummary(r.id, r.taskType.name, r.dedupKey, r.state, r.submittedAt,
        r.attempts, r.workerId, r.leaseExpiresAt, None))
    QueueSnapshot(counts, active)
  }
}
