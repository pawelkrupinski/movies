package services.tasks

import java.time.Instant
import java.util.UUID
import java.util.concurrent.CopyOnWriteArrayList
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

/**
 * In-memory `TaskQueue` for tests and Mongo-less dev. Mirrors
 * [[MongoTaskQueue]]'s semantics: per-dedupKey idempotent enqueue, and the lease
 * cycle — FIFO atomic claim, ownership-guarded complete/release, lease reaping —
 * from the same [[TaskLeases]] rules over its own rows. All
 * mutations are guarded by one monitor — fine at test/dev scale. What a task is written with — its payload and its
 * instants — is written as [[MongoTaskQueue]] inserts it to BSON and read back ([[services.BsonRoundTrip]]).
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
    leaseExpiresAt: Option[Instant],
    nextEligibleAt: Option[Instant] = None,
    enqueuedAt:     Option[Instant] = None,
    lastError:      Option[String]  = None
  ) {
    def matches(matching: TaskRowMatch): Boolean =
      matching.id.forall(_ == id) && matching.state.forall(_ == state) && matching.workerId.forall(workerId.contains) &&
        (!matching.knownTypesOnly || TaskType.all.contains(taskType)) &&
        matching.eligibleAt.forall(now => !nextEligibleAt.exists(_.isAfter(now))) &&
        matching.leaseExpiredBefore.forall(now => leaseExpiresAt.exists(_.isBefore(now)))

    def changed(change: TaskRowChange): Row = copy(
      state          = change.state,
      workerId       = change.lease.map(_.workerId),
      leaseExpiresAt = change.lease.map(_.expiresAt),
      nextEligibleAt = change.nextEligibleAt.getOrElse(nextEligibleAt),
      attempts       = attempts + change.attemptsBy,
      lastError      = change.lastError.orElse(lastError))
  }

  private val rows = scala.collection.mutable.LinkedHashMap.empty[String, Row]
  private val lock = new Object
  private val waitingListeners = new CopyOnWriteArrayList[Option[Instant] => Unit]()

  override def enqueue(
    taskType:    TaskType,
    dedupKey:    String,
    payload:     Map[String, String],
    submittedAt: Instant,
    notBefore:   Option[Instant],
    claimAhead:  FiniteDuration
  ): EnqueueResult = {
    val result = lock.synchronized {
      // `complete` removes the row, so any row still present with this key is active.
      val active = rows.values.exists(_.dedupKey == dedupKey)
      if (active) EnqueueResult.Duplicate
      else {
        val id     = UUID.randomUUID().toString
        val stored = services.BsonRoundTrip(MongoTaskQueue.inserted(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead))
        def instant(field: String) = stored.get(field).map(at => Instant.ofEpochMilli(at.asDateTime.getValue))
        rows.put(id, Row(id, taskType, dedupKey, MongoTaskQueue.payloadOf(stored), TaskState.Waiting,
          instant("submittedAt").get, 0, None, None, instant("nextEligibleAt"), instant("enqueuedAt")))
        EnqueueResult.Added
      }
    }
    // Ring outside the lock, and only for genuinely new work — a duplicate is a
    // no-op, exactly like MongoTaskQueue's insert-only change-stream signal.
    if (result == EnqueueResult.Added) waitingListeners.asScala.foreach(_(notBefore))
    result
  }

  override def raise(dedupKey: String, claimFrom: Instant): Boolean = lock.synchronized {
    rows.values.find(r => r.dedupKey == dedupKey && r.state == TaskState.Waiting).exists { r =>
      def earlier(at: Instant) = if (at.isAfter(claimFrom)) claimFrom else at
      // an eligibility already set later moves to `claimFrom`; none set is eligible already
      rows.put(r.id, r.copy(submittedAt = earlier(r.submittedAt), nextEligibleAt = r.nextEligibleAt.map(earlier))); true
    }
  }

  override def amendWaiting(dedupKey: String, fields: Map[String, String]): Boolean = lock.synchronized {
    // "Amended" means MATCHED, as Mongo's matched count reports it: a waiting task that
    // already carries `fields` still runs the caller's request.
    fields.nonEmpty && rows.values.find(r => r.dedupKey == dedupKey && r.state == TaskState.Waiting).exists { r =>
      val stored = services.BsonRoundTrip(org.mongodb.scala.Document("payload" -> MongoTaskQueue.payloadDocument(r.payload ++ fields)))
      rows.put(r.id, r.copy(payload = MongoTaskQueue.payloadOf(stored))); true
    }
  }

  // The rows as [[TaskRows]], under the one monitor: the lease cycle's rules are [[TaskLeases]]',
  // shared with [[MongoTaskQueue]].
  private object rowStore extends TaskRows {
    def claimOldest(matching: TaskRowMatch, change: TaskRowChange): Option[Task] = lock.synchronized {
      rows.values.filter(_.matches(matching)).toSeq.sortBy(_.submittedAt).headOption.map { r =>
        val claimed = r.changed(change)
        rows.put(r.id, claimed)
        Task(claimed.id, claimed.taskType, claimed.dedupKey, claimed.payload, claimed.attempts)
      }
    }
    def updateOne(matching: TaskRowMatch, change: TaskRowChange): Unit = lock.synchronized {
      rows.values.find(_.matches(matching)).foreach(r => rows.put(r.id, r.changed(change)))
    }
    def updateAll(matching: TaskRowMatch, change: TaskRowChange): Int = lock.synchronized {
      val matched = rows.values.filter(_.matches(matching)).toSeq
      matched.foreach(r => rows.put(r.id, r.changed(change)))
      matched.size
    }
    def deleteOne(matching: TaskRowMatch): Unit = lock.synchronized {
      rows.values.find(_.matches(matching)).foreach(r => rows.remove(r.id))
    }
  }

  private val leases = new TaskLeases(rowStore)

  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = leases.claim(workerId, lease, now)

  // Finishing removes the row outright (parity with MongoTaskQueue's delete-on-complete) — a
  // gone row allows re-enqueue just as an old tombstone did, and nothing lists finished tasks.
  override def complete(id: String, workerId: String): Unit = leases.complete(id, workerId)

  override def release(id: String, workerId: String, error: Option[String],
                       notBefore: Option[Instant], refundAttempt: Boolean): Unit =
    leases.release(id, workerId, error, notBefore, refundAttempt)

  override def reapExpiredLeases(now: Instant): Int = leases.reap(now)

  override def countByState(): Map[String, Long] = lock.synchronized {
    rows.values.groupBy(_.state).map { case (s, rs) => s -> rs.size.toLong }
  }

  override def waitingCount(taskType: TaskType): Int = lock.synchronized {
    rows.values.count(r => r.state == TaskState.Waiting && r.taskType == taskType)
  }

  override def monitor(activeLimit: Int): QueueSnapshot = lock.synchronized {
    val counts = rows.values.groupBy(_.state).map { case (s, rs) => s -> rs.size.toLong }
    val active = rows.values.toSeq
      .filter(r => TaskState.activeByPriority.contains(r.state))
      .sortBy(r => (TaskState.activeByPriority.indexOf(r.state), r.submittedAt))
      .take(activeLimit)
      .map(r => TaskSummary(r.id, r.taskType.name, r.dedupKey, r.state, r.submittedAt,
        r.attempts, r.workerId, r.leaseExpiresAt, r.lastError, r.nextEligibleAt, r.enqueuedAt))
    QueueSnapshot(counts, active)
  }

  /** Push parity with [[MongoTaskQueue]] for Mongo-less dev/tests: fire
   *  `onWaiting` whenever a fresh task is actually added (see `enqueue`). */
  override def watchWaiting(onWaiting: Option[Instant] => Unit): Option[AutoCloseable] = {
    waitingListeners.add(onWaiting)
    Some(new AutoCloseable { override def close(): Unit = { waitingListeners.remove(onWaiting); () } })
  }
}
