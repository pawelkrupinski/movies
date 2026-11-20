package services.tasks

import java.time.Instant

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

import play.api.Logging

/** Which task rows a lease rule applies to: every condition given must hold. The rules state their
 *  conditions as this data so the Mongo store translates them to a filter and the in-memory store
 *  evaluates them — neither decides which rows a rule reaches. */
final case class TaskRowMatch(
  id:                 Option[String]  = None,
  state:              Option[String]  = None,
  // In one of these states (none given: any) -- an index-served condition, unlike reading every state and dropping some.
  states:             Seq[String]     = Nil,
  workerId:           Option[String]  = None,
  // Only types this build has a decoder for: a row of any other is, on a rollback, a newer image's.
  knownTypesOnly:     Boolean         = false,
  // Not held back by a backoff window still after this instant (a row with none is eligible).
  eligibleAt:         Option[Instant] = None,
  // Leased until strictly before this instant.
  leaseExpiredBefore: Option[Instant] = None
)

/** Who holds a worked-on task, and until when. */
final case class TaskLease(workerId: String, expiresAt: Instant)

/** How a lease rule rewrites the rows it matched: the new state, the lease (`None` clears it),
 *  the backoff window (`None` leaves it, `Some(None)` clears it), what to add to `attempts`, and
 *  the error to record (none leaves the last one). */
final case class TaskRowChange(
  state:          String,
  lease:          Option[TaskLease],
  nextEligibleAt: Option[Option[Instant]] = None,
  attemptsBy:     Int                     = 0,
  lastError:      Option[String]          = None
)

/**
 * The storage half of a task queue's lease cycle: rows matched by a [[TaskRowMatch]] and
 * rewritten by a [[TaskRowChange]]. Pure storage — which rows a claim, release or reap reaches,
 * and what it does to them, are [[TaskLeases]]'. Every method throws on a failed read or write.
 */
trait TaskRows {
  /** Atomically rewrite the matching row with the oldest `submittedAt`, answering it as rewritten;
   *  None when no row matches, or the row's type is not one this build knows. */
  def claimOldest(matching: TaskRowMatch, change: TaskRowChange): Option[Task]
  /** Rewrite one matching row, if any. */
  def updateOne(matching: TaskRowMatch, change: TaskRowChange): Unit
  /** Rewrite every matching row; answers how many changed. */
  def updateAll(matching: TaskRowMatch, change: TaskRowChange): Int
  /** Remove one matching row, if any. */
  def deleteOne(matching: TaskRowMatch): Unit
  /** How many rows match, by their state. */
  def countByState(matching: TaskRowMatch): Map[String, Long]
  /** Up to `limit` matching rows, the oldest `submittedAt` first. */
  def oldest(matching: TaskRowMatch, limit: Int): Seq[TaskSummary]
}

object TaskRows {
  /** Holds nothing: the rows where there is no database. */
  val absent: TaskRows = new TaskRows {
    def claimOldest(matching: TaskRowMatch, change: TaskRowChange): Option[Task] = None
    def updateOne(matching: TaskRowMatch, change: TaskRowChange): Unit           = ()
    def updateAll(matching: TaskRowMatch, change: TaskRowChange): Int            = 0
    def deleteOne(matching: TaskRowMatch): Unit                                  = ()
    def countByState(matching: TaskRowMatch): Map[String, Long]                  = Map.empty
    def oldest(matching: TaskRowMatch, limit: Int): Seq[TaskSummary]             = Seq.empty
  }
}

/**
 * A task queue's lease cycle — claim, complete, release, reap — as ONE set of rules over
 * [[TaskRows]], shared by [[MongoTaskQueue]] and [[InMemoryTaskQueue]] so the two differ only in
 * where the rows live: claim order (oldest `submittedAt`), the attempt each claim charges and a
 * refund gives back, the backoff hold, the ownership guard, when a lease has expired, and what a
 * failed store call is worth.
 */
final class TaskLeases(rows: TaskRows) extends Logging {
  import TaskLeases.*

  /** [[TaskConsumer.claim]]: the oldest waiting task of a known type no backoff holds back, leased
   *  to `workerId` and charged an attempt. A failed claim is logged and claims nothing. */
  def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = {
    val leased = TaskRowChange(TaskState.WorkedOn, Some(TaskLease(workerId, now.plusMillis(lease.toMillis))), attemptsBy = 1)
    Try(rows.claimOldest(claimable(now), leased)).recover { case exception =>
      logger.warn(s"TaskQueue.claim failed: ${exception.getMessage}")
      None
    }.get
  }

  /** [[TaskConsumer.complete]]: the task is removed, while `workerId` still holds it. */
  def complete(id: String, workerId: String): Unit =
    Try(rows.deleteOne(heldBy(id, workerId)))
      .recover { case exception => logger.warn(s"TaskQueue.complete($id) failed: ${exception.getMessage}") }
      .get

  /** [[TaskConsumer.release]]: back to waiting, held until `notBefore` (a stale hold cleared without
   *  one), the claim's attempt given back on `refundAttempt`, while `workerId` still holds it. The
   *  refund cannot go negative: `claim` charged at least one before the handler ran, and the
   *  ownership guard no-ops a release of a task since reaped and reclaimed. */
  def release(id: String, workerId: String, error: Option[String], notBefore: Option[Instant], refundAttempt: Boolean): Unit =
    Try(rows.updateOne(heldBy(id, workerId), TaskRowChange(TaskState.Waiting, lease = None,
      nextEligibleAt = Some(notBefore), attemptsBy = if (refundAttempt) -1 else 0, lastError = error)))
      .recover { case exception => logger.warn(s"TaskQueue.release($id) failed: ${exception.getMessage}") }
      .get

  /** [[TaskConsumer.reapExpiredLeases]]: every worked-on task whose lease ended before `now`, back
   *  to waiting. THROWS on a failed write: answered as 0 it reads as nothing expired. */
  def reap(now: Instant): Int =
    rows.updateAll(TaskRowMatch(state = Some(TaskState.WorkedOn), leaseExpiredBefore = Some(now)),
      TaskRowChange(TaskState.Waiting, lease = None))
}

object TaskLeases {
  // Eligible = waiting AND not held back by a backoff window still in the future AND of a type this
  // build knows. A row of any other type is, on a rollback, one a NEWER image queued: it is left
  // waiting, untouched, for an image that knows it. Deleting it lost that work; leasing it and failing
  // the decode left it reaped back to waiting and re-claimed every lease. Matched out, no claim spins on it.
  private def claimable(now: Instant) =
    TaskRowMatch(state = Some(TaskState.Waiting), knownTypesOnly = true, eligibleAt = Some(now))

  private def heldBy(id: String, workerId: String) =
    TaskRowMatch(id = Some(id), state = Some(TaskState.WorkedOn), workerId = Some(workerId))
}

/**
 * A [[TaskQueue]] whose lease cycle — claim, complete, release, reap — is [[TaskLeases]]' over the [[TaskRows]] its
 * store supplies, and whose counts and snapshot are read through them by one rule: [[MongoTaskQueue]] and
 * [[InMemoryTaskQueue]] extend it and differ only in `taskRows`, never in the rules. Finishing a task removes its row outright (no tombstone): a gone row allows the key's re-enqueue, and
 * nothing lists finished tasks. The ownership guard keeps a late call from a reaped worker off a task another worker
 * has since reclaimed.
 */
abstract class LeasedTaskQueue extends TaskQueue {

  /** Where the rows live: read once, on the first lease call. */
  protected def taskRows: TaskRows

  private lazy val leases = new TaskLeases(taskRows)

  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = leases.claim(workerId, lease, now)

  override def complete(id: String, workerId: String): Unit = leases.complete(id, workerId)

  override def release(id: String, workerId: String, error: Option[String], notBefore: Option[Instant], refundAttempt: Boolean): Unit =
    leases.release(id, workerId, error, notBefore, refundAttempt)

  override def reapExpiredLeases(now: Instant): Int = leases.reap(now)

  /** Only the queue's own states, of the types this build knows: a row of any other type is a newer image's (left
   *  for it) or a retired type's (dropped by the lease reaper), never work this queue is behind on, and a state no
   *  rule writes is never shown as a queue state. A state with no rows stays absent, never 0. */
  override def countByState(): Map[String, Long] =
    taskRows.countByState(TaskRowMatch(states = TaskState.all, knownTypesOnly = true)).filter(_._2 > 0)

  /** The active rows of known types, walked in `activeByPriority` order — each state's oldest first — spending what
   *  is left of `activeLimit` on each state in turn (one index-served query a state in Mongo). */
  override def monitor(activeLimit: Int): QueueSnapshot = {
    val active = TaskState.activeByPriority.foldLeft(Seq.empty[TaskSummary]) { (listed, state) =>
      val remaining = activeLimit - listed.size
      if (remaining <= 0) listed
      else listed ++ taskRows.oldest(TaskRowMatch(state = Some(state), knownTypesOnly = true), remaining)
    }
    QueueSnapshot(countByState(), active)
  }
}
