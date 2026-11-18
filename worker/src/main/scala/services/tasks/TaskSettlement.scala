package services.tasks

import services.tasks.WorkerTaskMetrics.Outcome
import play.api.Logging

import java.time.{Clock, Instant}
import scala.util.{Failure, Success, Try}

/**
 * What a task the pool ran comes to on the queue, from what its handler answered: completed (Done,
 * Skipped, a deterministic failure, or past `maxAttempts`), or released back to waiting — held on the
 * per-task exponential backoff after a failure, until the handler's own instant (capped) after a
 * `Deferred`, at once with its attempt given back when the worker is stopping. Every outcome is told to
 * the observer.
 *
 * @param onCompleted invoked with the task the instant it completes successfully (Done/Skipped), never on a
 *                    reschedule. The composition root wires this to publish a `TaskFinished` event so consumers
 *                    (e.g. the chunk-scrape coordinator) can chain follow-up work.
 */
final class TaskSettlement(
  queue:       TaskConsumer,
  observer:    TaskObserver = TaskObserver.NoOp,
  maxAttempts: TaskWorker.MaxAttempts = TaskWorker.MaxAttempts(TaskWorker.DefaultMaxAttempts),
  onCompleted: Task => Unit = _ => (),
  clock:       Clock
) extends Logging {
  import HandlerOutcome._
  import TaskWorker.PollResult

  /** Settle `task`, which `workerId` ran in `millis` to `outcome`; `stopping` when the pool is shutting down. */
  def settle(task: Task, workerId: String, outcome: Try[HandlerOutcome], millis: Long, stopping: Boolean): PollResult = outcome match {
    case Success(Done)    => completeWith(task, workerId, Outcome.Done, millis)
    case Success(Skipped) => completeWith(task, workerId, Outcome.Skipped, millis)
    // Stopping: whatever the handler made of the interrupt is the shutdown's, not the task's — handed back at
    // once, its attempt given back, for the next pod to claim without waiting out a lease or a backoff.
    case _ if stopping => handBack(task, workerId, millis)
    case Success(Reschedule(err)) => retry(task, workerId, err, millis, Outcome.Rescheduled)
    // No work was attempted, so no backoff is owed and no attempt is charged:
    // hold the task until the precondition is due to clear (the handler knows
    // when — e.g. the circuit's own half-open instant) and hand back the
    // increment `claim` took. Falls back to the normal curve when the handler
    // names no instant, so an unhinted defer still can't hot-loop — and never past MaxBackoff.
    case Success(Deferred(err, notBefore)) =>
      queue.release(task.id, workerId, err, Some(notBefore.fold(backoffUntil(task.attempts))(capped)), refundAttempt = true)
      observer.onFinished(task, Outcome.Deferred, millis)
      PollResult.Returned
    case Failure(exception) if TaskWorker.isDeterministic(exception) =>
      logger.warn(s"Task ${task.taskType.name}/${task.dedupKey} failed permanently on attempt ${task.attempts} — " +
                  s"dropping it without retry, a retry would replay it: ${exception.getMessage}")
      queue.complete(task.id, workerId)
      observer.onFinished(task, Outcome.Permanent, millis)
      PollResult.Completed
    case Failure(exception) =>
      logger.warn(s"Task ${task.taskType.name}/${task.dedupKey} failed: ${exception.getMessage}")
      retry(task, workerId, Some(tools.FailureText.of(exception)), millis, Outcome.Failed)
  }

  /** No handler wired here (e.g. mid-deploy) — return it, held off for `NoHandlerBackoff`, so a node
   *  that has the handler can take it rather than tombstoning unfinished work. Never claimable at
   *  once: as the oldest waiting row it would be every claim's next, and starve the queue. */
  def noHandler(task: Task, workerId: String): PollResult = {
    queue.release(task.id, workerId, Some(s"no handler for ${task.taskType.name}"),
      notBefore = Some(clock.instant().plusMillis(TaskWorker.NoHandlerBackoff.toMillis)), refundAttempt = true)
    observer.onFinished(task, Outcome.NoHandler, 0L)
    PollResult.Returned
  }

  /** A task its worker stopped holding: back to waiting, claimable at once, the attempt `claim` charged given back. */
  def handBack(task: Task, workerId: String, handleMillis: Long): PollResult = {
    queue.release(task.id, workerId, Some("the worker stopped"), notBefore = None, refundAttempt = true)
    observer.onFinished(task, Outcome.Deferred, handleMillis)
    PollResult.Returned
  }

  /** A failed attempt: back to waiting on its backoff, or — past `maxAttempts` — exhausted. */
  private def retry(task: Task, workerId: String, error: Option[String], millis: Long, outcome: String): PollResult =
    if (task.attempts >= maxAttempts.value) exhaust(task, workerId, error, millis)
    else {
      queue.release(task.id, workerId, error, Some(backoffUntil(task.attempts)))
      observer.onFinished(task, outcome, millis)
      PollResult.Returned
    }

  /** The terminal state of a task that never succeeded: past `maxAttempts` it is
   *  DROPPED — removed from the queue without success — logged once with its last
   *  error, and metered as `exhausted`. Before this a failing task rode the 30-minute
   *  backoff for the life of the deployment. The queue keeps no tombstone (an active
   *  dedup entry that never cleared would block the key for ever, turning a transient
   *  outage into a permanent hole), so the reapers remain the retry-forever loop, at
   *  their own cadence: one that still finds the work due re-enqueues it fresh. A
   *  `Deferred` never reaches here — it refunds its attempt, so a circuit-open host
   *  cannot exhaust a task it never let run. Not `completeWith`: the chain hook fires
   *  on success only. */
  private def exhaust(task: Task, workerId: String, error: Option[String], handleMillis: Long): PollResult = {
    logger.warn(s"Task ${task.taskType.name}/${task.dedupKey} exhausted ${task.attempts} attempts — dropping it (last error: ${error.getOrElse("none")}).")
    queue.complete(task.id, workerId)
    observer.onFinished(task, Outcome.Exhausted, handleMillis)
    PollResult.Completed
  }

  /** Tombstone a finished task (Done/Skipped), fire the chain hook, and record
   *  the outcome — shared by the two completing branches. */
  private def completeWith(task: Task, workerId: String, outcome: String, handleMillis: Long): PollResult = {
    queue.complete(task.id, workerId)
    Try(onCompleted(task))   // a buggy chain hook must not fail the completed task
    observer.onFinished(task, outcome, handleMillis)
    PollResult.Completed
  }

  // When this attempt failed transiently, hold the task back from re-claim until
  // its exponential backoff elapses (see `TaskWorker.retryBackoffFor`).
  private def backoffUntil(attempts: Int): Instant =
    clock.instant().plusMillis(TaskWorker.retryBackoffFor(attempts).toMillis)

  // A handler's own "not before" is a hint, not a lease on the queue: held to the backoff cap
  // so a precondition that names a far-off instant (or a bug that does) cannot park its task
  // past the point the pool would have retried a genuine failure.
  private def capped(notBefore: Instant): Instant = {
    val ceiling = clock.instant().plusMillis(TaskWorker.MaxBackoff.toMillis)
    if (notBefore.isAfter(ceiling)) ceiling else notBefore
  }
}
