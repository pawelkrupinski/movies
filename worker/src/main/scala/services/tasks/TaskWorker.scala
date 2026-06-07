package services.tasks

import services.Stoppable
import tools.DaemonExecutors
import play.api.Logging

import java.net.InetAddress
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Does the actual work for one task type. The first thing a handler should do
 *  is re-check freshness and return `Skipped` if the data is already current —
 *  the queue hands out work without knowing whether it's still needed. */
trait TaskHandler {
  def taskType: TaskType
  def handle(task: Task): HandlerOutcome
}

/**
 * Polls a [[TaskQueue]] and dispatches claimed tasks to per-type [[TaskHandler]]s.
 *
 * One daemon scheduler thread ticks every `pollInterval`: it first times out
 * stuck/dead leases, then claims up to `maxPerTick` tasks and runs each handler
 * on `ec` (so the scheduler thread never blocks on a network fetch). A handler
 * that finishes (`Done`/`Skipped`) tombstones the task; one that can't
 * (`Reschedule` or a thrown exception) returns it to waiting to retry.
 *
 * `processingTimeout` is the lease length: any task left in `worked_on` longer
 * than this — a crashed worker, a handler that hung, a fetch stuck past its own
 * timeout — is returned to waiting on the next tick by `reapExpiredLeases`, so a
 * task can never be stranded "being processed" forever. It's a safety net, not a
 * correctness boundary: if it fires on a task that's actually still running, the
 * duplicate run is harmless because every handler is freshness-gated and writes
 * idempotently (it finds the data fresh and returns `Skipped`). Keep it
 * comfortably above the slowest handler's wall-clock; the default 5 min covers
 * the slowest scrape/detail fetch while still freeing a genuinely stuck task
 * quickly.
 */
class TaskWorker(
  queue:             TaskQueue,
  handlers:          Seq[TaskHandler],
  ec:                ExecutionContext,
  processingTimeout: FiniteDuration = 5.minutes,
  pollInterval:      FiniteDuration = 2.seconds,
  maxPerTick:        Int            = 20
) extends Stoppable with Logging {
  import HandlerOutcome._

  private val byType: Map[TaskType, TaskHandler] = handlers.map(h => h.taskType -> h).toMap

  private val workerId: String = {
    val host = Try(InetAddress.getLocalHost.getHostName).getOrElse("unknown")
    s"$host-${ProcessHandle.current().pid()}"
  }

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("task-worker")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(
      () => Try(poll()), pollInterval.toMillis, pollInterval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"TaskWorker started (workerId=$workerId, ${byType.size} handler(s), poll ${pollInterval.toSeconds}s).")
  }

  /** One tick: time out stuck leases, then claim and dispatch up to `maxPerTick`
   *  tasks onto `ec`. Package-private so tests can drive it deterministically
   *  with a same-thread EC. Returns how many tasks were dispatched. */
  private[tasks] def poll(): Int = {
    queue.reapExpiredLeases()
    val seen     = scala.collection.mutable.Set.empty[String]
    var continue = true
    while (continue && seen.size < maxPerTick) {
      queue.claim(workerId, processingTimeout) match {
        case None => continue = false
        // A task we already dispatched this tick came back: a synchronous
        // handler released it (Reschedule / failure / no-handler) and it
        // re-surfaced as waiting, so `claim` just leased it back to us. Release
        // it again — don't strand it in worked_on — and stop; it retries on a
        // later tick. (With the async prod EC the handler hasn't run yet within
        // a tick, so a dispatched task stays worked_on and this never fires.)
        case Some(task) if seen.contains(task.id) =>
          queue.release(task.id, workerId)
          continue = false
        case Some(task) =>
          seen += task.id
          ec.execute(() => runHandler(task))
      }
    }
    seen.size
  }

  private def runHandler(task: Task): Unit = byType.get(task.taskType) match {
    case None =>
      // No handler wired (e.g. mid-deploy) — return it so a node that has the
      // handler can take it rather than tombstoning unfinished work.
      queue.release(task.id, workerId, Some(s"no handler for ${task.taskType.name}"))
    case Some(h) =>
      Try(h.handle(task)) match {
        case Success(Done) | Success(Skipped) => queue.complete(task.id, workerId)
        case Success(Reschedule(err))         => queue.release(task.id, workerId, err)
        case Failure(ex) =>
          logger.warn(s"Task ${task.taskType.name}/${task.dedupKey} failed: ${ex.getMessage}")
          queue.release(task.id, workerId, Some(ex.getMessage))
      }
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
