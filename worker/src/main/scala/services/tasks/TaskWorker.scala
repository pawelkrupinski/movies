package services.tasks

import services.Stoppable
import tools.DaemonExecutors
import play.api.Logging

import java.net.InetAddress
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
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
 * A fixed pool of `poolSize` workers draining a [[TaskQueue]], each fetching and
 * running exactly **one task at a time**. Every worker is its own daemon thread
 * running a tight loop: claim one waiting task, run its handler to completion,
 * then claim the next. Total in-flight work is therefore hard-capped at
 * `poolSize` — there's no batch claim and no separate dispatch pool, so the
 * number of tasks in `worked_on` at once can never exceed the number of workers.
 * Each worker claims under its own id (`host-pid-N`), so the tasks page shows up
 * to `poolSize` distinct workers, each holding a single task.
 *
 * A worker only sleeps `pollInterval` when it found nothing to do, or when the
 * task it ran went back to the queue (`Reschedule`/failure/no-handler) — so a
 * backlog drains at full speed, but a lone perpetually-rescheduling task is
 * retried at most once per interval per worker rather than hot-spinning.
 *
 * `processingTimeout` is the lease length: any task left in `worked_on` longer
 * than this — a crashed worker, a handler that hung, a fetch stuck past its own
 * timeout — is returned to waiting by `reapExpiredLeases`, which a single shared
 * reaper thread runs every `pollInterval` (independently of the workers, so a
 * stuck lease is reaped even while all workers are busy). It's a safety net, not
 * a correctness boundary: if it fires on a task that's actually still running,
 * the duplicate run is harmless because every handler is freshness-gated and
 * writes idempotently (it finds the data fresh and returns `Skipped`). Keep it
 * comfortably above the slowest handler's wall-clock; the default 5 min covers
 * the slowest scrape/detail fetch while still freeing a genuinely stuck task
 * quickly.
 */
class TaskWorker(
  queue:             TaskQueue,
  handlers:          Seq[TaskHandler],
  processingTimeout: FiniteDuration = 5.minutes,
  pollInterval:      FiniteDuration = 2.seconds,
  poolSize:          Int            = 4,
  maxIdleInterval:   FiniteDuration = 30.seconds,
  reapInterval:      FiniteDuration = 30.seconds
) extends Stoppable with Logging {
  import HandlerOutcome._
  import TaskWorker._

  private val byType: Map[TaskType, TaskHandler] = handlers.map(h => h.taskType -> h).toMap

  private val baseId: String = {
    val host = Try(InetAddress.getLocalHost.getHostName).getOrElse("unknown")
    s"$host-${ProcessHandle.current().pid()}"
  }

  private val running: AtomicBoolean          = new AtomicBoolean(false)
  private val workers: mutable.Buffer[Thread] = mutable.Buffer.empty
  // Reaping is global, so one shared thread does it for the whole pool — and it
  // must keep ticking while every worker is busy, which a per-worker reap can't.
  private val reaper: ScheduledExecutorService = DaemonExecutors.scheduler("task-reaper")

  def start(): Unit = {
    running.set(true)
    // Leases run `processingTimeout` (5min), so reaping every `reapInterval`
    // (30s) is ample — a far cheaper cadence than polling, and it keeps the
    // every-tick `updateMany` off the shared Mongo's commit path most of the time.
    reaper.scheduleWithFixedDelay(
      () => Try(queue.reapExpiredLeases()), reapInterval.toMillis, reapInterval.toMillis, TimeUnit.MILLISECONDS)
    (0 until poolSize).foreach { i =>
      val id = s"$baseId-$i"
      val t  = new Thread(() => runLoop(id), s"task-worker-$i")
      t.setDaemon(true)
      t.start()
      workers += t
    }
    logger.info(s"TaskWorker started ($poolSize worker(s) $baseId-0..${poolSize - 1}, ${byType.size} handler(s), poll ${pollInterval.toSeconds}s, reap ${reapInterval.toSeconds}s).")
  }

  /** One worker slot: claim a task, run it to completion, claim the next.
   *
   *  When there's work it loops at full speed (claim → run → claim). When idle it
   *  backs off exponentially from `pollInterval` up to `maxIdleInterval`, so a
   *  quiet pool stops hammering the shared Mongo with empty `claim`s every few
   *  seconds — that constant floor is what denied the shared-cpu box the idle
   *  time to rebuild CPU credits. A returned (rescheduled/failed) task backs off
   *  one `pollInterval`, not the full ramp, so a transient failure retries soon. */
  private def runLoop(workerId: String): Unit = {
    var idleStreak = 0
    while (running.get()) {
      Try(claimAndRun(workerId)).getOrElse(PollResult.Idle) match {
        case PollResult.Completed => idleStreak = 0 // got work — claim the next immediately
        case PollResult.Returned  => idleStreak = 0; sleepFor(pollInterval.toMillis)
        case PollResult.Idle      => idleStreak += 1; sleepFor(backoffMillis(idleStreak))
      }
    }
  }

  /** Sleep, honouring a stop interrupt. */
  private def sleepFor(ms: Long): Unit =
    if (running.get())
      try Thread.sleep(ms)
      catch { case _: InterruptedException => Thread.currentThread().interrupt() }

  /** Idle back-off: `pollInterval` doubled per consecutive empty claim, capped at
   *  `maxIdleInterval`. Pure + package-private so the ramp is unit-testable. */
  private[tasks] def backoffMillis(idleStreak: Int): Long = {
    val base  = pollInterval.toMillis
    val shift = math.min(math.max(idleStreak - 1, 0), 30) // guard the bit-shift
    math.min(base << shift, maxIdleInterval.toMillis)
  }

  /** Claim a single waiting task for `workerId` and run its handler to
   *  completion — one task, then return. Package-private so tests can drive a
   *  worker deterministically without spinning up threads. */
  private[tasks] def claimAndRun(workerId: String): PollResult =
    queue.claim(workerId, processingTimeout) match {
      case None       => PollResult.Idle
      case Some(task) => runHandler(task, workerId)
    }

  private def runHandler(task: Task, workerId: String): PollResult = byType.get(task.taskType) match {
    case None =>
      // No handler wired (e.g. mid-deploy) — return it so a node that has the
      // handler can take it rather than tombstoning unfinished work.
      queue.release(task.id, workerId, Some(s"no handler for ${task.taskType.name}"))
      PollResult.Returned
    case Some(h) =>
      Try(h.handle(task)) match {
        case Success(Done) | Success(Skipped) => queue.complete(task.id, workerId); PollResult.Completed
        case Success(Reschedule(err))         => queue.release(task.id, workerId, err); PollResult.Returned
        case Failure(ex) =>
          logger.warn(s"Task ${task.taskType.name}/${task.dedupKey} failed: ${ex.getMessage}")
          queue.release(task.id, workerId, Some(ex.getMessage))
          PollResult.Returned
      }
  }

  override def stop(): Unit = {
    running.set(false)
    reaper.shutdown()
    workers.foreach(_.interrupt()) // break any in-flight pollInterval sleep
    ()
  }
}

object TaskWorker {
  /** Why a worker slot's last claim ended — drives whether it backs off. */
  private[tasks] sealed trait PollResult
  private[tasks] object PollResult {
    case object Idle      extends PollResult // nothing waiting to claim
    case object Completed extends PollResult // handler finished (Done/Skipped) — claim the next immediately
    case object Returned  extends PollResult // task went back to the queue — back off one poll interval
  }
}
