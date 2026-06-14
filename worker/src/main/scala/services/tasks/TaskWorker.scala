package services.tasks

import services.Stoppable
import tools.DaemonExecutors
import play.api.Logging

import java.net.InetAddress
import java.time.Instant
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
 * running exactly **one task at a time**. Every worker is its own daemon thread:
 * claim one waiting task, run its handler to completion, then claim the next.
 * Total in-flight work is therefore hard-capped at `poolSize` — there's no batch
 * claim, no separate dispatch pool, and crucially no in-memory task buffer: the
 * `worked_on` set can never exceed the number of workers. Each worker claims
 * under its own id (`host-pid-N`), so the tasks page shows up to `poolSize`
 * distinct workers, each holding a single task.
 *
 * **Workers don't poll.** When nothing is waiting a worker *parks* on a
 * [[TaskDoorbell]] rather than sleeping on a timer. The queue's
 * [[TaskQueue.watchWaiting]] push — a Mongo change stream in production — rings
 * the doorbell the instant a task is enqueued, so a parked worker wakes and
 * `claim`s it with near-zero latency and zero idle Mongo traffic (that constant
 * empty-claim floor is what denied the shared-cpu box the idle time to rebuild
 * CPU credits). The ring is only a doorbell: it carries no task, so workers
 * still claim atomically against Mongo and a spurious or missed ring is harmless
 * — at worst it costs one extra cheap claim, or defers pickup to the backstop.
 *
 * `idleBackstop` bounds how long a worker stays parked without a ring: a safety
 * net for a change-stream blip, a standalone (non-replica-set) Mongo that can't
 * stream, or a lease another machine returned to waiting that this node didn't
 * observe — not the normal wakeup path. `retryBackoff` is the pause after a task
 * goes back to the queue (`Reschedule`/failure/no-handler) so a perpetually
 * failing task retries at ~`retryBackoff` cadence rather than hot-spinning; that
 * pause deliberately ignores the doorbell.
 *
 * `processingTimeout` is the lease length: any task left in `worked_on` longer
 * than this — a crashed worker, a handler that hung, a fetch stuck past its own
 * timeout — is returned to waiting by `reapExpiredLeases`, which a single shared
 * reaper thread runs every `reapInterval` (independently of the workers, so a
 * stuck lease is reaped even while all workers are busy); a reap that frees work
 * also rings the doorbell so it's picked up promptly. It's a safety net, not a
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
  processingTimeout: FiniteDuration = 5.minutes,
  retryBackoff:      FiniteDuration = 2.seconds,
  idleBackstop:      FiniteDuration = 30.seconds,
  poolSize:          Int            = 4,
  reapInterval:      FiniteDuration = 30.seconds
) extends Stoppable with Logging {
  import HandlerOutcome._
  import TaskWorker._

  private val byType: Map[TaskType, TaskHandler] = handlers.map(h => h.taskType -> h).toMap

  private val baseId: String = {
    val host = Try(InetAddress.getLocalHost.getHostName).getOrElse("unknown")
    s"$host-${ProcessHandle.current().pid()}"
  }

  private val running:  AtomicBoolean          = new AtomicBoolean(false)
  private val workers:  mutable.Buffer[Thread] = mutable.Buffer.empty
  // Decouples the change-stream producer from the worker threads: producers ring,
  // idle workers park on it. Carries no task — the claim is still the source of
  // truth.
  private val doorbell: TaskDoorbell           = new TaskDoorbell
  // Handle to the queue's new-task push; closed on stop().
  private var watchHandle: Option[AutoCloseable] = None
  // Reaping is global, so one shared thread does it for the whole pool — and it
  // must keep ticking while every worker is busy, which a per-worker reap can't.
  private val reaper: ScheduledExecutorService = DaemonExecutors.scheduler("task-reaper")

  def start(): Unit = {
    running.set(true)
    // Push: ring the doorbell whenever the queue gains a fresh task, so a parked
    // worker wakes and claims it immediately instead of waiting out the backstop.
    watchHandle = queue.watchWaiting(() => doorbell.ring())
    // Leases run `processingTimeout` (5min), so reaping every `reapInterval`
    // (30s) is ample. A reap returns crashed/stuck (and other nodes') leases to
    // waiting; ring so the freed work is picked up now, not at the next backstop.
    reaper.scheduleWithFixedDelay(
      () => Try { if (queue.reapExpiredLeases() > 0) doorbell.ring() },
      reapInterval.toMillis, reapInterval.toMillis, TimeUnit.MILLISECONDS)
    (0 until poolSize).foreach { i =>
      val id = s"$baseId-$i"
      val t  = new Thread(() => runLoop(id), s"task-worker-$i")
      t.setDaemon(true)
      t.start()
      workers += t
    }
    logger.info(s"TaskWorker started ($poolSize worker(s) $baseId-0..${poolSize - 1}, ${byType.size} handler(s), push=${watchHandle.isDefined}, idle-backstop ${idleBackstop.toSeconds}s, reap ${reapInterval.toSeconds}s).")
  }

  /** One worker slot: claim a task, run it to completion, claim the next.
   *
   *  With work it loops at full speed (claim → run → claim). Idle, it parks on
   *  the doorbell until a ring (a new task, or a reap that freed one) or the
   *  `idleBackstop` elapses — so a quiet pool issues no empty claims at all. A
   *  returned (rescheduled/failed/no-handler) task backs off `retryBackoff` and
   *  ignores the doorbell, so a single perpetually-failing task can't hot-spin.
   *
   *  The generation is snapshotted *before* the claim, so a ring landing in the
   *  gap between a failed claim and the park advances the counter past the
   *  snapshot and the park returns at once — the lost-wakeup window is closed. */
  private def runLoop(workerId: String): Unit =
    while (running.get()) {
      val since = doorbell.generation
      Try(claimAndRun(workerId)).getOrElse(PollResult.Idle) match {
        case PollResult.Completed => ()                              // got work — claim the next immediately
        case PollResult.Returned  => sleepFor(retryBackoff.toMillis) // bounded retry; ignore the doorbell
        case PollResult.Idle      => doorbell.awaitSince(since, idleBackstop.toMillis)
      }
    }

  /** Sleep, honouring a stop interrupt. */
  private def sleepFor(ms: Long): Unit =
    if (running.get())
      try Thread.sleep(ms)
      catch { case _: InterruptedException => Thread.currentThread().interrupt() }

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
      // No handler wired (e.g. mid-deploy) — return it immediately (no backoff)
      // so a node that has the handler can take it rather than tombstoning
      // unfinished work.
      queue.release(task.id, workerId, Some(s"no handler for ${task.taskType.name}"))
      PollResult.Returned
    case Some(h) =>
      Try(h.handle(task)) match {
        case Success(Done) | Success(Skipped) => queue.complete(task.id, workerId); PollResult.Completed
        case Success(Reschedule(err)) =>
          queue.release(task.id, workerId, err, Some(backoffUntil(task.attempts)))
          PollResult.Returned
        case Failure(exception) =>
          logger.warn(s"Task ${task.taskType.name}/${task.dedupKey} failed: ${exception.getMessage}")
          queue.release(task.id, workerId, Some(exception.getMessage), Some(backoffUntil(task.attempts)))
          PollResult.Returned
      }
  }

  // When this attempt failed transiently, hold the task back from re-claim until
  // its exponential backoff elapses (see `TaskWorker.retryBackoffFor`).
  private def backoffUntil(attempts: Int): Instant =
    Instant.now().plusMillis(retryBackoffFor(attempts).toMillis)

  override def stop(): Unit = {
    running.set(false)
    watchHandle.foreach(h => Try(h.close()))
    reaper.shutdown()
    doorbell.ring()                // wake every parked worker so it sees running=false
    workers.foreach(_.interrupt()) // break any in-flight retryBackoff sleep
    ()
  }
}

object TaskWorker {
  /** Exponential backoff a transiently-failing task is held back before re-claim:
   *  5s, 10s, 20s, 40s, … doubling per attempt, capped at 30 min. Keyed on
   *  `attempts` (incremented on each claim, so the first failure ⇒ attempts=1 ⇒
   *  5s). This is the per-task version of what the inline `scheduleTmdbRetry`
   *  used to do for TMDB — now applied to every task type, so a rate-limited
   *  upstream isn't hammered at the pool's tight `retryBackoff` cadence. */
  private[tasks] def retryBackoffFor(attempts: Int): scala.concurrent.duration.FiniteDuration = {
    import scala.concurrent.duration._
    val shift  = math.min(math.max(attempts - 1, 0), 20)
    math.min(30.minutes.toMillis, 5000L * (1L << shift)).millis
  }

  /** Why a worker slot's last claim ended — drives whether it backs off. */
  private[tasks] sealed trait PollResult
  private[tasks] object PollResult {
    case object Idle      extends PollResult // nothing waiting to claim
    case object Completed extends PollResult // handler finished (Done/Skipped) — claim the next immediately
    case object Returned  extends PollResult // task went back to the queue — back off one retry interval
  }
}

/**
 * A monotone "new work might be waiting" doorbell decoupling the change-stream
 * producer from the worker threads. `ring()` bumps a generation counter and
 * wakes every parked worker; a worker parks with `awaitSince(gen, timeoutMs)`,
 * having snapshotted `generation` *before* its failed claim — so a ring that
 * lands in the gap between the claim and the park advances the counter past the
 * snapshot and the park returns at once, closing the lost-wakeup window. The
 * timeout is the pool's idle backstop. No task travels through it; it's purely a
 * wakeup signal, so over- or under-ringing only ever costs an extra cheap claim.
 */
private[tasks] final class TaskDoorbell {
  private val lock = new Object
  private var gen  = 0L

  /** The current ring count; snapshot it before a claim, pass to `awaitSince`. */
  def generation: Long = lock.synchronized(gen)

  /** Wake every parked worker and advance the generation. */
  def ring(): Unit = lock.synchronized { gen += 1L; lock.notifyAll() }

  /** Park until `generation` moves past `since`, or `timeoutMs` elapses
   *  (whichever first). Interrupt-safe so `stop()` can break it. */
  def awaitSince(since: Long, timeoutMs: Long): Unit = lock.synchronized {
    val deadline  = System.nanoTime() + timeoutMs * 1000000L
    var remaining = timeoutMs
    try
      while (gen <= since && remaining > 0L) {
        lock.wait(remaining)
        remaining = (deadline - System.nanoTime()) / 1000000L
      }
    catch { case _: InterruptedException => Thread.currentThread().interrupt() }
  }
}
