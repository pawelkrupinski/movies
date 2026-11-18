package services.tasks

import services.tasktypes.TaskType

import settings.WorkerPoolSize

import tools.Stoppable
import play.api.Logging

import java.net.InetAddress
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

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
 * The parts: [[TaskDispatch]] runs a claimed task through its handler and [[TaskSettlement]] settles what it
 * answered; [[TaskLeaseReaper]] is the pool's shared timer thread. This class is the pool itself.
 *
 * `idleBackstop` bounds how long a worker stays parked without a ring: a safety
 * net for a change-stream blip, a standalone (non-replica-set) Mongo that can't
 * stream, or a lease another machine returned to waiting that this node didn't
 * observe — not the normal wakeup path. `retryBackoff` is the pause after a task
 * goes back to the queue (`Reschedule`/failure/no-handler) so a perpetually
 * failing task retries at ~`retryBackoff` cadence rather than hot-spinning; that
 * pause deliberately ignores the doorbell. Each retry is also held back on the
 * per-task exponential backoff (`retryBackoffFor`), and after `maxAttempts` the
 * task is dropped for good ([[TaskSettlement]]) — the pool never retries anything for ever.
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
  queue:             TaskConsumer,
  // Runs a claimed task through its type's handler and settles what it answered.
  dispatch:          TaskDispatch,
  // Returns expired leases to waiting, and rings the pool when a held task becomes claimable.
  leases:            TaskLeaseReaper,
  processingTimeout: TaskWorker.ProcessingTimeout = TaskWorker.ProcessingTimeout(5.minutes),
  retryBackoff:      TaskWorker.RetryBackoff = TaskWorker.RetryBackoff(2.seconds),
  idleBackstop:      TaskWorker.IdleBackstop = TaskWorker.IdleBackstop(30.seconds),
  poolSize:          WorkerPoolSize = WorkerPoolSize(TaskWorker.DefaultPoolSize),
  // Metrics hook: notified on every claim (and, through the dispatch's settlement, every handler
  // outcome). Default no-op keeps the worker decoupled from the metrics sink; the composition root
  // wires WorkerTaskMetrics here.
  observer:          TaskObserver   = TaskObserver.NoOp,
  // How long `stop()` waits for in-flight handlers to return before handing their tasks back itself.
  stopGrace:         FiniteDuration = TaskWorker.StopGrace,
  // What "now" is for claims — the wiring's clock. It has to ADVANCE: a clock that stands still
  // (TestWiring's pinned one) parks every backed-off task forever, so a harness that starts the task
  // worker must give the wiring a moving clock.
  clock:             java.time.Clock,
  // The retry back-off's sleep, and the monotonic source the idle wait's deadline and the stop's
  // join grace are read on: the root's `sleep` and `stopwatch`. The idle wait blocks on the doorbell's monitor in real
  // time, so a spec that leaves the worker idle passes a stopwatch that moves (a frozen one never reaches the deadline).
  sleep:             scala.concurrent.duration.FiniteDuration => Unit,
  stopwatch:         tools.Stopwatch
) extends Stoppable with Logging {
  import TaskWorker._

  private val baseId: String = {
    val host = Try(InetAddress.getLocalHost.getHostName).getOrElse("unknown")
    s"$host-${ProcessHandle.current().pid()}"
  }

  private val running:  AtomicBoolean          = new AtomicBoolean(false)
  // The task each worker holds while its handler runs: what `stop()` hands back if the handler outlives the grace.
  private val inFlight = new java.util.concurrent.ConcurrentHashMap[String, Task]()
  // Set by `stop()` alone: a worker driven without `start()` (a test's `claimAndRun`) is not stopping.
  @volatile private var stopping = false
  private val workers:  mutable.Buffer[Thread] = mutable.Buffer.empty
  // Decouples the change-stream producer from the worker threads: producers ring,
  // idle workers park on it. Carries no task — the claim is still the source of
  // truth.
  private val doorbell: TaskDoorbell           = new TaskDoorbell(stopwatch)
  // Handle to the queue's new-task push; closed on stop().
  private var watchHandle: Option[AutoCloseable] = None

  def start(): Unit = {
    running.set(true)
    // Push: ring the doorbell whenever the queue gains a fresh task, so a parked
    // worker wakes and claims it immediately instead of waiting out the backstop —
    // or, for a task held by `notBefore`, the moment its hold ends.
    watchHandle = queue.watchWaiting(heldUntil => leases.ringWhenClaimable(heldUntil, () => doorbell.ring()))
    leases.start(() => doorbell.ring())
    (0 until poolSize.value).foreach { i =>
      val id = s"$baseId-$i"
      val t  = new Thread(() => runLoop(id), s"task-worker-$i")
      t.setDaemon(true)
      t.start()
      workers += t
    }
    logger.info(s"TaskWorker started (${poolSize.value} worker(s) $baseId-0..${poolSize.value - 1}, ${dispatch.handlerCount} handler(s), push=${watchHandle.isDefined}, idle-backstop ${idleBackstop.value.toSeconds}s, reap ${leases.interval.value.toSeconds}s).")
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
    while (running.get()) tick(workerId)

  /** One scheduling tick: claim+run a task (or park when idle), swallowing a stop
   *  interrupt. Package-private so a test can drive a single iteration deterministically.
   *
   *  `stop()` interrupts every worker (`workers.foreach(_.interrupt())`) to break an
   *  in-flight retry sleep OR an in-flight HTTP fetch. When the interrupt lands while a
   *  handler is blocked in a fetch (`RealHttpFetch.send` → `CompletableFuture.get`), it
   *  surfaces as an `InterruptedException` — which `NonFatal` treats as FATAL, so the
   *  `Try` wrappers below rethrow it. Left uncaught it escaped the worker thread's run
   *  method and was reported to Sentry as a FATAL `InterruptedException` on EVERY worker
   *  restart (mechanism `UncaughtExceptionHandler`) — pure shutdown noise, not a real
   *  fault. Swallow it here as the expected stop signal: clear the interrupt flag and let
   *  `while (running.get())` end the loop (`stop()` sets `running=false` before
   *  interrupting). A spurious interrupt while still running simply resumes the loop. */
  private[tasks] def tick(workerId: String): Unit =
    try {
      val since = doorbell.generation
      Try(claimAndRun(workerId)).getOrElse(PollResult.Idle) match {
        case PollResult.Completed => ()                              // got work — claim the next immediately
        case PollResult.Returned  => sleepFor(retryBackoff.value.toMillis) // bounded retry; ignore the doorbell
        case PollResult.Idle      => doorbell.awaitSince(since, idleBackstop.value.toMillis)
      }
    } catch {
      case _: InterruptedException => Thread.interrupted(); ()
    }

  /** Sleep, honouring a stop interrupt. */
  private def sleepFor(ms: Long): Unit =
    if (running.get())
      try sleep(scala.concurrent.duration.Duration(ms, java.util.concurrent.TimeUnit.MILLISECONDS))
      catch { case _: InterruptedException => Thread.currentThread().interrupt() }

  /** Claim a single waiting task for `workerId` and run its handler to
   *  completion — one task, then return. Package-private so tests can drive a
   *  worker deterministically without spinning up threads. */
  private[tasks] def claimAndRun(workerId: String): PollResult =
    queue.claim(workerId, processingTimeout.value, clock.instant()) match {
      case None       => PollResult.Idle
      case Some(task) =>
        // Baton: this claim consumed one task, so wake one more parked worker in
        // case another is waiting. A burst of N tasks thus wakes ≈N workers (each
        // successful claimer hands off) instead of the whole pool per enqueue.
        doorbell.ring()
        observer.onStarted(task)          // claimed and kicked off
        inFlight.put(workerId, task)
        try dispatch.run(task, workerId, stopping)
        finally inFlight.remove(workerId, task)
    }

  /** The doorbell's ring count — for tests asserting the baton hand-off. */
  private[tasks] def doorbellGeneration: Long = doorbell.generation

  /** Stop claiming, interrupt every worker (a sleep, an in-flight fetch), wait up to [[stopGrace]] for their handlers to
   *  return — each hands its task back ([[TaskSettlement.handBack]]) — then hand back any task still held: a dying pod leaves nothing
   *  leased behind it. Before, its tasks sat worked-on until their lease ran out (5 min) or came back charged and backed
   *  off as failures (prod PL 2026-10-04: three tasks held by a pod gone since 22:08 until 22:13). The queue's ownership
   *  guard turns a late completion from a handler that outlived the grace into a no-op. */
  override def stop(): Unit = {
    stopping = true
    running.set(false)
    watchHandle.foreach(h => Try(h.close()))
    leases.stop()                  // the reap, and every hold's pending ring with it
    doorbell.ringAll()             // wake every parked worker so it sees running=false
    workers.foreach(_.interrupt()) // break any in-flight retryBackoff sleep or fetch
    val waited = stopwatch.start()
    workers.foreach(t => Try(t.join((stopGrace - waited.elapsed).toMillis.max(1L))))
    val held = inFlight.asScala.toSeq
    held.foreach { case (workerId, task) =>
      Try(queue.release(task.id, workerId, Some("the worker stopped"), notBefore = None, refundAttempt = true))
    }
    if (held.nonEmpty) logger.info(s"TaskWorker stopped: handed back ${held.size} task(s) still running after ${stopGrace.toSeconds}s")
    ()
  }
}

object TaskWorker {

  /** How long a stopping worker waits for its in-flight handlers before handing their tasks back itself — well inside
   *  the pod's 30 s termination grace, which the rest of the shutdown shares. */
  val StopGrace: FiniteDuration = 10.seconds

  /** How long from `now` until a task held to `heldUntil` can be claimed, or None when it already can. Kept to the
   *  nanosecond: a wait cut to whole milliseconds rings before a hold that splits one ends, and that ring's claim
   *  finds nothing, parking the pool for the whole idle backstop. */
  def untilClaimable(now: Instant, heldUntil: Option[Instant]): Option[FiniteDuration] =
    heldUntil.map(until => java.time.Duration.between(now, until).toNanos).filter(_ > 0).map(_.nanos)

  /** How long a claimed task may run before its lease is taken back. */
  final case class ProcessingTimeout(value: FiniteDuration) extends AnyVal
  /** How long a failed task waits before it may be claimed again. */
  final case class RetryBackoff(value: FiniteDuration) extends AnyVal
  /** How long an idle worker sleeps before polling the queue again, whatever it was told. */
  final case class IdleBackstop(value: FiniteDuration) extends AnyVal
  /** How often expired leases are reaped back onto the queue. */
  final case class ReapInterval(value: FiniteDuration) extends AnyVal
  /** How many attempts a task gets before it is parked as failed. */
  final case class MaxAttempts(value: Int) extends AnyVal

  /** A failure a retry can only replay: a violated `require` / an argument the code
   *  itself rejected (ES "La luz", 2026-09-21: `rekey requires same normalised
   *  cleanTitle`, 12 identical attempts over 1h45m). EXACTLY `IllegalArgumentException`,
   *  not its subclasses: `NumberFormatException`, `UnsupportedCharsetException` and
   *  friends come out of reading an upstream payload, where an error or truncated page
   *  is transient. No handler throws IAE as a "not yet" signal — those return
   *  `Reschedule`/`Deferred` — so nothing that clears on its own is classed here. */
  def isDeterministic(failure: Throwable): Boolean = failure.getClass == classOf[IllegalArgumentException]

  /** A failed lookup that is an ANSWER about the film, not a read that did not happen:
   *  the source's own "not found" (`ReadOutcome.isAbsent`), a deterministic failure a retry would
   *  replay ([[isDeterministic]]), or a missing replay fixture — which only the
   *  fixture harness throws, and treats as permanent (see `tools.RetryingHttpFetch.recoverable`). Anything
   *  else — an outage, a rate limit, a timeout, a 401 on a bad key — is about the source, never the
   *  film, and must not conclude it. Read by the TMDB side's writes (`services.corpus.MovieService`) and the IMDb id
   *  resolver's retries (`services.enrichment.ImdbIdResolver.definitive`). */
  def failedDefinitively(failure: Throwable): Boolean =
    tools.ReadOutcome.isAbsent(failure) || isDeterministic(failure) || failure.isInstanceOf[java.io.FileNotFoundException]

  /** How many worker threads the pool runs by default.
   *
   *  Named rather than inlined because the test harness's synchronous stand-in for
   *  this pool sizes itself from the same number: a harness that drains one task at
   *  a time is not standing in for a pool of four, it is a slower thing with a
   *  different shape. */
  val DefaultPoolSize: Int = 4

  /** How many claims a task gets before the pool gives up on it (see `TaskSettlement.exhaust`). The
   *  backoff curve reaches its 30-minute cap at attempt 10, so 12 is ~2.3 hours of
   *  retrying — comfortably past the staging handlers' own give-up budgets (6), which
   *  therefore still conclude on their own terms, and long enough that an upstream
   *  outage of an hour or two never drops anything. */
  val DefaultMaxAttempts: Int = 12

  /** Exponential backoff a transiently-failing task is held back before re-claim:
   *  5s, 10s, 20s, 40s, … doubling per attempt, capped at 30 min — and cut off for
   *  good at `maxAttempts` (see `TaskSettlement.exhaust`). Keyed on
   *  `attempts` (incremented on each claim, so the first failure ⇒ attempts=1 ⇒
   *  5s). This is the per-task version of what the inline `scheduleTmdbRetry`
   *  used to do for TMDB — now applied to every task type, so a rate-limited
   *  upstream isn't hammered at the pool's tight `retryBackoff` cadence. */
  private[tasks] def retryBackoffFor(attempts: Int): FiniteDuration = {
    val shift  = math.min(math.max(attempts - 1, 0), 20)
    math.min(MaxBackoff.toMillis, 5000L * (1L << shift)).millis
  }

  /** How long a task no handler here runs is held back before it is claimable again. */
  val NoHandlerBackoff: FiniteDuration = 5.minutes

  /** The longest the pool ever holds a waiting task back — the backoff curve's cap, and the
   *  ceiling on a `Deferred`'s named instant too. Alerted on as WorkerTaskParkedTooLong. */
  val MaxBackoff: FiniteDuration = 30.minutes

  /** Why a worker slot's last claim ended — drives whether it backs off. */
  private[tasks] sealed trait PollResult
  private[tasks] object PollResult {
    case object Idle      extends PollResult // nothing waiting to claim
    case object Completed extends PollResult // task left the queue (Done/Skipped/exhausted) — claim the next immediately
    case object Returned  extends PollResult // task went back to the queue — back off one retry interval
  }
}

/**
 * A monotone "new work might be waiting" doorbell decoupling the change-stream
 * producer from the worker threads. `ring()` bumps a generation counter and
 * wakes ONE parked worker; a worker parks with `awaitSince(gen, timeoutMs)`,
 * having snapshotted `generation` *before* its failed claim — so a ring that
 * lands in the gap between the claim and the park advances the counter past the
 * snapshot and the park returns at once, closing the lost-wakeup window. The
 * timeout is the pool's idle backstop. No task travels through it; it's purely a
 * wakeup signal, so over- or under-ringing only ever costs an extra cheap claim.
 *
 * `ring()` wakes ONE waiter, not all: a single enqueue-insert is one new task, so
 * waking the whole pool means `poolSize - 1` empty claims (and, with N instances,
 * N×poolSize). Instead a woken worker that actually claims work rings again (the
 * baton, in [[TaskWorker.claimAndRun]]), so N available tasks wake ≈N workers with
 * ~one trailing empty claim per burst. `ringAll()` (used only by `stop`) still
 * wakes everyone.
 */
private[tasks] final class TaskDoorbell(stopwatch: tools.Stopwatch) {
  private val lock = new Object
  private var gen  = 0L

  /** The current ring count; snapshot it before a claim, pass to `awaitSince`. */
  def generation: Long = lock.synchronized(gen)

  /** Wake ONE parked worker and advance the generation (the baton hands off to the
   *  next as each claim succeeds — see the class doc). */
  def ring(): Unit = lock.synchronized { gen += 1L; lock.notify() }

  /** Wake EVERY parked worker (shutdown only — so all see `running=false`). */
  def ringAll(): Unit = lock.synchronized { gen += 1L; lock.notifyAll() }

  /** Park until `generation` moves past `since`, or `timeoutMs` elapses
   *  (whichever first). Interrupt-safe so `stop()` can break it. */
  def awaitSince(since: Long, timeoutMs: Long): Unit = lock.synchronized {
    val deadline  = stopwatch.nanoTime() + timeoutMs * 1000000L
    var remaining = timeoutMs
    try
      while (gen <= since && remaining > 0L) {
        lock.wait(remaining)
        remaining = (deadline - stopwatch.nanoTime()) / 1000000L
      }
    catch { case _: InterruptedException => Thread.currentThread().interrupt() }
  }
}
