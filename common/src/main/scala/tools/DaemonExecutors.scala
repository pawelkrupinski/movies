package tools

import java.util.concurrent.{AbstractExecutorService, ExecutorService, Executors, RejectedExecutionException, ScheduledExecutorService, Semaphore, TimeUnit}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

/**
 * Factories for background execution contexts and schedulers used across the
 * app. Both flavours produce daemon-flagged threads — the JVM doesn't wait on
 * them at shutdown — and follow a consistent `${name}-N` naming scheme so
 * thread dumps and logs stay readable.
 *
 * `virtualThreadEC` returns an [[ExecutionContextExecutorService]] — a Scala
 * `ExecutionContext` (for `Future { … }(executionContext)` call sites) that's also a
 * lifecycle-bearing `ExecutorService` (`shutdown()` / `awaitTermination(...)`
 * for `Stoppable.stop`). Backed by `newThreadPerTaskExecutor` with a virtual-
 * thread factory: every task gets its own virtual thread, the JVM scheduler
 * multiplexes them onto the small carrier pool, and we no longer cap
 * concurrency at the thread count. Rate-limit-sensitive call sites (TMDB,
 * Filmweb, scraped cinema sites) still enforce their throttle at the HTTP
 * layer — virtual threads are cheap to create, but the upstream still 429s.
 */
object DaemonExecutors {

  /** Virtual-thread-per-task EC + ExecutorService. Threads are virtual
   *  (so daemon by definition — virtual threads never keep the JVM alive)
   *  and named `${name}-N`. */
  def virtualThreadEC(name: String): ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(dropRejectedAfterShutdown(virtualThreadExecutor(name)))

  /** Wrap `es` so a submission rejected *because it has already been shut
   *  down* is silently dropped instead of thrown. In `Wiring.stop` the ordered
   *  cascade-drain races against still-completing background `Future` chains:
   *  a stage finishing on a drained pool synchronously schedules its
   *  continuation back onto that same (now shut-down) pool. The resulting
   *  `RejectedExecutionException` has nowhere to go — a terminal callback has no
   *  downstream promise to absorb it, so it reaches the EC's failure reporter
   *  (logback in prod) — and floods the logs with one stack trace per dangling
   *  continuation. For a thread-per-task executor a rejection can only mean
   *  "already shut down" (there's no bounded queue to overflow), so dropping it
   *  once `isShutdown` holds is benign; any other rejection is re-thrown. */
  private[tools] def dropRejectedAfterShutdown(es: ExecutorService): ExecutorService =
    new DelegatingExecutorService(es) {
      override def execute(command: Runnable): Unit =
        try es.execute(command)
        catch { case _: RejectedExecutionException if es.isShutdown => () }
    }

  /** Virtual-thread EC capped at `maxConcurrent` tasks running at once. Use
   *  when the upstream rate-limits or load-tests poorly past a small number
   *  of concurrent requests (Filmweb, scraped detail pages, …). Each task
   *  parks on a [[Semaphore]] before running; virtual threads park cheaply,
   *  so blocked tasks cost nothing. Falls back to plain `virtualThreadEC`
   *  semantics when `maxConcurrent <= 0`.
   *
   *  This is the single-consumer shortcut for [[SharedExecutionBudget]] —
   *  when several services need to share ONE cap across them all, build a
   *  `SharedExecutionBudget` and call `.executionContext(...)` per service instead. */
  def boundedEC(name: String, maxConcurrent: Int): ExecutionContextExecutorService =
    new SharedExecutionBudget(maxConcurrent).executionContext(name)

  /** Wrap `underlying` so every submitted task acquires a permit from
   *  `permits` before running and releases it after — capping how many run at
   *  once to the semaphore's size. Shared by [[boundedEC]] and
   *  [[SharedExecutionBudget]] so the gating logic lives in one place. */
  private[tools] def semaphoreGated(underlying: ExecutorService, permits: Semaphore): ExecutorService =
    new DelegatingExecutorService(underlying) {
      override def execute(command: Runnable): Unit = underlying.execute { () =>
        permits.acquire()
        try command.run() finally permits.release()
      }
    }

  private[tools] def virtualThreadExecutor(name: String): ExecutorService =
    Executors.newThreadPerTaskExecutor(Thread.ofVirtual().name(s"$name-", 0L).factory())

  /** A caller-runs `ExecutorService`: every submitted task executes INLINE on
   *  the calling thread during `execute`, so a `submit(...).get(...)` round-trip
   *  never spawns a thread and can never time out (the task is already done by
   *  the time `get` is called). Used by the deterministic test harness so a
   *  decorator that hands work to an executor ([[services.cinemas.common.AdaptiveTimeoutScraper]])
   *  stays single-threaded and order-stable — no thread-pool race for the
   *  determinism specs to shuffle. Production uses [[virtualThreadEC]] instead,
   *  where a real timeout can fire. */
  def directExecutor(): ExecutorService =
    new AbstractExecutorService {
      @volatile private var stopped = false
      override def execute(command: Runnable): Unit = command.run()
      override def shutdown(): Unit = stopped = true
      override def shutdownNow(): java.util.List[Runnable] = { stopped = true; java.util.Collections.emptyList() }
      override def isShutdown: Boolean = stopped
      override def isTerminated: Boolean = stopped
      override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true
    }

  /** Single-thread scheduled executor with a daemon platform thread named
   *  `name`. Schedulers stay platform-threaded — their job is firing a
   *  Runnable on a timer, and `Future`/EC has no stdlib equivalent for
   *  `scheduleAtFixedRate`. The scheduled Runnable typically just hands work
   *  back to a `virtualThreadEC` (`executionContext.execute(() => …)` or
   *  `Future(...)(executionContext)`), so the scheduler thread doesn't block on the work
   *  itself. */
  def scheduler(name: String): ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor { r =>
      val thread = new Thread(r, name)
      thread.setDaemon(true)
      thread
    }

  /** Single daemon platform thread with a FIFO queue, named `name`. Use to move
   *  ORDERED, potentially-blocking work off a thread that must not block —
   *  notably the Mongo driver's Netty I/O event loops: running the change-stream
   *  apply (a blocking stitch read + the synchronized read-model projection) on
   *  those loops made them contend and spin their wakeup eventfds, flooring the
   *  worker's CPU credit. A single thread keeps change events applied strictly in
   *  order; blocking on it is fine — that's the point, it isn't an I/O loop. */
  def singleThreadExecutor(name: String): ExecutorService =
    dropRejectedAfterShutdown(Executors.newSingleThreadExecutor { r =>
      val thread = new Thread(r, name)
      thread.setDaemon(true)
      thread
    })
}

/** An `AbstractExecutorService` whose lifecycle (`shutdown` / `shutdownNow` /
 *  `isShutdown` / `isTerminated` / `awaitTermination`) delegates straight to
 *  `delegate`; subclasses override only `execute`. Shared by the
 *  semaphore-gating and shutdown-tolerant wrappers so the lifecycle plumbing
 *  isn't spelled out twice. */
private[tools] abstract class DelegatingExecutorService(delegate: ExecutorService)
    extends AbstractExecutorService {
  override def shutdown(): Unit                        = delegate.shutdown()
  override def shutdownNow(): java.util.List[Runnable] = delegate.shutdownNow()
  override def isShutdown: Boolean                     = delegate.isShutdown
  override def isTerminated: Boolean                   = delegate.isTerminated
  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
    delegate.awaitTermination(timeout, unit)
}

/**
 * A concurrency budget shared across several execution contexts. Every EC
 * handed out by [[executionContext]] is an independent, separately-shutdownable virtual-
 * thread `ExecutorService`, but they all draw their run permits from ONE
 * shared [[Semaphore]] — so the total number of tasks running at once across
 * *every* EC built from this budget never exceeds `maxConcurrent`.
 *
 * The point is to let unrelated background services (cinema scrape + movie
 * enrichment + the rating refreshers) share a single CPU/concurrency cap on a
 * small box, without merging them into one literal pool. A merged pool would
 * break the ordered cascade-drain shutdown in `Wiring`, which shuts each
 * service's EC independently and relies on each draining before the next; here
 * each service still owns and shuts down its own `ExecutorService`, and only
 * the permit budget is shared.
 *
 * Deadlock note: tasks hold their permit for their whole run, so a task that
 * blocks waiting on *another* task from the SAME budget can deadlock once all
 * permits are held. Every consumer of a shared budget must therefore be
 * fire-and-forget (or block only on a DIFFERENT executor) — which is why the
 * nested-blocking `ParallelDetailFetch` keeps its own pool rather than drawing
 * from the scrape/enrichment budget.
 *
 * `maxConcurrent <= 0` means unbounded — a permit-free passthrough matching
 * `DaemonExecutors.virtualThreadEC` semantics (used by tests that don't want a
 * cap).
 */
/** The abstraction the wiring depends on for background-task execution contexts.
 *  Production wires [[SharedExecutionBudget]] (virtual threads under a shared cap);
 *  the determinism harness wires a same-thread implementation so the enrichment
 *  cascade runs by ordering alone, with no thread-pool race to shuffle. */
trait ExecutionBudget {
  def executionContext(name: String): ExecutionContextExecutorService
  def executionContext(name: String, subLimit: Int): ExecutionContextExecutorService
}

final class SharedExecutionBudget(val maxConcurrent: Int) extends ExecutionBudget {
  private val permits: Option[Semaphore] =
    if (maxConcurrent > 0) Some(new Semaphore(maxConcurrent)) else None

  /** A fresh virtual-thread EC named `${name}-N`, gated by this budget's shared
   *  permits. Shut it down independently of any sibling EC from the same
   *  budget. */
  override def executionContext(name: String): ExecutionContextExecutorService = {
    val underlying = DaemonExecutors.virtualThreadExecutor(name)
    val gated      = permits.fold(underlying)(p => DaemonExecutors.semaphoreGated(underlying, p))
    ExecutionContext.fromExecutorService(DaemonExecutors.dropRejectedAfterShutdown(gated))
  }

  /** Like [[executionContext]], but this EC additionally caps ITS OWN tasks at `subLimit`
   *  running at once — so it shares the budget with its siblings yet never uses
   *  more than `subLimit` of the budget's permits simultaneously. Use for a
   *  lower-priority consumer (e.g. the cinema scrape) that should get only a
   *  slice of the shared budget and never crowd out the rest.
   *
   *  The EC-private sub-permit is acquired BEFORE the shared budget permit, so a
   *  task waiting for a sub-slot doesn't sit holding a budget permit (which would
   *  let `subLimit` parked tasks starve the other consumers). The sub-semaphore
   *  is private to this EC, so there's no cross-consumer deadlock. */
  override def executionContext(name: String, subLimit: Int): ExecutionContextExecutorService = {
    val underlying  = DaemonExecutors.virtualThreadExecutor(name)
    val subGated    = DaemonExecutors.semaphoreGated(underlying, new Semaphore(subLimit.max(1)))
    val budgetGated = permits.fold(subGated)(p => DaemonExecutors.semaphoreGated(subGated, p))
    ExecutionContext.fromExecutorService(DaemonExecutors.dropRejectedAfterShutdown(budgetGated))
  }
}
