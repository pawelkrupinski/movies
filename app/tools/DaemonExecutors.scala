package tools

import java.util.concurrent.{AbstractExecutorService, ExecutorService, Executors, ScheduledExecutorService, Semaphore, TimeUnit}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

/**
 * Factories for background execution contexts and schedulers used across the
 * app. Both flavours produce daemon-flagged threads — the JVM doesn't wait on
 * them at shutdown — and follow a consistent `${name}-N` naming scheme so
 * thread dumps and logs stay readable.
 *
 * `virtualThreadEC` returns an [[ExecutionContextExecutorService]] — a Scala
 * `ExecutionContext` (for `Future { … }(ec)` call sites) that's also a
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
    ExecutionContext.fromExecutorService(virtualThreadExecutor(name))

  /** Virtual-thread EC capped at `maxConcurrent` tasks running at once. Use
   *  when the upstream rate-limits or load-tests poorly past a small number
   *  of concurrent requests (Filmweb, scraped detail pages, …). Each task
   *  parks on a [[Semaphore]] before running; virtual threads park cheaply,
   *  so blocked tasks cost nothing. Falls back to plain `virtualThreadEC`
   *  semantics when `maxConcurrent <= 0`.
   *
   *  This is the single-consumer shortcut for [[SharedExecutionBudget]] —
   *  when several services need to share ONE cap across them all, build a
   *  `SharedExecutionBudget` and call `.ec(...)` per service instead. */
  def boundedEC(name: String, maxConcurrent: Int): ExecutionContextExecutorService =
    new SharedExecutionBudget(maxConcurrent).ec(name)

  /** Wrap `underlying` so every submitted task acquires a permit from
   *  `permits` before running and releases it after — capping how many run at
   *  once to the semaphore's size. Lifecycle (`shutdown`/`awaitTermination`/…)
   *  delegates straight to `underlying`. Shared by [[boundedEC]] and
   *  [[SharedExecutionBudget]] so the gating logic lives in one place. */
  private[tools] def semaphoreGated(underlying: ExecutorService, permits: Semaphore): ExecutorService =
    new AbstractExecutorService {
      override def execute(command: Runnable): Unit = underlying.execute { () =>
        permits.acquire()
        try command.run() finally permits.release()
      }
      override def shutdown(): Unit                        = underlying.shutdown()
      override def shutdownNow(): java.util.List[Runnable] = underlying.shutdownNow()
      override def isShutdown: Boolean                     = underlying.isShutdown
      override def isTerminated: Boolean                   = underlying.isTerminated
      override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
        underlying.awaitTermination(timeout, unit)
    }

  private[tools] def virtualThreadExecutor(name: String): ExecutorService =
    Executors.newThreadPerTaskExecutor(Thread.ofVirtual().name(s"$name-", 0L).factory())

  /** Single-thread scheduled executor with a daemon platform thread named
   *  `name`. Schedulers stay platform-threaded — their job is firing a
   *  Runnable on a timer, and `Future`/EC has no stdlib equivalent for
   *  `scheduleAtFixedRate`. The scheduled Runnable typically just hands work
   *  back to a `virtualThreadEC` (`ec.execute(() => …)` or
   *  `Future(...)(ec)`), so the scheduler thread doesn't block on the work
   *  itself. */
  def scheduler(name: String): ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor { r =>
      val t = new Thread(r, name)
      t.setDaemon(true)
      t
    }
}

/**
 * A concurrency budget shared across several execution contexts. Every EC
 * handed out by [[ec]] is an independent, separately-shutdownable virtual-
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
final class SharedExecutionBudget(maxConcurrent: Int) {
  private val permits: Option[Semaphore] =
    if (maxConcurrent > 0) Some(new Semaphore(maxConcurrent)) else None

  /** A fresh virtual-thread EC named `${name}-N`, gated by this budget's shared
   *  permits. Shut it down independently of any sibling EC from the same
   *  budget. */
  def ec(name: String): ExecutionContextExecutorService = {
    val underlying = DaemonExecutors.virtualThreadExecutor(name)
    val gated      = permits.fold(underlying)(p => DaemonExecutors.semaphoreGated(underlying, p))
    ExecutionContext.fromExecutorService(gated)
  }
}
