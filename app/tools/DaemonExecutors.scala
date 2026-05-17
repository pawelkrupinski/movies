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
  def virtualThreadEC(name: String): ExecutionContextExecutorService = {
    val factory = Thread.ofVirtual().name(s"$name-", 0L).factory()
    ExecutionContext.fromExecutorService(Executors.newThreadPerTaskExecutor(factory))
  }

  /** Virtual-thread EC capped at `maxConcurrent` tasks running at once. Use
   *  when the upstream rate-limits or load-tests poorly past a small number
   *  of concurrent requests (Filmweb, scraped detail pages, …). Each task
   *  parks on a [[Semaphore]] before running; virtual threads park cheaply,
   *  so blocked tasks cost nothing. Falls back to plain `virtualThreadEC`
   *  semantics when `maxConcurrent <= 0`. */
  def boundedEC(name: String, maxConcurrent: Int): ExecutionContextExecutorService = {
    val underlying = Executors.newThreadPerTaskExecutor(
      Thread.ofVirtual().name(s"$name-", 0L).factory()
    )
    val permits = new Semaphore(maxConcurrent)
    val wrapped: ExecutorService = new AbstractExecutorService {
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
    ExecutionContext.fromExecutorService(wrapped)
  }

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
