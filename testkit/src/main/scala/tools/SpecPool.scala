package tools

import java.util.concurrent.ExecutorService
import scala.concurrent.ExecutionContext

/** A spec's own thread pool, for as long as `body` runs: `threads` daemon threads named `name`, built through
 *  [[DaemonExecutors.boundedPool]] (its queue bounded, a producer that fills it waiting for room, as a fixed pool's
 *  submit never runs a task on the caller), and shut down — its running tasks interrupted — when `body` returns or
 *  throws, so no spec leaves a pool's threads behind (each hand-rolled `try … finally pool.shutdown()` used to, the
 *  moment one forgot the `finally`). */
object SpecPool {
  /** Tasks a spec may have waiting at once; far above any spec's fan-out. */
  private val QueueCapacity = 100_000

  def using[A](threads: Int, name: String = "spec-pool")(body: ExecutorService => A): A = {
    val pool = DaemonExecutors.boundedPool(name, threads, QueueCapacity, WhenFull.WaitForRoom)
    try body(pool) finally { pool.shutdownNow(); () }
  }

  /** [[using]], the pool handed over as an `ExecutionContext` (`Future`s, `ParallelTasks.all`). */
  def usingContext[A](threads: Int, name: String = "spec-pool")(body: ExecutionContext => A): A =
    using(threads, name)(pool => body(ExecutionContext.fromExecutor(pool)))
}
