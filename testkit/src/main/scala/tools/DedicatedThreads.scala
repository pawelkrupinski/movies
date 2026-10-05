package tools

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{ExecutionContext, Future}

/**
 * An executor that gives every task a thread of its own, for a test whose work must start the
 * moment it is submitted because the spec then waits a bounded time for it.
 *
 * Not `ExecutionContext.global`: other suites in the same test JVM keep that pool busy (a few-core
 * CI runner has only a few threads in it), so work queued there can start after a spec's wait has
 * already run out — the spec then times the pool's queue, not the code. `NoGlobalPoolInTestsSpec`
 * keeps `global` out of test sources.
 */
object DedicatedThreads extends ExecutionContext {
  private val started = new AtomicInteger(0)

  def execute(task: Runnable): Unit = {
    val thread = new Thread(task, s"dedicated-test-thread-${started.incrementAndGet()}")
    thread.setDaemon(true)
    thread.start()
  }

  def reportFailure(cause: Throwable): Unit = cause.printStackTrace()

  /** `body` on a thread of its own, as a Future. */
  def future[A](body: => A): Future[A] = Future(body)(using this)
}
