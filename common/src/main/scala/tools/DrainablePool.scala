package tools

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{ExecutionContextExecutorService, Future}

/**
 * An execution context plus the in-flight bookkeeping that tells when it has gone
 * quiet — so a caller can wait for the work to finish WITHOUT ending the pool.
 *
 * `ExecutorService` offers only the second half of that: `shutdown()` +
 * `awaitTermination` waits, and then permanently rejects everything after. Both
 * services that own an inline enrichment pool wanted the first half and had only
 * the second, so both spelt the same `shutdown(); while (!isTerminated) …` and both
 * inherited its one-way-door behaviour — see [[services.Drainable]] for what that
 * cost the replay harness.
 *
 * The counter is incremented on the SUBMITTING thread, before the task can start, so
 * a `drain()` racing a `submit()` either sees the work or happens strictly before it
 * was offered. Work that enqueues further work onto the SAME pool is covered too: the
 * child increments before the parent decrements. Work that hands off to a DIFFERENT
 * pool is not, which is why the drain order (publishers before subscribers) still
 * matters and is still the caller's to choose.
 */
final class DrainablePool(executionContext: ExecutionContextExecutorService) {

  private val inFlight = new AtomicInteger(0)

  /** Run `body` on the pool, counted so [[drain]] can wait for it. */
  def submit(body: => Unit): Unit = {
    inFlight.incrementAndGet()
    Future(try body finally { inFlight.decrementAndGet(); () })(using executionContext)
    ()
  }

  /** Block until nothing submitted here is still running. The pool stays usable. */
  def drain(): Unit = while (inFlight.get() > 0) Thread.sleep(PollInterval.toMillis)

  /** Drain, then end the pool. After this every [[submit]] is rejected. */
  def stop(): Unit = {
    drain()
    executionContext.shutdown()
    while (!executionContext.isTerminated) executionContext.awaitTermination(1, TimeUnit.HOURS)
  }

  /** Short enough that a drain of fast work isn't dominated by the wait, long enough
   *  that a drain of a real network lookup doesn't spin a core doing nothing. */
  private val PollInterval = scala.concurrent.duration.Duration(5, TimeUnit.MILLISECONDS)
}
