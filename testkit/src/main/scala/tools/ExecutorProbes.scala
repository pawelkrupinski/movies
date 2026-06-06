package tools

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executor, TimeUnit}

/** Shared concurrency probe for executor specs. Run `n` overlapping tasks
 *  across the given executors, each recording the peak number observed running
 *  at once, and return that peak. Every task sleeps while holding its slot, so
 *  an uncapped executor reaches `n` and a capped one tops out at the cap.
 *
 *  Lives in common test so both [[DaemonExecutorsSpec]] (common) and the
 *  worker's `SharedExecutionBudgetSpec` reuse it instead of each re-spelling the
 *  latch/peak bookkeeping. */
object ExecutorProbes {

  def peakConcurrency(n: Int, executors: IndexedSeq[Executor]): Int = {
    val active    = new AtomicInteger(0)
    val maxActive = new AtomicInteger(0)
    val done      = new CountDownLatch(n)
    (0 until n).foreach { i =>
      executors(i % executors.size).execute { () =>
        val cur = active.incrementAndGet()
        maxActive.updateAndGet(m => math.max(m, cur))
        try Thread.sleep(60) finally { active.decrementAndGet(); done.countDown() }
      }
    }
    if (!done.await(30, TimeUnit.SECONDS))
      throw new AssertionError(s"tasks did not finish within 30s (peak so far ${maxActive.get()})")
    maxActive.get()
  }
}
