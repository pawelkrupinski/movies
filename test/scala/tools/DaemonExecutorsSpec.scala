package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, TimeUnit}

class DaemonExecutorsSpec extends AnyFlatSpec with Matchers {

  /** Run `n` tasks across the given executors, each recording the peak number
   *  observed running at once, and return that peak. Every task overlaps the
   *  others (it sleeps while holding its slot), so an uncapped executor reaches
   *  `n` and a capped one tops out at the cap. */
  private def peakConcurrency(n: Int, executors: IndexedSeq[java.util.concurrent.Executor]): Int = {
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
    done.await(30, TimeUnit.SECONDS) shouldBe true
    maxActive.get()
  }

  "SharedExecutionBudget" should "cap total concurrency across every EC it hands out" in {
    val budget = new SharedExecutionBudget(2)
    // Two distinct ECs from the same budget — work submitted to either still
    // shares the one permit pool, so the combined peak can't exceed 2.
    val peak = peakConcurrency(12, IndexedSeq(budget.ec("a"), budget.ec("b")))
    peak should be <= 2
  }

  it should "run unbounded when maxConcurrent <= 0" in {
    val budget = new SharedExecutionBudget(0)
    // No permit gate: all 8 overlapping tasks run at once.
    val peak = peakConcurrency(8, IndexedSeq(budget.ec("unbounded")))
    peak should be > 2
  }

  "boundedEC" should "cap concurrency for a single EC" in {
    val peak = peakConcurrency(10, IndexedSeq(DaemonExecutors.boundedEC("bounded", 3)))
    peak should be <= 3
  }
}
