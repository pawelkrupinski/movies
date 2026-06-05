package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.Promise

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

  // The shutdown race in `Wiring.stop`: a `Future` stage is mid-run on the pool
  // when the pool is shut down; as that stage completes it notifies a terminal
  // callback whose submit lands on the now-shut-down pool. A terminal callback
  // has no downstream promise to absorb the failure into, so the
  // RejectedExecutionException is handed to the EC's failure reporter, which
  // prints a stack trace — one per dangling continuation, the storm in the bug
  // report (in prod the reporter routes through logback, hence the timestamps).
  // `dropRejectedAfterShutdown` (wired into every DaemonExecutors EC) must drop
  // the submit at its source so nothing is ever reported: stderr stays clean.
  "an EC handed out by DaemonExecutors" should "not print a RejectedExecutionException when a Future stage completes after shutdown" in {
    val ec      = DaemonExecutors.boundedEC("storm", 1)
    val running = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val printed = captureStdErr {
      val p      = Promise[Int]()
      val stage1 = p.future.map { x => running.countDown(); release.await(); x }(using ec)
      stage1.onComplete(_ => ())(using ec) // terminal: submit fires from stage1's run(), no
                                           // downstream to capture the failure → it's reported
      p.success(1)                         // schedules stage1 onto ec
      running.await(5, TimeUnit.SECONDS) shouldBe true
      ec.shutdown()                        // pool drained while stage1 is still running
      release.countDown()                  // stage1 finishes → notifies the callback → submit rejected
      Thread.sleep(300)                    // let the reported/uncaught rejection surface
    }
    printed should not include "RejectedExecutionException"
  }

  /** Run `body` with `System.err` redirected to a buffer, restore it, and
   *  return everything written. Used to assert the shutdown-race storm no
   *  longer reaches stderr. */
  private def captureStdErr(body: => Any): String = {
    val captured = new ByteArrayOutputStream()
    val original = System.err
    System.setErr(new PrintStream(captured, true))
    try body
    finally System.setErr(original)
    captured.toString
  }
}
