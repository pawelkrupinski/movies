package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, Executors}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

class DrainablePoolSpec extends AnyFlatSpec with Matchers {

  private def singleThreadPool() = new DrainablePool(ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor()))

  // A shutdown drain used to wait for every SUBMITTED task, queued ones included, however long the
  // backlog: past the pod's 30 s grace to the SIGKILL, skipping every close after it. What is queued
  // at shutdown is recomputable, so the drain gets a deadline and the rest is dropped, counted.
  "stop" should "drain within its budget, then abandon the backlog and say how much" in {
    val never = new CountDownLatch(1)
    val pool  = singleThreadPool()
    (1 to 4).foreach(_ => pool.submit(never.await())) // one running, three queued behind it
    val stopwatch = Stopwatch.System.start()

    val dropped = pool.stop(within = 200.millis)

    stopwatch.elapsed should be < 2.seconds
    dropped shouldBe 4
  }

  it should "drop nothing when the work finishes inside the budget" in {
    val pool = singleThreadPool()
    (1 to 3).foreach(_ => pool.submit(()))
    pool.stop(within = 5.seconds) shouldBe 0
  }
}
