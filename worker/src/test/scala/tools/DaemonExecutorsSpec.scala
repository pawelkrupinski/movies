package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.Promise

class DaemonExecutorsSpec extends AnyFlatSpec with Matchers {

  "boundedEC" should "cap concurrency for a single EC" in {
    val peak = ExecutorProbes.peakConcurrency(10, IndexedSeq(DaemonExecutors.boundedEC("bounded", 3)))
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
    val executionContext      = DaemonExecutors.boundedEC("storm", 1)
    val running = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val printed = captureStdErr {
      val p      = Promise[Int]()
      val stage1 = p.future.map { x => running.countDown(); release.await(); x }(using executionContext)
      stage1.onComplete(_ => ())(using executionContext) // terminal: submit fires from stage1's run(), no
                                           // downstream to capture the failure → it's reported
      p.success(1)                         // schedules stage1 onto executionContext
      running.await(5, TimeUnit.SECONDS) shouldBe true
      executionContext.shutdown()                        // pool drained while stage1 is still running
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
