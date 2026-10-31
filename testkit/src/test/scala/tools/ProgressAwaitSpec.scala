package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import scala.concurrent.{Future, Promise, TimeoutException}

/** The waits are held by hand (`ready`), so the windows pass as the case says and no clock decides a verdict. */
class ProgressAwaitSpec extends AnyFlatSpec with Matchers {

  /** A `ready` that sees `f` incomplete for `windows` windows, the work moving `progress` once in each when `moves`,
   *  and completes `f` with `value` in the last. */
  private def held(windows: Int, moves: Boolean, progress: java.util.concurrent.atomic.AtomicLong, done: Promise[Int])
      : (Future[Int], FiniteDuration) => Boolean = {
    var waited = 0
    (_, _) => {
      waited += 1
      if (waited > windows) { done.trySuccess(42); true }
      else { if (moves) progress.incrementAndGet(); false }
    }
  }

  "a wait bounded by progress" should "outlast any number of windows while the work moves" in {
    val progress = new java.util.concurrent.atomic.AtomicLong()
    val done     = Promise[Int]()
    ProgressAwait.result("boot", done.future, 1.minute, () => progress.get)(held(25, moves = true, progress, done)) shouldBe 42
  }

  it should "fail, named, after one window in which nothing moved" in {
    val progress = new java.util.concurrent.atomic.AtomicLong()
    val done     = Promise[Int]()
    val failure  = intercept[TimeoutException] {
      ProgressAwait.result("boot", done.future, 1.minute, () => progress.get)(held(25, moves = false, progress, done))
    }
    failure.getMessage should include("boot made no progress in 1 minute")
  }

  it should "rethrow the work's own failure" in {
    val failed = Future.failed[Int](new IllegalStateException("pass p2 broke"))
    intercept[IllegalStateException](ProgressAwait.result("boot", failed, 1.minute, () => 0L)()).getMessage shouldBe "pass p2 broke"
  }

  it should "return at once what is already done" in {
    ProgressAwait.result("boot", Future.successful(7), 1.minute, () => 0L)() shouldBe 7
  }
}
