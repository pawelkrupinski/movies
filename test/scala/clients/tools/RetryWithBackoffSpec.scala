package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RetryWithBackoff

import scala.collection.mutable
import scala.concurrent.duration._

class RetryWithBackoffSpec extends AnyFlatSpec with Matchers {

  // Inject a no-op sleep so the spec runs in milliseconds, and capture
  // the requested wait durations so the backoff curve is verifiable
  // without depending on wall clock.
  private def withSleepLog(): (mutable.ListBuffer[Long], Long => Unit) = {
    val log = mutable.ListBuffer.empty[Long]
    (log, ms => { log += ms; () })
  }

  "RetryWithBackoff" should "return the block's value on first success without sleeping" in {
    val (sleeps, sleep) = withSleepLog()
    var calls = 0
    val r = RetryWithBackoff("t", maxAttempts = 3, initialBackoff = 1.second, sleep = sleep) {
      calls += 1
      "ok"
    }
    r       shouldBe "ok"
    calls   shouldBe 1
    sleeps shouldBe empty
  }

  it should "retry after a transient failure and return the next success" in {
    val (_, sleep) = withSleepLog()
    var calls = 0
    val r = RetryWithBackoff("t", maxAttempts = 3, initialBackoff = 1.second, sleep = sleep) {
      calls += 1
      if (calls == 1) throw new RuntimeException("blip") else "ok"
    }
    r     shouldBe "ok"
    calls shouldBe 2
  }

  it should "follow an exponential backoff between attempts (1×, 2×, 4×, …)" in {
    val (sleeps, sleep) = withSleepLog()
    intercept[RuntimeException] {
      RetryWithBackoff("t", maxAttempts = 4, initialBackoff = 100.millis, sleep = sleep) {
        throw new RuntimeException("always")
      }
    }
    // 4 attempts → 3 sleeps between them; doubling each step.
    sleeps.toSeq shouldBe Seq(100L, 200L, 400L)
  }

  it should "throw the LAST failure unchanged after exhausting attempts" in {
    val (_, sleep) = withSleepLog()
    val ex = intercept[IllegalStateException] {
      var n = 0
      RetryWithBackoff("t", maxAttempts = 3, initialBackoff = 1.millis, sleep = sleep) {
        n += 1
        throw new IllegalStateException(s"fail-$n")
      }
    }
    // The exception type and message of the third (last) failure surface
    // unchanged — caller's existing try/catch still sees the original
    // type, no wrapping layer.
    ex.getMessage shouldBe "fail-3"
  }

  it should "not sleep after the final attempt — no wasted wait when there's nothing left to retry" in {
    val (sleeps, sleep) = withSleepLog()
    intercept[RuntimeException] {
      RetryWithBackoff("t", maxAttempts = 2, initialBackoff = 500.millis, sleep = sleep) {
        throw new RuntimeException("always")
      }
    }
    // 2 attempts → exactly 1 sleep in between, no trailing sleep.
    sleeps.toSeq shouldBe Seq(500L)
  }

  it should "treat maxAttempts=1 as 'try once, don't retry'" in {
    val (sleeps, sleep) = withSleepLog()
    var calls = 0
    intercept[RuntimeException] {
      RetryWithBackoff("t", maxAttempts = 1, initialBackoff = 999.seconds, sleep = sleep) {
        calls += 1
        throw new RuntimeException("once")
      }
    }
    calls  shouldBe 1
    sleeps shouldBe empty
  }

  it should "refuse maxAttempts < 1 at runtime — that's a wiring bug, not a fallback" in {
    intercept[IllegalArgumentException] {
      RetryWithBackoff("t", maxAttempts = 0)(())
    }
  }
}
