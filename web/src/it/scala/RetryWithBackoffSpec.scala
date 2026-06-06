package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class RetryWithBackoffSpec extends AnyFlatSpec with Matchers {

  "RetryWithBackoff" should "return immediately when the block succeeds on the first try" in {
    val clock = new FakeClock(0L)
    val sleeps = ArrayBuffer.empty[Long]
    var calls = 0
    val result = RetryWithBackoff(sleep = sleeps += _, now = () => clock.now())({
      calls += 1
      "ok"
    })
    result   shouldBe "ok"
    calls    shouldBe 1
    sleeps   shouldBe empty
  }

  it should "retry on failure with exponentially growing delays" in {
    val clock = new FakeClock(0L)
    val sleeps = ArrayBuffer.empty[Long]
    var calls = 0
    val result = RetryWithBackoff(
      sleep = ms => { sleeps += ms; clock.advance(ms) },
      now = () => clock.now(),
    )({
      calls += 1
      if (calls < 4) throw new RuntimeException(s"fail $calls") else "ok"
    })
    result shouldBe "ok"
    calls  shouldBe 4
    sleeps.toSeq shouldBe Seq(100L, 200L, 400L)
  }

  it should "give up and rethrow once the total budget is exhausted" in {
    val clock = new FakeClock(0L)
    val sleeps = ArrayBuffer.empty[Long]
    var calls = 0
    val thrown = intercept[RuntimeException] {
      RetryWithBackoff(
        totalBudget = 1.second,
        sleep = ms => { sleeps += ms; clock.advance(ms) },
        now = () => clock.now(),
      )({
        calls += 1
        throw new RuntimeException(s"fail $calls")
      })
    }
    thrown.getMessage should startWith("fail ")
    // 100 + 200 + 400 = 700ms slept; next would-be 800ms is clamped to the
    // 300ms remaining; after that the budget is gone and the next failure
    // rethrows without sleeping.
    sleeps.toSeq shouldBe Seq(100L, 200L, 400L, 300L)
    calls        shouldBe 5
    clock.now() should be <= 1000L
  }

  it should "rethrow without sleeping when the very first call already exceeds the budget" in {
    val clock = new FakeClock(0L)
    val sleeps = ArrayBuffer.empty[Long]
    val thrown = intercept[RuntimeException] {
      RetryWithBackoff(
        totalBudget = 10.millis,
        sleep = ms => { sleeps += ms; clock.advance(ms) },
        now = () => { val t = clock.now(); clock.advance(50); t },
      )({
        throw new RuntimeException("boom")
      })
    }
    thrown.getMessage shouldBe "boom"
    sleeps shouldBe empty
  }

  private class FakeClock(private var t: Long) {
    def now(): Long = t
    def advance(ms: Long): Unit = t += ms
  }
}
