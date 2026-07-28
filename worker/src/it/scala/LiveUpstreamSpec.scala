package integration

import org.scalatest.exceptions.TestCanceledException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

/**
 * The whole value of [[LiveUpstream]] is which failures it lets through, so both
 * directions are pinned: a reachable upstream must still be able to fail the build.
 */
class LiveUpstreamSpec extends AnyFlatSpec with Matchers {

  private val clock = new FakeClock(0L)
  private def fast[T](probe: () => Unit)(body: => T): T =
    LiveUpstream.orCancel("Test upstream", probe, totalBudget = 1.second,
      sleep = ms => clock.advance(ms), now = () => clock.now())(body)

  "LiveUpstream" should "return the body's value and never probe when it succeeds" in {
    var probed = 0
    fast(() => probed += 1)("ok") shouldBe "ok"
    probed shouldBe 0
  }

  // The regression path. A rate limiter must not be able to hide a contract change:
  // if the upstream is answering, the assertion's own failure is the truth.
  it should "rethrow the original failure when the upstream IS answering" in {
    val boom = intercept[RuntimeException] {
      fast(() => ())(throw new RuntimeException("canonicalUrl returned None"))
    }
    boom.getMessage shouldBe "canonicalUrl returned None"
  }

  it should "cancel, not fail, when the upstream is NOT answering" in {
    val cancelled = intercept[TestCanceledException] {
      fast(() => throw new java.net.ConnectException("connect timed out"))(
        throw new RuntimeException("canonicalUrl returned None"))
    }
    cancelled.getMessage should include ("Test upstream is not answering")
    cancelled.getMessage should include ("ConnectException")
    withClue("the cancel must carry the assertion's own failure, or a real break is invisible: ")(
      cancelled.getMessage should include ("canonicalUrl returned None"))
  }

  // A flaky upstream that recovers within the budget never reaches the probe at all —
  // this is the burst case `RetryWithBackoff` already handled, and it must keep working.
  it should "still absorb a transient failure that recovers inside the budget" in {
    var calls = 0
    var probed = 0
    val result = fast(() => probed += 1) {
      calls += 1
      if (calls < 3) throw new RuntimeException("502") else "recovered"
    }
    result shouldBe "recovered"
    calls  shouldBe 3
    probed shouldBe 0
  }
}
