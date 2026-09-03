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

  /** The shape that failed a local `itAll` on 2026-09-03: a POST to IMDb's GraphQL
   *  endpoint timed out, and `https://www.imdb.com/` — the probe — answered 2xx, so the
   *  timeout was rethrown and reddened the build. A request that never got a response
   *  says nothing about the contract, so the probe's opinion is irrelevant to it. */
  it should "cancel a transport failure without asking the probe at all" in {
    var probed = 0
    val cancelled = intercept[TestCanceledException] {
      fast(() => probed += 1)(throw new java.net.http.HttpConnectTimeoutException("HTTP connect timed out"))
    }
    cancelled.getMessage should include ("did not answer at the transport level")
    withClue("a response that never arrived cannot be judged by whether the site root is up: ")(
      probed shouldBe 0)
  }

  /** …and the distinction still holds in the other direction: a failure the CLIENT
   *  swallowed arrives as an ordinary assertion failure, which is exactly the case the
   *  probe exists to judge. */
  it should "still probe, and still fail, for a swallowed failure on a reachable upstream" in {
    var probed = 0
    intercept[RuntimeException] {
      fast(() => probed += 1)(throw new RuntimeException("canonicalUrl returned None"))
    }
    probed should be > 0
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
