package clients.scraping_ant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ScrapingAntClient.{FetchResult, retryWhileUnusable}

import scala.concurrent.duration._

/**
 * Unit tests for the generic ScrapingAnt wrapper's retry semantics.
 *
 * Regression: production logs showed ScrapingAnt returning 200 with `body=0B`
 * for the Multikino API call, which used to throw on the first failure. The
 * fix is `retryWhileUnusable` — retry the whole proxy session (fresh cookies
 * + fresh API call) before giving up.
 *
 * Tests work at the `FetchResult` / `retryWhileUnusable` level so we don't
 * have to mock `HttpClient`.
 */
class ScrapingAntClientSpec extends AnyFlatSpec with Matchers {

  "FetchResult.isUsable" should "accept a 200 with JSON body" in {
    FetchResult(200, """{"result":[]}""").isUsable shouldBe true
  }

  it should "accept a 200 with leading whitespace before the JSON" in {
    FetchResult(200, "  \n{}").isUsable shouldBe true
  }

  it should "reject an empty body even when status is 200 (the bug we're fixing)" in {
    FetchResult(200, "").isUsable shouldBe false
  }

  it should "reject an HTML interstitial body" in {
    FetchResult(200, "<!DOCTYPE html><html>...</html>").isUsable shouldBe false
  }

  it should "reject non-200 even when the body looks like JSON" in {
    FetchResult(503, """{"error":"unavailable"}""").isUsable shouldBe false
  }

  "retryWhileUnusable" should "return the first usable response without retrying" in {
    var calls = 0
    val r = retryWhileUnusable(3, "test", Duration.Zero) {
      calls += 1
      FetchResult(200, """{"ok":true}""")
    }
    r.body shouldBe """{"ok":true}"""
    calls  shouldBe 1
  }

  it should "retry on a 200/empty-body response and return when the next attempt succeeds" in {
    var calls = 0
    val r = retryWhileUnusable(3, "test", Duration.Zero) {
      calls += 1
      if (calls == 1) FetchResult(200, "")  // mimics the production failure
      else            FetchResult(200, """{"result":[]}""")
    }
    r.body shouldBe """{"result":[]}"""
    calls  shouldBe 2
  }

  it should "retry on an HTML interstitial and return when canonical JSON comes back" in {
    var calls = 0
    val r = retryWhileUnusable(3, "test", Duration.Zero) {
      calls += 1
      if (calls < 3) FetchResult(200, "<!DOCTYPE html><html>blocked</html>")
      else           FetchResult(200, "{}")
    }
    r.body shouldBe "{}"
    calls  shouldBe 3
  }

  it should "throw after maxAttempts when every response is unusable" in {
    var calls = 0
    val ex = intercept[RuntimeException] {
      retryWhileUnusable(3, "ScrapingAnt", Duration.Zero) {
        calls += 1
        FetchResult(200, "")
      }
    }
    calls shouldBe 3
    ex.getMessage should include ("ScrapingAnt")
    ex.getMessage should include ("after 3 attempts")
    ex.getMessage should include ("body=0B")
  }

  it should "include the latest unusable response's diagnostics in the error message" in {
    var calls = 0
    val ex = intercept[RuntimeException] {
      retryWhileUnusable(2, "ScrapingAnt", Duration.Zero) {
        calls += 1
        FetchResult(503, "Service Unavailable")
      }
    }
    ex.getMessage should include ("status=503")
    ex.getMessage should include ("body=19B")
    ex.getMessage should include ("Service Unavailable")
  }

  // Regression: ScrapingAnt's free tier returns 409 "Free user concurrency
  // limit reached" when overlapping requests trip its metering. With
  // zero-delay retries all three attempts land in the same window and fail.
  // The fix is exponential backoff between retries — the test below proves
  // it (a) waits at least the initial backoff before the second attempt and
  // (b) doubles on each subsequent failure.
  "retryWhileUnusable backoff" should "wait at least initialBackoff between attempts" in {
    val backoff   = 50.millis
    var calls     = 0
    val timestamps = scala.collection.mutable.ListBuffer.empty[Long]
    val r = retryWhileUnusable(3, "test", backoff) {
      calls += 1
      timestamps += System.currentTimeMillis()
      if (calls < 3) FetchResult(409, """{"detail":"Free user concurrency limit reached"}""")
      else           FetchResult(200, "{}")
    }
    r.body shouldBe "{}"
    calls  shouldBe 3
    // Gap before attempt 2 is the initial backoff; gap before attempt 3 is
    // doubled. Allow small wiggle room for scheduling jitter.
    val gap1 = timestamps(1) - timestamps.head
    val gap2 = timestamps(2) - timestamps(1)
    gap1 should be >= 45L
    gap2 should be >= 95L  // ~2× initialBackoff
  }

  it should "not sleep after the final attempt (no wasted wait)" in {
    val started = System.currentTimeMillis()
    intercept[RuntimeException] {
      retryWhileUnusable(2, "test", 200.millis) {
        FetchResult(409, "rate limit")
      }
    }
    val elapsed = System.currentTimeMillis() - started
    // 2 attempts → exactly 1 backoff in between (≥200ms) and no trailing wait.
    // Generous upper bound to keep this stable on a busy CI.
    elapsed should (be >= 200L and be < 800L)
  }

  it should "skip sleeping entirely when initialBackoff is zero" in {
    val started = System.currentTimeMillis()
    intercept[RuntimeException] {
      retryWhileUnusable(5, "test", Duration.Zero) {
        FetchResult(200, "")
      }
    }
    val elapsed = System.currentTimeMillis() - started
    elapsed should be < 200L
  }
}
