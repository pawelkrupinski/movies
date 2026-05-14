package clients.scraping_ant

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ScrapingAntClient.{FetchResult, retryWhileUnusable}

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
    val r = retryWhileUnusable(3, "test") {
      calls += 1
      FetchResult(200, """{"ok":true}""")
    }
    r.body shouldBe """{"ok":true}"""
    calls  shouldBe 1
  }

  it should "retry on a 200/empty-body response and return when the next attempt succeeds" in {
    var calls = 0
    val r = retryWhileUnusable(3, "test") {
      calls += 1
      if (calls == 1) FetchResult(200, "")  // mimics the production failure
      else            FetchResult(200, """{"result":[]}""")
    }
    r.body shouldBe """{"result":[]}"""
    calls  shouldBe 2
  }

  it should "retry on an HTML interstitial and return when canonical JSON comes back" in {
    var calls = 0
    val r = retryWhileUnusable(3, "test") {
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
      retryWhileUnusable(3, "ScrapingAnt") {
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
      retryWhileUnusable(2, "ScrapingAnt") {
        calls += 1
        FetchResult(503, "Service Unavailable")
      }
    }
    ex.getMessage should include ("status=503")
    ex.getMessage should include ("body=19B")
    ex.getMessage should include ("Service Unavailable")
  }
}
