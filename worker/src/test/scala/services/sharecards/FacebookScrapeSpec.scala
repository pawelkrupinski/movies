package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** How a Graph API answer to one re-scrape is read: the rate limit stops the whole fleet, anything
 *  else fails the one page — and either way the reason names Facebook's own code and message, which
 *  the failures of 2026-09-25 did not (they logged "HTTP 403" and nothing else). */
class FacebookScrapeSpec extends AnyFlatSpec with Matchers {

  private def fixture(name: String): String =
    new String(Files.readAllBytes(Paths.get(getClass.getResource(s"/fixtures/facebook-graph/$name").toURI)), StandardCharsets.UTF_8)

  "A 2xx" should "be accepted" in {
    FacebookScrape.of(200, """{"id":"https://kinowo.net/"}""") shouldBe FacebookScrape.Accepted
  }

  // Recorded 2026-09-26: an unauthenticated scrape POST to graph.facebook.com.
  "A Graph API error" should "be refused, with its code and message kept" in {
    FacebookScrape.of(400, fixture("scrape-without-token-400.json")) shouldBe
      FacebookScrape.Refused("HTTP 400 code 100 (#100) Must have a valid access token or a valid url_hmac")
  }

  "Facebook's rate-limit codes, and a 429" should "be the rate limit" in {
    FacebookScrape.RateLimitCodes.foreach { code =>
      FacebookScrape.of(403, fixture("scrape-without-token-400.json").replace("\"code\":100", s"\"code\":$code")) shouldBe a[FacebookScrape.RateLimited]
    }
    FacebookScrape.of(429, "") shouldBe FacebookScrape.RateLimited("HTTP 429")
  }

  "A body that is not the Graph API's error" should "be refused with the status alone" in {
    FacebookScrape.of(403, "<html>Forbidden</html>") shouldBe FacebookScrape.Refused("HTTP 403")
  }
}
