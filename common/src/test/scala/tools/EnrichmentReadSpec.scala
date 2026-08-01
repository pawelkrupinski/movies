package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException

/**
 * The line between "the upstream answered, and the answer is nothing" and "the
 * read never happened". Every rating client used to collapse both into `None`,
 * so a total upstream outage was indistinguishable from a film that genuinely
 * has no rating — the shape that let IMDb's 403 block run ~47h unnoticed on
 * 2026-07-30 while `RatingHandler` recorded each failed refresh as a successful
 * "unchanged" check. 404/410 stay data; everything else must reach the caller.
 */
class EnrichmentReadSpec extends AnyFlatSpec with Matchers {

  private def status(code: Int) = new HttpStatusException(code, "GET", "https://x/y", None)

  "absentOnNotFound" should "pass a successful read through" in {
    EnrichmentRead.absentOnNotFound("body") shouldBe Some("body")
  }

  it should "treat 404 and 410 as a real answer of 'nothing here'" in {
    // The RT and Metacritic slug probes rely on this: they try candidate URLs and
    // read a 404 as "that slug doesn't exist", which is information, not a fault.
    EnrichmentRead.absentOnNotFound(throw status(404)) shouldBe None
    EnrichmentRead.absentOnNotFound(throw status(410)) shouldBe None
  }

  it should "rethrow a block, a throttle, or a server error — the read did not happen" in {
    // 403 is THE case this exists for: IMDb's CDN refusing us wholesale.
    a[HttpStatusException] should be thrownBy EnrichmentRead.absentOnNotFound(throw status(403))
    a[HttpStatusException] should be thrownBy EnrichmentRead.absentOnNotFound(throw status(429))
    a[HttpStatusException] should be thrownBy EnrichmentRead.absentOnNotFound(throw status(500))
    a[HttpStatusException] should be thrownBy EnrichmentRead.absentOnNotFound(throw status(503))
  }

  it should "rethrow a transport failure that never reached the server" in {
    an[IOException] should be thrownBy EnrichmentRead.absentOnNotFound(throw new IOException("reset"))
    a[java.net.http.HttpTimeoutException] should be thrownBy
      EnrichmentRead.absentOnNotFound(throw new java.net.http.HttpTimeoutException("slow"))
  }

  it should "rethrow an unexpected non-HTTP failure rather than hiding it as 'no rating'" in {
    a[RuntimeException] should be thrownBy EnrichmentRead.absentOnNotFound(throw new RuntimeException("boom"))
  }

  // HttpStatusException keeps the `HTTP <code> for <method> <url>` message shape
  // that predates it, and MonitoringHttpFetch's classifier plus every client fake
  // match on that shape. A status carried in the message must therefore classify
  // exactly like the typed one — otherwise the RT/Metacritic slug ladders, whose
  // whole job is to read a 404 as "no such page", would abort on their first miss.
  it should "read a status carried in the message, not just the typed exception" in {
    EnrichmentRead.absentOnNotFound(throw new RuntimeException("HTTP 404")) shouldBe None
    EnrichmentRead.absentOnNotFound(throw new RuntimeException("HTTP 404 for GET https://x/y")) shouldBe None
    a[RuntimeException] should be thrownBy
      EnrichmentRead.absentOnNotFound(throw new RuntimeException("HTTP 403 for GET https://x/y"))
    a[RuntimeException] should be thrownBy
      EnrichmentRead.absentOnNotFound(throw new RuntimeException("HTTP 503"))
  }

  it should "not mistake an unrelated message that merely mentions a number for a status" in {
    a[RuntimeException] should be thrownBy
      EnrichmentRead.absentOnNotFound(throw new RuntimeException("connection reset after 404 bytes"))
  }

  it should "agree with the metrics taxonomy about which codes are a failure" in {
    // One definition of "blocked", not two that can drift: anything HttpOutcome
    // classifies as 403/429/5xx must be rethrown, and 404/410 must not be.
    Seq(403, 429, 500, 503).foreach { code =>
      withClue(s"$code should rethrow: ") {
        a[HttpStatusException] should be thrownBy EnrichmentRead.absentOnNotFound(throw status(code))
      }
    }
    Seq(404, 410).foreach { code =>
      withClue(s"$code should be absent: ") {
        EnrichmentRead.absentOnNotFound(throw status(code)) shouldBe None
      }
    }
  }
}
