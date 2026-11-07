package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class HttpStatusExceptionSpec extends AnyFlatSpec with Matchers {

  "the message" should "keep the legacy shape so MonitoringHttpFetch's 5xx classifier still matches" in {
    val e = new HttpStatusException(503, "GET", "http://x/y", None)
    e.getMessage shouldBe "HTTP 503 for GET http://x/y"
    e.getMessage.matches("HTTP 5\\d\\d .*") shouldBe true   // the classifier's regex
  }

  "parseRetryAfter" should "read the delta-seconds form" in {
    HttpStatusException.parseRetryAfter(Some("120")) shouldBe Some(120.seconds)
    HttpStatusException.parseRetryAfter(Some(" 30 ")) shouldBe Some(30.seconds)
    HttpStatusException.parseRetryAfter(Some("0")) shouldBe Some(0.seconds)
  }

  it should "return None for absent, empty or negative values" in {
    HttpStatusException.parseRetryAfter(None) shouldBe None
    HttpStatusException.parseRetryAfter(Some("")) shouldBe None
    HttpStatusException.parseRetryAfter(Some("-5")) shouldBe None
  }

  it should "read the HTTP-date form against the response's own Date header" in {
    // Measured from the server's clock, not ours: no local clock read, and no skew between the two.
    HttpStatusException.parseRetryAfter(Some("Tue, 21 Oct 2025 07:28:00 GMT"), Some("Tue, 21 Oct 2025 07:27:15 GMT")) shouldBe Some(45.seconds)
    HttpStatusException.parseRetryAfter(Some("Tue, 21 Oct 2025 07:28:00 GMT"), Some("Tue, 21 Oct 2025 07:29:00 GMT")) shouldBe Some(0.seconds)
    HttpStatusException.parseRetryAfter(Some("Tue, 21 Oct 2025 07:28:00 GMT"), None) shouldBe None
    HttpStatusException.parseRetryAfter(Some("not a date"), Some("Tue, 21 Oct 2025 07:27:15 GMT")) shouldBe None
  }
}
