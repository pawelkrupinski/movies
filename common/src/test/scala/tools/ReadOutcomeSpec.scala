package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException

/**
 * The classifier every client reads through: only a TYPED 404/410 is an answer of
 * "nothing here"; every other failure — including a message that merely reads like a
 * 404 — is a failed read the caller must be told about.
 */
class ReadOutcomeSpec extends AnyFlatSpec with Matchers {
  import ReadOutcome._

  private def status(code: Int) = new HttpStatusException(code, "GET", "https://x/y?api_key=secret", None)

  "classify" should "read a typed 404 or 410 as absent, keeping the original status" in {
    Seq(404, 410).foreach { code =>
      val failure = status(code)
      ReadOutcome.of(throw failure) shouldBe Absent(AbsentReason.NotFound(failure))
    }
  }

  it should "read every other status as a failed read" in {
    (400 to 599).filterNot(Set(404, 410)).foreach { code =>
      ReadOutcome.of(throw status(code)) shouldBe a[Failed]
    }
  }

  it should "read transport failures and bugs as failed reads" in {
    ReadOutcome.of(throw new IOException("reset")) shouldBe a[Failed]
    ReadOutcome.of(throw new java.net.http.HttpTimeoutException("slow")) shouldBe a[Failed]
    ReadOutcome.of(throw new IllegalStateException("bug")) shouldBe a[Failed]
  }

  it should "not read an untyped message that says 404 as absent" in {
    // The old EnrichmentRead regex treated this as "not found"; the type now decides.
    ReadOutcome.of(throw new RuntimeException("HTTP 404 for GET https://x/y")) shouldBe a[Failed]
    ReadOutcome.isAbsent(new RuntimeException("HTTP 410")) shouldBe false
  }

  it should "see a 404 through a subclass of the typed status" in {
    val relayed = new HttpStatusException(404, "GET", "https://origin/x", None) {
      override def getMessage: String = "Zyte: upstream status=404"
    }
    ReadOutcome.isAbsent(relayed) shouldBe true
  }

  "of" should "let a fatal error escape rather than classify it" in {
    an[InterruptedException] should be thrownBy ReadOutcome.of(throw new InterruptedException("stop"))
  }

  "toOptionOrThrow" should "answer Some, read absent as None, and THROW a failure" in {
    Answered(1).toOptionOrThrow shouldBe Some(1)
    ReadOutcome.of(throw status(404)).toOptionOrThrow shouldBe None
    ReadOutcome.none("empty search").toOptionOrThrow shouldBe None
    val boom = status(503)
    the[HttpStatusException] thrownBy ReadOutcome.of(throw boom).toOptionOrThrow shouldBe boom
  }

  "required" should "rethrow an absence as its original status so the archive still sees HTTP 404" in {
    val gone = status(404)
    the[HttpStatusException] thrownBy ReadOutcome.of(throw gone).required shouldBe gone
    a[NoSuchElementException] should be thrownBy ReadOutcome.none("no venues").required
  }

  "explain" should "say why, with the url's credentials masked" in {
    ReadOutcome.of(throw status(404)).explain should (include("absent") and include("HTTP 404") and not include "secret")
    ReadOutcome.of(throw status(503)).explain should (include("failed") and include("HTTP 503"))
    ReadOutcome.none("[] from search").explain should include("[] from search")
  }

  "map and flatMap" should "transform only an answer" in {
    Answered(2).map(_ * 2) shouldBe Answered(4)
    Answered(2).flatMap(_ => ReadOutcome.none("x")) shouldBe ReadOutcome.none("x")
    val failed: ReadOutcome[Int] = ReadOutcome.of(throw status(500))
    failed.map(_ * 2) shouldBe failed
  }
}
