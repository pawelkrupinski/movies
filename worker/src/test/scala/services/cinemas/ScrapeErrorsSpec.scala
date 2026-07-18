package services.cinemas

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.ScrapeErrors

import java.io.IOException
import java.net.SocketTimeoutException
import java.util.concurrent.TimeoutException

/** Drives the WARN-vs-ERROR (Sentry-vs-silent) classification that
  * `ScrapeCinemaHandler` applies to a failed scrape. Transient external/runtime
  * failures must NOT reach Sentry; only genuinely actionable errors should. */
class ScrapeErrorsSpec extends AnyFlatSpec with Matchers {

  "isTransientHttpError" should "classify network / timeout failures as transient" in {
    ScrapeErrors.isTransientHttpError(new IOException("connection reset")) shouldBe true
    ScrapeErrors.isTransientHttpError(new SocketTimeoutException("read timed out")) shouldBe true
    ScrapeErrors.isTransientHttpError(new TimeoutException("Future timed out")) shouldBe true
  }

  // Regression for the six Sentry `InterruptedException` issues: a scrape's
  // blocking `Await` interrupted by worker shutdown / task cancellation is
  // operational, not actionable — it must log WARN, not ERROR.
  it should "treat an interrupted blocking scrape Await as transient" in {
    ScrapeErrors.isTransientHttpError(new InterruptedException()) shouldBe true
  }

  it should "classify upstream HTTP-status and all-backends-failed messages as transient" in {
    ScrapeErrors.isTransientHttpError(new RuntimeException("HTTP 503 for GET https://cinema/x")) shouldBe true
    ScrapeErrors.isTransientHttpError(
      new RuntimeException("All 3 backends failed for GET https://cinema/x")) shouldBe true
  }

  it should "leave a genuine programming error as actionable (ERROR / Sentry)" in {
    ScrapeErrors.isTransientHttpError(new NullPointerException()) shouldBe false
    ScrapeErrors.isTransientHttpError(new IllegalStateException("parser invariant broken")) shouldBe false
  }
}
