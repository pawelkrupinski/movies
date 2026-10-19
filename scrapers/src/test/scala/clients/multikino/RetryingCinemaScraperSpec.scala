package clients.multikino

import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ScriptedCinemaScraper
import services.cinemas.ScriptedCinemaScraper.OneMovie
import services.cinemas.common.RetryingCinemaScraper

import scala.concurrent.duration._

/**
 * RetryingCinemaScraper is now retry-ONLY — recording the outcome moved to
 * UptimeRecordingScraper (see UptimeRecordingScraperSpec). These specs cover the
 * retry/backoff contract: happy path, recover-within-the-tick, and rethrow when
 * every attempt is exhausted.
 */
class RetryingCinemaScraperSpec extends AnyFlatSpec with Matchers {

  "RetryingCinemaScraper" should "delegate to the underlying scraper on the happy path" in {
    val s = new RetryingCinemaScraper(ScriptedCinemaScraper(List(Right(OneMovie))), initialBackoff = 1.millis)
    s.fetch() shouldBe OneMovie
  }

  it should "preserve the wrapped scraper's cinema identity (so list lookups still work)" in {
    val s = new RetryingCinemaScraper(ScriptedCinemaScraper(List(Right(OneMovie))), initialBackoff = 1.millis)
    s.cinema shouldBe Multikino
  }

  it should "retry on a transient failure and return the next success" in {
    val s = new RetryingCinemaScraper(
      ScriptedCinemaScraper(List(Left(new RuntimeException("blip")), Right(OneMovie))),
      initialBackoff = 1.millis
    )
    s.fetch() shouldBe OneMovie
  }

  it should "rethrow the last exception when every retry fails" in {
    val s = new RetryingCinemaScraper(
      ScriptedCinemaScraper(List.fill(3)(Left(new RuntimeException("down")))),
      maxAttempts    = 3,
      initialBackoff = 1.millis
    )
    intercept[RuntimeException] { s.fetch() }.getMessage shouldBe "down"
  }
}
