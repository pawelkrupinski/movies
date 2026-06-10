package services.cinemas

import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.ScriptedCinemaScraper.{NoShowtimes, OneMovie}

import scala.concurrent.duration._

/**
 * UptimeRecordingScraper records the scrape's outcome under the cinema's
 * displayName: success / empty / failure. These specs assert the classification
 * directly, plus one composition test proving the retry+record split preserves
 * the original "a retry-recovered blip is green, not yellow" behaviour.
 */
class UptimeRecordingScraperSpec extends AnyFlatSpec with Matchers {

  "UptimeRecordingScraper" should "record a success (green) when the scrape returns screenings" in {
    val monitor = new UptimeMonitor()
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(OneMovie))), monitor).fetch() shouldBe OneMovie
    val bucket = monitor.history(Multikino.displayName).head
    bucket.successes shouldBe 1
    bucket.failures  shouldBe 0
    bucket.status    shouldBe "green"
  }

  it should "record an empty (not a success) when the scrape returns no movies" in {
    val monitor = new UptimeMonitor()
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(Seq.empty))), monitor).fetch() shouldBe empty
    val bucket = monitor.history(Multikino.displayName).head
    bucket.successes shouldBe 0
    bucket.zeroes    shouldBe 1
    bucket.status    shouldBe "zero"
  }

  it should "record an empty when movies come back with zero showtimes" in {
    val monitor = new UptimeMonitor()
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(NoShowtimes))), monitor).fetch() shouldBe NoShowtimes
    val bucket = monitor.history(Multikino.displayName).head
    bucket.successes shouldBe 0
    bucket.zeroes    shouldBe 1
  }

  it should "record a failure (red) and rethrow when the scrape throws" in {
    val monitor = new UptimeMonitor()
    val s = new UptimeRecordingScraper(ScriptedCinemaScraper(List(Left(new RuntimeException("down")))), monitor)
    intercept[RuntimeException] { s.fetch() }.getMessage shouldBe "down"
    val bucket = monitor.history(Multikino.displayName).head
    bucket.successes shouldBe 0
    bucket.failures  shouldBe 1
    bucket.errors.head should include ("down")
    bucket.status    shouldBe "red"
  }

  // The split's regression guard: retry swallows the blip and returns success,
  // so the recorder sees only the green outcome — no yellow bar for a recovered
  // tick, exactly as the pre-split single class did.
  it should "record only a success when an inner retry recovers within the tick" in {
    val monitor = new UptimeMonitor()
    val s = new UptimeRecordingScraper(
      new RetryingCinemaScraper(
        ScriptedCinemaScraper(List(Left(new RuntimeException("blip")), Right(OneMovie))),
        initialBackoff = 1.millis
      ),
      monitor
    )
    s.fetch() shouldBe OneMovie
    val bucket = monitor.history(Multikino.displayName).head
    bucket.successes shouldBe 1
    bucket.failures  shouldBe 0
    bucket.errors    shouldBe empty
    bucket.status    shouldBe "green"
  }
}
