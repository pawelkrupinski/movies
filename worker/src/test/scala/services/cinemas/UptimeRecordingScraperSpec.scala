package services.cinemas

import services.cinemas.ScriptedCinemaScraper.{NoShowtimes, OneMovie}
import org.scalatest.matchers.should.Matchers
import models.{Cinema, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import services.UptimeMonitor
import services.cinemas.common.{RetryingCinemaScraper, ScrapeOutcome, ScrapeOutcomeListener, UptimeRecordingScraper}

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

  // OneMovie screens 2026-06-10 18:00 (Warsaw). A green scrape whose screenings
  // all sit past the next 72 hours is still green, but its bucket is marked `thin`
  // so /uptime can show it — the Polonez shape (four pre-sale slots, nothing near).
  it should "mark a green scrape thin when none of its showtimes fall in the next 72 hours" in {
    val monitor = new UptimeMonitor()
    val fourDaysBefore = java.time.Clock.fixed(java.time.Instant.parse("2026-06-06T10:00:00Z"), java.time.ZoneOffset.UTC)
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(OneMovie))), monitor, clock = fourDaysBefore).fetch()
    val bucket = monitor.history(Multikino.displayName).head
    bucket.status shouldBe "green"
    bucket.thin   shouldBe true
  }

  it should "not mark a green scrape thin when a showtime falls in the next 72 hours" in {
    val monitor = new UptimeMonitor()
    val sameDay = java.time.Clock.fixed(java.time.Instant.parse("2026-06-10T08:00:00Z"), java.time.ZoneOffset.UTC)
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(OneMovie))), monitor, clock = sameDay).fetch()
    monitor.history(Multikino.displayName).head.thin shouldBe false
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

  // ── Outcome forwarded to a ScrapeOutcomeListener (the Filmweb-drop watcher's hook) ──

  private class RecordingListener extends ScrapeOutcomeListener {
    val seen = scala.collection.mutable.ListBuffer.empty[(Cinema, ScrapeOutcome)]
    def onOutcome(cinema: Cinema, outcome: ScrapeOutcome): Unit = { seen += ((cinema, outcome)); () }
  }

  it should "forward a Success outcome to the listener" in {
    val l = new RecordingListener
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(OneMovie))), new UptimeMonitor(), l).fetch()
    l.seen.toList shouldBe List(Multikino -> ScrapeOutcome.Success)
  }

  it should "forward an Empty outcome to the listener" in {
    val l = new RecordingListener
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(Seq.empty))), new UptimeMonitor(), l).fetch()
    l.seen.toList shouldBe List(Multikino -> ScrapeOutcome.Empty)
  }

  it should "forward a Failure outcome to the listener and still rethrow" in {
    val l = new RecordingListener
    val s = new UptimeRecordingScraper(ScriptedCinemaScraper(List(Left(new RuntimeException("down")))), new UptimeMonitor(), l)
    intercept[RuntimeException] { s.fetch() }
    l.seen.toList shouldBe List(Multikino -> ScrapeOutcome.Failure)
  }

  it should "not let a throwing listener break the scrape" in {
    val boom = new ScrapeOutcomeListener {
      def onOutcome(cinema: Cinema, outcome: ScrapeOutcome): Unit = throw new RuntimeException("listener boom")
    }
    new UptimeRecordingScraper(ScriptedCinemaScraper(List(Right(OneMovie))), new UptimeMonitor(), boom).fetch() shouldBe OneMovie
  }

  // Regression for the Cineworld relaunch outage (2026-09-17..18): a
  // FallbackHttpFetch message nests one nested line per backend, each
  // repeating the full request URL, so the actual failure (the status code)
  // can land well past the old 200-char cap. A cap that chops before any
  // backend's status code appears is worse than no cap.
  it should "not truncate errorLabel before a backend's status code, for a long nested fallback message" in {
    val longUrl = "https://www.cineworld.co.uk/uk/data-api-service/v1/quickbook/10108/dates/in-cinema/075/until/2028-09-17?attr=&lang=en_GB"
    val nested =
      s"All 2 backends failed for get $longUrl:\n" +
        s"  proxy: HttpStatusException: HTTP 404 for GET $longUrl\n" +
        s"  fallback: RuntimeException: All 2 backends failed for get $longUrl:\n" +
        s"    zyte: RuntimeException: Zyte API call returned upstream status=404 for $longUrl\n" +
        s"    direct: HttpStatusException: HTTP 403 for GET $longUrl"
    nested.length should be > 200 // the case that broke the old cap
    UptimeRecordingScraper.errorLabel(new RuntimeException(nested)) should include ("HTTP 404")
  }
}
