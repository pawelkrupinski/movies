package services.cinemas

import models.{Cinema, CinemaMovie, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.alerts.FallbackAlert
import services.cinemas.ScriptedCinemaScraper.{NoShowtimes, OneMovie}
import services.fallback.{FallbackEvent, FilmwebFallbackState, InMemoryFilmwebFallbackStore}

import java.time.Instant
import scala.concurrent.duration._

class FilmwebFallbackSpec extends AnyFlatSpec with Matchers {

  private val Service = Multikino.displayName

  /** A scraper replaying a plan, repeating the last entry, and counting calls so
   *  a test can assert the primary was (not) probed. */
  private class FakeScraper(plan: Seq[Either[Throwable, Seq[CinemaMovie]]]) extends CinemaScraper {
    var calls = 0
    val cinema: Cinema           = Multikino
    def scrapeHosts: Set[String] = Set.empty
    def fetch(): Seq[CinemaMovie] = {
      val o = plan(math.min(calls, plan.length - 1)); calls += 1
      o match { case Right(v) => v; case Left(t) => throw t }
    }
  }

  private def boom = new RuntimeException("primary down")

  private class Harness(
    primaryPlan: Seq[Either[Throwable, Seq[CinemaMovie]]],
    filmweb:     Option[CinemaScraper],
    base:        FiniteDuration = 10.minutes
  ) {
    val monitor = new UptimeMonitor()
    val store   = new InMemoryFilmwebFallbackStore
    val primary = new FakeScraper(primaryPlan)
    var clock: Instant = Instant.parse("2026-06-10T08:00:00Z")
    val events = collection.mutable.ListBuffer.empty[(FilmwebFallbackState, FallbackEvent)]
    val scraper = new FilmwebFallbackScraper(
      primary, () => filmweb, () => Some(2180), monitor, store,
      now = () => clock, baseBackoff = base, maxBackoff = 60.minutes,
      onEvent = (s, e) => events += ((s, e))
    )
    def bucket = monitor.history(Service).head
    def state  = store.get(Service)
    def advance(d: FiniteDuration): Unit = clock = clock.plusMillis(d.toMillis)
    /** The Telegram pages that would actually go out — what each onEvent resolves
     *  to via FallbackAlert (gated on `alerted`), in fire order. */
    def alerts: Seq[String] = events.toList.flatMap { case (s, e) => FallbackAlert.messageFor(s, e) }
  }

  private def filmwebWith(movies: Seq[CinemaMovie]) = ScriptedCinemaScraper(List.fill(99)(Right(movies)))

  "FilmwebFallbackScraper" should "serve the primary and record a plain success when it's healthy" in {
    val h = new Harness(Seq(Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch() shouldBe OneMovie
    h.bucket.successes shouldBe 1
    h.bucket.fallback  shouldBe false
    h.bucket.status    shouldBe "green"
    h.state shouldBe None                 // never entered fallback → no state document
    h.primary.calls shouldBe 1            // filmweb never consulted
  }

  it should "fall back to Filmweb (ENTER) when the primary throws, recording a fallback-success" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch() shouldBe OneMovie    // served from Filmweb
    h.bucket.successes shouldBe 1
    h.bucket.fallback  shouldBe true
    h.bucket.status    shouldBe "green"    // user got showtimes
    h.state.map(_.active) shouldBe Some(true)
    h.state.flatMap(_.filmwebCinemaId) shouldBe Some(2180)
    h.state.map(_.consecutiveFailures) shouldBe Some(1)
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter)
    h.state.map(_.alerted) shouldBe Some(false)  // entered, but not yet paged
    h.alerts shouldBe empty                       // a fresh fall-back does NOT page immediately
  }

  it should "fall back when the primary returns zero screenings but Filmweb has data" in {
    val h = new Harness(Seq(Right(NoShowtimes)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch() shouldBe OneMovie
    h.bucket.fallback shouldBe true
    h.state.map(_.active) shouldBe Some(true)
  }

  it should "NOT trip when both the primary and Filmweb are empty (genuine empty, e.g. night)" in {
    val h = new Harness(Seq(Right(Seq.empty)), Some(filmwebWith(Seq.empty)))
    h.scraper.fetch() shouldBe empty
    h.bucket.zeroes   shouldBe 1
    h.bucket.fallback shouldBe false
    h.bucket.status   shouldBe "zero"
    h.state shouldBe None
    h.events shouldBe empty
  }

  it should "record the primary's failure (red) and RE-RAISE when it throws and Filmweb is unavailable" in {
    val h = new Harness(Seq(Left(boom)), filmweb = None)
    // Rethrows like UptimeRecordingScraper did, so the scrape tick still skips the cinema.
    intercept[RuntimeException] { h.scraper.fetch() }.getMessage shouldBe "primary down"
    h.bucket.failures shouldBe 1
    h.bucket.fallback shouldBe false
    h.bucket.status   shouldBe "red"
    h.state shouldBe None
  }

  it should "skip the broken primary within the backoff window and serve Filmweb directly" in {
    val h = new Harness(Seq(Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch()              // tick 1: ENTER, nextProbe = now + 10min
    h.primary.calls shouldBe 1
    h.advance(1.minute)            // still within the 10-min window
    h.scraper.fetch() shouldBe OneMovie
    h.primary.calls shouldBe 1     // primary NOT re-probed — served straight from Filmweb
    h.bucket.fallback shouldBe true
    h.state.map(_.active) shouldBe Some(true)
  }

  it should "re-probe after the backoff window and RECOVER when the primary returns" in {
    val h = new Harness(Seq(Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch()              // tick 1: ENTER
    h.advance(11.minutes)          // past nextProbe
    h.scraper.fetch() shouldBe OneMovie   // tick 2: primary healthy again
    h.primary.calls shouldBe 2     // it WAS re-probed
    h.state.map(_.active) shouldBe Some(false)
    h.state.map(_.consecutiveFailures) shouldBe Some(0)
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter, FallbackEvent.Recovered)
    // Both ticks landed in the same real 15-min uptime slot: one fallback-success
    // then one plain success → two successes; the slot's `fallback` flag stays set
    // (it WAS on fallback for part of the slot), which is the intended semantics.
    h.bucket.successes shouldBe 2
  }

  it should "extend the backoff (PROBE_FAILED) when a re-probe finds the primary still down" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))   // primary always down
    h.scraper.fetch()                          // ENTER, consecutive=1, nextProbe=+10min
    val firstNext = h.state.flatMap(_.nextPrimaryProbeAt).get
    h.advance(11.minutes)
    h.scraper.fetch()                          // re-probe: still down → PROBE_FAILED, consecutive=2
    h.state.map(_.consecutiveFailures) shouldBe Some(2)
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter, FallbackEvent.ProbeFailed)
    // consecutive=2 → backoff doubles to 20min, so the new probe time is further out.
    val secondNext = h.state.flatMap(_.nextPrimaryProbeAt).get
    java.time.Duration.between(h.clock, secondNext).toMinutes shouldBe 20L
    secondNext.isAfter(firstNext) shouldBe true
  }

  it should "NOT page when a cinema falls back and recovers within the alert window" in {
    val h = new Harness(Seq(Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch()              // ENTER
    h.advance(11.minutes)          // well under the 3h alert window
    h.scraper.fetch() shouldBe OneMovie   // primary back → RECOVERED
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter, FallbackEvent.Recovered)
    h.alerts shouldBe empty        // neither the ENTER nor the RECOVERED ever paged
  }

  it should "page ENTER once a cinema has been on fallback longer than the alert window" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))   // primary stays down
    h.scraper.fetch()              // ENTER, no page
    h.alerts shouldBe empty
    h.advance(3.hours + 1.minute)
    h.scraper.fetch()              // re-probe: still down AND now past 3h → page ENTER
    h.state.map(_.alerted) shouldBe Some(true)
    h.alerts should have size 1
    h.alerts.head should include ("serving via Filmweb fallback")
    // Subsequent failed re-probes do NOT re-page — alerted once per spell.
    h.advance(1.hour)
    h.scraper.fetch()
    h.alerts should have size 1
  }

  it should "page RECOVERED only when the primary returns AFTER the alert window (i.e. it had paged ENTER)" in {
    val h = new Harness(Seq(Left(boom), Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch()              // ENTER
    h.advance(3.hours + 1.minute)
    h.scraper.fetch()              // re-probe still down, past 3h → page ENTER
    h.advance(1.hour)              // past the extended backoff
    h.scraper.fetch() shouldBe OneMovie   // primary back → RECOVERED (and we had paged ENTER)
    h.state.map(_.active) shouldBe Some(false)
    h.alerts should have size 2
    h.alerts.head  should include ("serving via Filmweb fallback")
    h.alerts.last  should include ("recovered")
  }
}
