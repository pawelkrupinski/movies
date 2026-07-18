package services.cinemas

import services.cinemas.ScriptedCinemaScraper.{NoShowtimes, OneMovie}
import services.alerts.FallbackAlert
import models.{Cinema, CinemaMovie, Multikino}
import services.fallback.{FallbackEvent, FilmwebFallbackState, InMemoryFilmwebFallbackStore}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.UptimeMonitor
import services.cinemas.common.CinemaScraper
import services.cinemas.pl.FilmwebFallbackScraper

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
    base:        FiniteDuration = 10.minutes,
    grace:       FiniteDuration = 6.hours
  ) {
    val monitor = new UptimeMonitor()
    val store   = new InMemoryFilmwebFallbackStore
    val primary = new FakeScraper(primaryPlan)
    var clock: Instant = Instant.parse("2026-06-10T08:00:00Z")
    val events = collection.mutable.ListBuffer.empty[(FilmwebFallbackState, FallbackEvent)]
    val scraper = new FilmwebFallbackScraper(
      primary, () => filmweb, () => Some(2180), monitor, store,
      now = () => clock, baseBackoff = base, maxBackoff = 60.minutes, fallbackAfter = grace,
      onEvent = (s, e) => events += ((s, e))
    )
    def bucket = monitor.history(Service).head
    def state  = store.get(Service)
    def advance(d: FiniteDuration): Unit = clock = clock.plusMillis(d.toMillis)
    /** The Telegram pages that would actually go out — what each onEvent resolves
     *  to via FallbackAlert (gated on `alerted`), in fire order. */
    def alerts: Seq[String] = events.toList.flatMap { case (s, e) => FallbackAlert.messageFor(s, e) }
    /** Drive one failing tick, swallowing the re-raised throw a grace tick emits. */
    def tickSwallowing(): Unit = try { scraper.fetch(); () } catch { case _: RuntimeException => () }
  }

  private def filmwebWith(movies: Seq[CinemaMovie]) = ScriptedCinemaScraper(List.fill(99)(Right(movies)))

  "FilmwebFallbackScraper" should "serve the primary and record a plain success when it's healthy" in {
    val h = new Harness(Seq(Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch() shouldBe OneMovie
    h.bucket.successes shouldBe 1
    h.bucket.fallback  shouldBe false
    h.bucket.status    shouldBe "green"
    h.state shouldBe None                 // never failed → no state document
    h.primary.calls shouldBe 1            // filmweb never consulted
  }

  // ---- grace window: ride out short outages on last-good data, don't fall back yet ----

  it should "RE-RAISE (not fall back) on the first throw — inside the grace window Filmweb is never consulted" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    intercept[RuntimeException] { h.scraper.fetch() }.getMessage shouldBe "primary down"
    h.bucket.failures shouldBe 1
    h.bucket.fallback shouldBe false               // NOT served via Filmweb
    h.bucket.status   shouldBe "red"
    h.state.map(_.active)               shouldBe Some(false)   // failing, but not on fallback
    h.state.flatMap(_.failingSince)     shouldBe Some(h.clock) // grace clock started
    h.state.map(_.consecutiveFailures)  shouldBe Some(0)
    h.state.map(_.history)              shouldBe Some(Nil)     // grace is not a fallback transition
    h.events shouldBe empty                                    // nothing to page
  }

  it should "keep re-raising and NOT fall back while the outage is younger than the grace window" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // t0: grace starts
    h.advance(5.hours)             // still under 6h
    intercept[RuntimeException] { h.scraper.fetch() }  // still re-raises
    h.state.map(_.active) shouldBe Some(false)
    h.events shouldBe empty
    h.bucket.fallback shouldBe false
  }

  it should "fall back to Filmweb (ENTER) only once the primary has failed continuously for the grace window" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // t0: grace starts, re-raises
    h.advance(6.hours + 1.minute)  // past the grace window
    h.scraper.fetch() shouldBe OneMovie     // now served from Filmweb
    h.bucket.fallback shouldBe true         // the slot is marked "served via Filmweb"
    h.state.map(_.active) shouldBe Some(true)
    h.state.flatMap(_.filmwebCinemaId) shouldBe Some(2180)
    h.state.map(_.consecutiveFailures) shouldBe Some(1)
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter)
  }

  it should "reset the grace clock when the primary recovers mid-window (a fresh outage starts over)" in {
    val h = new Harness(Seq(Left(boom), Right(OneMovie), Left(boom)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // t0: grace starts
    h.advance(5.hours)
    h.scraper.fetch() shouldBe OneMovie     // primary healthy again → clock cleared
    h.state.flatMap(_.failingSince) shouldBe None
    h.advance(2.hours)             // 7h since the FIRST failure, but the run was broken
    intercept[RuntimeException] { h.scraper.fetch() }   // new outage: back in grace, re-raises
    h.state.map(_.active) shouldBe Some(false)
    h.events shouldBe empty        // did NOT enter fallback despite >6h since first-ever failure
  }

  it should "enter immediately after a restart when the persisted grace clock is already older than the window" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    // Simulate a worker that recorded the first failure 7h ago and then restarted.
    h.store.put(FilmwebFallbackState(
      cinema = Service, active = false, filmwebCinemaId = Some(2180),
      failingSince = Some(h.clock.minusMillis((7.hours).toMillis)), since = None, lastReason = Some("down"),
      consecutiveFailures = 0, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
      updatedAt = h.clock.minusMillis((7.hours).toMillis), history = Nil
    ))
    h.scraper.fetch() shouldBe OneMovie     // one throw, clock already past 6h → ENTER
    h.state.map(_.active) shouldBe Some(true)
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter)
  }

  // ---- empty-primary handling ----

  it should "treat an empty scrape with Filmweb data as a grace failure (keep the empty until the window elapses)" in {
    val h = new Harness(Seq(Right(NoShowtimes)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch() shouldBe NoShowtimes  // empty returned as-is (no-op downstream keeps last-good)
    h.bucket.fallback shouldBe false
    h.bucket.status   shouldBe "zero"
    h.state.map(_.active) shouldBe Some(false)
    h.state.flatMap(_.failingSince) should not be empty
  }

  it should "fall back on a persistently-empty primary once the grace window elapses" in {
    val h = new Harness(Seq(Right(NoShowtimes)), Some(filmwebWith(OneMovie)))
    h.scraper.fetch()              // t0: empty, grace starts
    h.advance(6.hours + 1.minute)
    h.scraper.fetch() shouldBe OneMovie     // now served from Filmweb
    h.bucket.fallback shouldBe true
    h.state.map(_.active) shouldBe Some(true)
  }

  it should "NOT trip or start a grace clock when both the primary and Filmweb are empty (genuine empty, e.g. night)" in {
    val h = new Harness(Seq(Right(Seq.empty)), Some(filmwebWith(Seq.empty)))
    h.scraper.fetch() shouldBe empty
    h.bucket.zeroes   shouldBe 1
    h.bucket.fallback shouldBe false
    h.bucket.status   shouldBe "zero"
    h.state shouldBe None
    h.events shouldBe empty
  }

  it should "record the primary's failure (red) and RE-RAISE when it throws and Filmweb is unavailable, even past the window" in {
    val h = new Harness(Seq(Left(boom)), filmweb = None)
    h.tickSwallowing()
    h.advance(6.hours + 1.minute)
    intercept[RuntimeException] { h.scraper.fetch() }.getMessage shouldBe "primary down"
    h.bucket.failures should be >= 1
    h.bucket.fallback shouldBe false
    h.bucket.status   shouldBe "red"
    h.state.map(_.active) shouldBe Some(false)  // can't fall back with no Filmweb → stays in grace
  }

  // ---- once on fallback: existing backoff / recovery / paging semantics ----

  it should "skip the broken primary within the backoff window and serve Filmweb directly" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // t0: grace
    h.advance(6.hours + 1.minute)
    h.scraper.fetch()              // ENTER, nextProbe = now + 10min
    val callsAtEntry = h.primary.calls
    h.advance(1.minute)            // still within the 10-min window
    h.scraper.fetch() shouldBe OneMovie
    h.primary.calls shouldBe callsAtEntry   // primary NOT re-probed — served straight from Filmweb
    h.bucket.fallback shouldBe true
    h.state.map(_.active) shouldBe Some(true)
  }

  it should "re-probe after the backoff window and RECOVER when the primary returns" in {
    val h = new Harness(Seq(Left(boom), Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // t0: grace
    h.advance(6.hours + 1.minute)
    h.scraper.fetch()              // ENTER
    h.advance(11.minutes)          // past nextProbe
    h.scraper.fetch() shouldBe OneMovie   // primary healthy again
    h.state.map(_.active) shouldBe Some(false)
    h.state.map(_.consecutiveFailures) shouldBe Some(0)
    h.state.flatMap(_.failingSince) shouldBe None
    h.events.map(_._2.event) shouldBe List(FallbackEvent.Enter, FallbackEvent.Recovered)
  }

  it should "extend the backoff (PROBE_FAILED) when a re-probe finds the primary still down" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // t0: grace
    h.advance(6.hours + 1.minute)
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

  it should "page ENTER immediately on entering fallback (the grace window already filtered out blips)" in {
    val h = new Harness(Seq(Left(boom)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // grace — no page
    h.alerts shouldBe empty
    h.advance(6.hours + 1.minute)
    h.scraper.fetch()              // ENTER → page at once
    h.state.map(_.alerted) shouldBe Some(true)
    h.alerts should have size 1
    h.alerts.head should include ("serving via Filmweb fallback")
    // Subsequent failed re-probes do NOT re-page.
    h.advance(1.hour)
    h.scraper.fetch()
    h.alerts should have size 1
  }

  it should "NOT page when a cinema recovers while still in the grace window (never entered fallback)" in {
    val h = new Harness(Seq(Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // grace
    h.advance(11.minutes)          // well under the 6h window
    h.scraper.fetch() shouldBe OneMovie   // primary back → grace cleared, no event
    h.events shouldBe empty
    h.alerts shouldBe empty
  }

  it should "page RECOVERED when the primary returns after we had entered fallback" in {
    val h = new Harness(Seq(Left(boom), Left(boom), Right(OneMovie)), Some(filmwebWith(OneMovie)))
    h.tickSwallowing()             // grace
    h.advance(6.hours + 1.minute)
    h.scraper.fetch()              // ENTER → page
    h.advance(1.hour)              // past the extended backoff
    h.scraper.fetch() shouldBe OneMovie   // primary back → RECOVERED
    h.state.map(_.active) shouldBe Some(false)
    h.alerts should have size 2
    h.alerts.head  should include ("serving via Filmweb fallback")
    h.alerts.last  should include ("recovered")
  }
}
