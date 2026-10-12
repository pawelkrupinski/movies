package services.closure

import models.{Cinema, GermanRoster, UsRoster}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.{FallbackState, InMemoryFallbackStore}
import services.scrapes._
import services.cinemas.ScriptedCinemaScraper
import tools.{GetOnlyHttpFetch, HttpStatusException}

import java.time.temporal.ChronoUnit.DAYS
import java.time.{Clock, Instant, ZoneOffset}

/**
 * The daily sweep: every rostered venue through [[VenueClosure]]. A venue whose sources
 * dropped it AND that shows it has closed pages once and asks for ONE retirement; one
 * whose sources dropped it but that shows no closure pages once as a coverage gap and is
 * never retired; either is withdrawn when it shows life again.
 */
class ClosureSweepSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {

  private val now = Instant.parse("2026-09-27T09:00:00Z")
  private def daysAgo(days: Long) = now.minus(days, DAYS)

  // Two real German venues, so each maps to its Filmstarts id in data/germany.
  private val (hochland, griesbraeu) = {
    val byId = GermanRoster.theaterIdByCinema.map(_.swap)
    (byId("A0609"), byId("A0593"))
  }
  private val NotFound = "HttpStatusException: HTTP 404 for GET https://www.filmstarts.de/kinoprogramm/kino/A0609/"
  private val hochlandSite = "https://www.kino-hochland.example/"

  private class Harness(candidates: Seq[ClosureCandidate], dispatchFails: Boolean = false, withDispatch: Boolean = true) {
    var clock     = now
    val archive   = new InMemoryScrapeArchiveRepository
    val fallbacks = new InMemoryFallbackStore
    val ledger    = new InMemoryClosureLedger
    val pages     = collection.mutable.ListBuffer.empty[String]
    val requested = collection.mutable.ListBuffer.empty[(RosterDirectory, Seq[RetirementRequest])]
    val websites  = collection.mutable.Map.empty[String, String]
    /** What each own site answers: its page, or the failure fetching it throws. */
    val sites     = collection.mutable.Map.empty[String, Either[Throwable, String]]
    val fetched   = collection.mutable.ListBuffer.empty[String]
    val dispatch  = Option.when(withDispatch)(new RetirementDispatch {
      def request(directory: RosterDirectory, venues: Seq[RetirementRequest]): Unit =
        if (dispatchFails) throw new IllegalStateException("HTTP 401") else requested += (directory -> venues)
    })
    private val fetch = new GetOnlyHttpFetch {
      def get(url: String): String = { fetched += url; sites(url).fold(throw _, identity) }
    }
    val sweep = new ClosureSweep(() => candidates, archive, fallbacks,
      new VenueChecks(venue => websites.get(venue), fetch),
      ledger, pages += _, dispatch,
      new Clock { def instant(): Instant = clock; def getZone = ZoneOffset.UTC; override def withZone(z: java.time.ZoneId): Clock = this })

    def gone(cinema: Cinema, since: Instant): Unit = {
      archive.record(ScrapeAttempt(cinema, None, since, listingComplete = true, Nil, Some(NotFound)))
      archive.record(ScrapeAttempt(cinema, None, clock, listingComplete = true, Nil, Some(NotFound)))
    }
    def emptyFallback(cinema: Cinema, since: Instant): Unit =
      fallbacks.put(fallbackState(cinema).copy(emptyFallback = Some(FallbackState.EmptySpell(since, clock))))
    def droppedFallback(cinema: Cinema, since: Instant, error: String): Unit =
      fallbacks.put(fallbackState(cinema).copy(droppedFallback = Some(FallbackState.DroppedSpell(since, clock, error))))
    private def fallbackState(cinema: Cinema) =
      FallbackState(cinema.displayName, active = false, fallbackSource = "Kinoprogramm", fallbackRef = None,
        since = None, lastReason = Some(NotFound), consecutiveFailures = 0, lastPrimaryProbeAt = Some(clock),
        nextPrimaryProbeAt = None, updatedAt = clock, history = Nil)
    def serving(cinema: Cinema): Unit =
      archive.record(ScrapeAttempt(cinema, None, clock, listingComplete = true, ScriptedCinemaScraper.OneMovie))
    def site(cinema: Cinema, url: String, answer: Either[Throwable, String]): Unit = { websites(cinema.displayName) = url; sites(url) = answer }
    /** Hochland's sources dropped it and its own site is gone: closed. */
    def closed(): Unit = {
      gone(hochland, daysAgo(20)); emptyFallback(hochland, daysAgo(15))
      site(hochland, hochlandSite, Left(new HttpStatusException(404, "GET", hochlandSite, None)))
    }
  }

  "ClosureSweep" should "page once and request retirement for a venue whose sources dropped it and whose own site is gone" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true), ClosureCandidate(griesbraeu, hasFallback = true)))
    h.closed()
    h.serving(griesbraeu)
    h.sweep.sweep()
    h.requested.map { case (dir, venues) => dir -> venues.map(_.entry.id) } shouldBe Seq(RosterDirectory("germany") -> Seq("A0609"))
    h.requested.head._2.head.checks shouldBe Seq(LiveCheck("website", hochlandSite))
    h.pages should have size 1
    h.pages.head should (include (hochland.displayName) and include ("HTTP 404") and include ("retirement PR requested")
      and include (s"CLOSED: own site $hochlandSite answered HTTP 404"))
    h.ledger.entries().map { case (name, entry) => name -> entry.kind } shouldBe Map(hochland.displayName -> LedgerKind.Closed)

    h.sweep.sweep()                                   // still closed tomorrow: nothing new to say
    h.requested should have size 1
    h.pages should have size 1
  }

  // ACME Screening Room, Lambertville: flicks.us 404'd it for weeks, nothing else listed it,
  // and it was retired as "closed" — while its own site sold tickets to its October festival.
  it should "never retire a venue its aggregator dropped while its own site is live with a programme (ACME)" in {
    val venue = UsRoster.byDisplayName.values.find(UsRoster.flicksSlugByCinema.contains).get
    val acme  = "https://www.acmescreeningroom.org/"
    val h = new Harness(Seq(ClosureCandidate(venue, hasFallback = false)))
    h.gone(venue, daysAgo(40))
    // Recorded 2026-10-12 from https://www.acmescreeningroom.org/ (scripts, styles and inline data stripped).
    h.site(venue, acme, Right(String(getClass.getResourceAsStream("/fixtures/venue-websites/acme_screening_room_open_with_programme.html").readAllBytes(), "UTF-8")))
    h.sweep.sweep()
    h.requested shouldBe empty
    h.ledger.entries()(venue.displayName).kind shouldBe LedgerKind.SourceDropped
    h.pages should have size 1
    h.pages.head should (include ("coverage gap") and include (s"OPEN: own site $acme answered with no closure notice")
      and not include ("looks closed"))

    h.sweep.sweep()                                   // still a gap tomorrow: paged once
    h.pages should have size 1
  }

  it should "raise a dropped venue with nothing known about the venue itself as a coverage gap, never a closure" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(15))
    h.sweep.sweep()
    h.requested shouldBe empty
    h.pages.head should (include ("coverage gap") and include ("no own website or second listing on record"))
    h.ledger.entries()(hochland.displayName).kind shouldBe LedgerKind.SourceDropped
  }

  it should "retire a venue a second listing has dropped too, and hand the live re-check that listing's page" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    val listing = "https://www.kinoprogramm.com/kino/hochland/"
    h.gone(hochland, daysAgo(20)); h.droppedFallback(hochland, daysAgo(15), s"HttpStatusException: HTTP 404 for GET $listing")
    h.sweep.sweep()
    h.requested.head._2.head.checks shouldBe Seq(LiveCheck("listing", listing))
    h.pages.head should include ("CLOSED: second listing Kinoprogramm dropped the venue")
  }

  // The workflow's live re-check reads these again; without them it keeps the venue.
  it should "hand the retirement workflow each venue's evidence line and venue-level checks" in {
    val request = RetirementRequest(RosterEntry(RosterDirectory("germany"), "A0609"), hochland.displayName, "evidence line",
      Seq(LiveCheck("website", hochlandSite)))
    GitHubRetirementDispatch.venueJson(request) shouldBe play.api.libs.json.Json.obj("id" -> "A0609", "name" -> hochland.displayName,
      "evidence" -> "evidence line", "checks" -> Seq(play.api.libs.json.Json.obj("kind" -> "website", "url" -> hochlandSite)))
  }

  // The workflow re-checks the page the sweep names, so it must be the page whose 404 is
  // the evidence: the primary's own (ACME Screening Room's site), not the aggregator a
  // venue scraped off its own site may have dropped for good.
  it should "send the page its primary scrapes, for the workflow to re-check" in {
    val page = "https://kino.example/programm"
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true, page = Some(page))))
    h.closed()
    h.sweep.sweep()
    val request = h.requested.flatMap(_._2).loneElement
    request.page shouldBe Some(page)
    (GitHubRetirementDispatch.venueJson(request) \ "page").as[String] shouldBe page
    (GitHubRetirementDispatch.venueJson(request.copy(page = None)) \ "page").toOption shouldBe None
  }

  it should "not ask a venue's own site anything while its sources still list it" in {
    val h = new Harness(Seq(ClosureCandidate(griesbraeu, hasFallback = true)))
    h.serving(griesbraeu)
    h.site(griesbraeu, "https://griesbraeu.example/", Right("<html/>"))
    h.sweep.sweep()
    h.fetched shouldBe empty
  }

  it should "say nothing for a venue whose sources have not yet dropped it" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(10))
    h.sweep.sweep()
    h.pages shouldBe empty
    h.requested shouldBe empty
  }

  it should "withdraw a confirmed venue that serves again, and say so" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.closed()
    h.sweep.sweep()
    h.serving(hochland)
    h.sweep.sweep()
    h.ledger.entries() shouldBe empty
    h.pages.last should (include (hochland.displayName) and include ("no longer"))
  }

  it should "withdraw a closure whose own site comes back, keeping the venue as a coverage gap" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.closed()
    h.sweep.sweep()
    h.sites(hochlandSite) = Right("<html><body>Programm diese Woche</body></html>")
    h.sweep.sweep()
    h.ledger.entries()(hochland.displayName).kind shouldBe LedgerKind.SourceDropped
    h.pages.last should (include ("no longer looks closed") and include ("coverage gap"))
  }

  it should "withdraw a coverage gap once the venue is listed again" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(15))
    h.sweep.sweep()
    h.serving(hochland)
    h.sweep.sweep()
    h.ledger.entries() shouldBe empty
    h.pages.last should include ("listed again")
  }

  // Merged: the venue left the roster, so it is no longer a candidate. Nothing to say.
  it should "quietly forget a confirmed venue once it has left the roster" in {
    val h = new Harness(Seq.empty)
    h.ledger.record(hochland.displayName, LedgerEntry(LedgerKind.Closed, daysAgo(2), ""))
    h.sweep.sweep()
    h.ledger.entries() shouldBe empty
    h.pages shouldBe empty
  }

  it should "try again next sweep when the retirement request fails" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)), dispatchFails = true)
    h.closed()
    h.sweep.sweep()
    h.ledger.entries() shouldBe empty
    h.pages.head should include ("HTTP 401")
  }

  it should "ask for a hand retirement when there is no way to request one" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)), withDispatch = false)
    h.closed()
    h.sweep.sweep()
    h.pages.head should include ("retire by hand")
    h.ledger.entries().keySet shouldBe Set(hochland.displayName)
  }

  // Freiluftkino Kreuzberg's page answers 404 every winter: that is the season ending, not the venue.
  it should "never retire a seasonal venue on its off-season silence" in {
    val freiluftkino = GermanRoster.theaterIdByCinema.map(_.swap)("A2198")
    val h = new Harness(Seq(ClosureCandidate(freiluftkino, hasFallback = true)))
    h.clock = Instant.parse("2026-12-01T09:00:00Z")
    h.gone(freiluftkino, h.clock.minus(60, DAYS)); h.emptyFallback(freiluftkino, h.clock.minus(60, DAYS))
    h.site(freiluftkino, "https://freiluftkino.example/", Left(new HttpStatusException(404, "GET", "https://freiluftkino.example/", None)))
    h.sweep.sweep()
    h.requested shouldBe empty
    h.pages shouldBe empty
    h.ledger.entries() shouldBe empty
  }

  // A failed read is not data: a partial archive would make every unread venue look
  // like one with no evidence, and withdraw every confirmed closure.
  it should "change nothing when the archive could not be read in full" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.ledger.record(hochland.displayName, LedgerEntry(LedgerKind.Closed, daysAgo(1), ""))
    val broken = new ClosureSweep(() => Seq(ClosureCandidate(hochland, hasFallback = true)),
      new InMemoryScrapeArchiveRepository { override def scan(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome = tools.ScanOutcome.of(whole = false, "unreadable on purpose") },
      h.fallbacks, new VenueChecks(VenueWebsites(Map.empty), new GetOnlyHttpFetch { def get(url: String): String = fail(url) }),
      h.ledger, h.pages += _, h.dispatch, Clock.fixed(now, ZoneOffset.UTC))
    broken.sweep()
    h.ledger.entries().keySet shouldBe Set(hochland.displayName)
    h.pages shouldBe empty
  }
}
