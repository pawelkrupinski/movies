package services.closure

import models.{Cinema, GermanRoster}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.{FallbackState, InMemoryFallbackStore}
import services.scrapes._
import services.cinemas.ScriptedCinemaScraper

import java.time.temporal.ChronoUnit.DAYS
import java.time.{Clock, Instant, ZoneOffset}

/**
 * The daily sweep: every rostered venue through [[VenueClosure]], a page and ONE
 * retirement request per newly confirmed closure, and a withdrawal for one that
 * shows life again before it is retired.
 */
class ClosureSweepSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-09-27T09:00:00Z")
  private def daysAgo(days: Long) = now.minus(days, DAYS)

  // Two real German venues, so each maps to its Filmstarts id in data/germany.
  private val (hochland, griesbraeu) = {
    val byId = GermanRoster.theaterIdByCinema.map(_.swap)
    (byId("A0609"), byId("A0593"))
  }
  private val NotFound = "HttpStatusException: HTTP 404 for GET https://www.filmstarts.de/kinoprogramm/kino/A0609/"

  private class Harness(candidates: Seq[ClosureCandidate], dispatchFails: Boolean = false, withDispatch: Boolean = true) {
    var clock     = now
    val archive   = new InMemoryScrapeArchiveRepository
    val fallbacks = new InMemoryFallbackStore
    val ledger    = new InMemoryClosureLedger
    val pages     = collection.mutable.ListBuffer.empty[String]
    val requested = collection.mutable.ListBuffer.empty[(RosterDirectory, Seq[RetirementRequest])]
    val dispatch  = Option.when(withDispatch)(new RetirementDispatch {
      def request(directory: RosterDirectory, venues: Seq[RetirementRequest]): Unit =
        if (dispatchFails) throw new IllegalStateException("HTTP 401") else requested += (directory -> venues)
    })
    val sweep = new ClosureSweep(() => candidates, archive, fallbacks, ledger, pages += _, dispatch,
      new Clock { def instant(): Instant = clock; def getZone = ZoneOffset.UTC; override def withZone(z: java.time.ZoneId): Clock = this })

    def gone(cinema: Cinema, since: Instant): Unit = {
      archive.record(ScrapeAttempt(cinema, None, since, listingComplete = true, Nil, Some(NotFound)))
      archive.record(ScrapeAttempt(cinema, None, clock, listingComplete = true, Nil, Some(NotFound)))
    }
    def emptyFallback(cinema: Cinema, since: Instant): Unit =
      fallbacks.put(FallbackState(cinema.displayName, active = false, fallbackSource = "Kinoprogramm", fallbackRef = None,
        since = None, lastReason = Some(NotFound), consecutiveFailures = 0, lastPrimaryProbeAt = Some(clock),
        nextPrimaryProbeAt = None, updatedAt = clock, history = Nil,
        emptyFallback = Some(FallbackState.EmptySpell(since, clock))))
    def serving(cinema: Cinema): Unit =
      archive.record(ScrapeAttempt(cinema, None, clock, listingComplete = true, ScriptedCinemaScraper.OneMovie))
  }

  "ClosureSweep" should "page once and request retirement for a venue confirmed closed" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true), ClosureCandidate(griesbraeu, hasFallback = true)))
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(15))
    h.serving(griesbraeu)
    h.sweep.sweep()
    h.requested.map { case (dir, venues) => dir -> venues.map(_.entry.id) } shouldBe Seq(RosterDirectory("germany") -> Seq("A0609"))
    h.pages should have size 1
    h.pages.head should (include (hochland.displayName) and include ("HTTP 404") and include ("retirement PR requested"))
    h.ledger.confirmed().keySet shouldBe Set(hochland.displayName)

    h.sweep.sweep()                                   // still closed tomorrow: nothing new to say
    h.requested should have size 1
    h.pages should have size 1
  }

  it should "say nothing for a venue that is not yet confirmed" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(10))
    h.sweep.sweep()
    h.pages shouldBe empty
    h.requested shouldBe empty
  }

  it should "withdraw a confirmed venue that serves again, and say so" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(15))
    h.sweep.sweep()
    h.serving(hochland)
    h.sweep.sweep()
    h.ledger.confirmed() shouldBe empty
    h.pages.last should (include (hochland.displayName) and include ("no longer"))
  }

  // Merged: the venue left the roster, so it is no longer a candidate. Nothing to say.
  it should "quietly forget a confirmed venue once it has left the roster" in {
    val h = new Harness(Seq.empty)
    h.ledger.confirm(hochland.displayName, daysAgo(2))
    h.sweep.sweep()
    h.ledger.confirmed() shouldBe empty
    h.pages shouldBe empty
  }

  it should "try again next sweep when the retirement request fails" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)), dispatchFails = true)
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(15))
    h.sweep.sweep()
    h.ledger.confirmed() shouldBe empty
    h.pages.head should include ("HTTP 401")
  }

  it should "ask for a hand retirement when there is no way to request one" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)), withDispatch = false)
    h.gone(hochland, daysAgo(20)); h.emptyFallback(hochland, daysAgo(15))
    h.sweep.sweep()
    h.pages.head should include ("retire by hand")
    h.ledger.confirmed().keySet shouldBe Set(hochland.displayName)
  }

  // A failed read is not data: a partial archive would make every unread venue look
  // like one with no evidence, and withdraw every confirmed closure.
  it should "change nothing when the archive could not be read in full" in {
    val h = new Harness(Seq(ClosureCandidate(hochland, hasFallback = true)))
    h.ledger.confirm(hochland.displayName, daysAgo(1))
    val broken = new ClosureSweep(() => Seq(ClosureCandidate(hochland, hasFallback = true)),
      new InMemoryScrapeArchiveRepository { override def scan(consume: Seq[ArchivedScrape] => Unit): Boolean = false },
      h.fallbacks, h.ledger, h.pages += _, h.dispatch, Clock.fixed(now, ZoneOffset.UTC))
    broken.sweep()
    h.ledger.confirmed().keySet shouldBe Set(hochland.displayName)
    h.pages shouldBe empty
  }
}
