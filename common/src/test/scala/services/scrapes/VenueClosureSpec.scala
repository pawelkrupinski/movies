package services.scrapes

import models.Cinema
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.FallbackState

import java.time.Instant
import java.time.temporal.ChronoUnit.DAYS

/**
 * When a venue is closed beyond reasonable doubt, which is when it may be retired
 * without a human checking first. Wrong in one direction and a running cinema
 * disappears from the site; wrong in the other and a dead one pages forever, as
 * Heimgarten Kino (Filmstarts 404, kinoprogramm.com empty) did. Every "not yet"
 * below is a real way a live venue looks dead for a while.
 */
class VenueClosureSpec extends AnyFlatSpec with Matchers with EitherValues {

  private val now    = Instant.parse("2026-09-27T09:00:00Z")
  private val cinema = Cinema.all.head
  private def daysAgo(days: Long) = now.minus(days, DAYS)

  private val NotFound = "HttpStatusException: HTTP 404 for GET https://www.filmstarts.de/kinoprogramm/kino/A1451/"

  private def gone(since: Instant, lastAttempt: Instant = now, error: String = NotFound) =
    ArchivedScrape(cinema, None, lastSuccess = None,
      lastBarren = Some(BarrenAttempt(lastAttempt, ScrapeOutcome.Failed, Some(error), Some(since))))

  private def listedEmpty(since: Instant, lastSeen: Instant = now) =
    Some(FallbackState(cinema.displayName, active = false, fallbackSource = "Kinoprogramm", fallbackRef = None,
      since = None, lastReason = Some(NotFound), consecutiveFailures = 0, lastPrimaryProbeAt = Some(lastSeen),
      nextPrimaryProbeAt = None, updatedAt = lastSeen, history = Nil,
      emptyFallback = Some(FallbackState.EmptySpell(since, lastSeen))))

  "A venue with a fallback" should "be closed when the primary is gone and the fallback has listed nothing for two weeks" in {
    val evidence = VenueClosure.judge(gone(daysAgo(20)), listedEmpty(daysAgo(15)), hasFallback = true, now).value
    evidence.goneSince shouldBe daysAgo(20)
    evidence.primaryError shouldBe NotFound
    evidence.fallback shouldBe VenueClosure.FallbackEvidence.ListedEmpty(daysAgo(15), now)
  }

  it should "not be closed while the fallback's empty spell is under two weeks old" in {
    VenueClosure.judge(gone(daysAgo(20)), listedEmpty(daysAgo(13)), hasFallback = true, now).isLeft shouldBe true
  }

  // A fallback that only ever errored has said nothing about the venue.
  it should "not be closed when the fallback never answered empty" in {
    val erroredOnly = listedEmpty(daysAgo(15)).map(_.copy(emptyFallback = None))
    VenueClosure.judge(gone(daysAgo(20)), erroredOnly, hasFallback = true, now).isLeft shouldBe true
    VenueClosure.judge(gone(daysAgo(20)), None, hasFallback = true, now).isLeft shouldBe true
  }

  // An empty spell nobody has re-confirmed lately may since have ended unrecorded.
  it should "not be closed on an empty spell last confirmed over three days ago" in {
    VenueClosure.judge(gone(daysAgo(20)), listedEmpty(daysAgo(15), lastSeen = daysAgo(4)), hasFallback = true, now)
      .isLeft shouldBe true
  }

  "A venue" should "not be closed while its primary has been gone under two weeks" in {
    VenueClosure.judge(gone(daysAgo(13)), listedEmpty(daysAgo(13)), hasFallback = true, now).isLeft shouldBe true
  }

  // Blocked, throttled or broken describes a page that EXISTS.
  it should "not be closed on a failure that is not 404/410" in {
    val forbidden = gone(daysAgo(40), error = "HttpStatusException: HTTP 403 for GET https://www.filmstarts.de/x/")
    VenueClosure.judge(forbidden, listedEmpty(daysAgo(40)), hasFallback = true, now).isLeft shouldBe true
  }

  // A row nobody is probing any more says nothing about today.
  it should "not be closed when the primary has not been probed for over three days" in {
    VenueClosure.judge(gone(daysAgo(40), lastAttempt = daysAgo(4)), listedEmpty(daysAgo(30)), hasFallback = true, now)
      .isLeft shouldBe true
  }

  it should "not be closed while it is serving" in {
    val serving = ArchivedScrape(cinema, None, Some(SuccessfulScrape(daysAgo(1), listingComplete = true, Nil)), None)
    VenueClosure.judge(serving, listedEmpty(daysAgo(30)), hasFallback = true, now).isLeft shouldBe true
  }

  // With no second source to corroborate it, the primary alone has to hold longer.
  "A venue with no fallback" should "be closed only once its primary has been gone four weeks" in {
    VenueClosure.judge(gone(daysAgo(27)), None, hasFallback = false, now).isLeft shouldBe true
    VenueClosure.judge(gone(daysAgo(28)), None, hasFallback = false, now).value.fallback shouldBe
      VenueClosure.FallbackEvidence.NoFallback
  }

  "The verdict's reason" should "name what is still missing" in {
    VenueClosure.judge(gone(daysAgo(13)), listedEmpty(daysAgo(15)), hasFallback = true, now).left.value should
      include ("gone 13 days")
  }
}
