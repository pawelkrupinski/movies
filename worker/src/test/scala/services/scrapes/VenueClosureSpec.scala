package services.scrapes

import models.{Cinema, SeasonWindow}
import models.Opening.{Seasonal, YearRound}
import org.scalatest.{EitherValues, OptionValues}
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
class VenueClosureSpec extends AnyFlatSpec with Matchers with EitherValues with OptionValues {

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
    val evidence = VenueClosure.sourceDropped(gone(daysAgo(20)), listedEmpty(daysAgo(15)), hasFallback = true, YearRound, now).value
    evidence.goneSince shouldBe daysAgo(20)
    evidence.primaryError shouldBe NotFound
    evidence.fallback shouldBe VenueClosure.FallbackEvidence.ListedEmpty(daysAgo(15), now)
  }

  it should "not be closed while the fallback's empty spell is under two weeks old" in {
    VenueClosure.sourceDropped(gone(daysAgo(20)), listedEmpty(daysAgo(13)), hasFallback = true, YearRound, now).isLeft shouldBe true
  }

  // A fallback that only ever errored has said nothing about the venue.
  it should "not be closed when the fallback never answered empty" in {
    val erroredOnly = listedEmpty(daysAgo(15)).map(_.copy(emptyFallback = None))
    VenueClosure.sourceDropped(gone(daysAgo(20)), erroredOnly, hasFallback = true, YearRound, now).isLeft shouldBe true
    VenueClosure.sourceDropped(gone(daysAgo(20)), None, hasFallback = true, YearRound, now).isLeft shouldBe true
  }

  // An empty spell nobody has re-confirmed lately may since have ended unrecorded.
  it should "not be closed on an empty spell last confirmed over three days ago" in {
    VenueClosure.sourceDropped(gone(daysAgo(20)), listedEmpty(daysAgo(15), lastSeen = daysAgo(4)), hasFallback = true, YearRound, now)
      .isLeft shouldBe true
  }

  "A venue" should "not be closed while its primary has been gone under two weeks" in {
    VenueClosure.sourceDropped(gone(daysAgo(13)), listedEmpty(daysAgo(13)), hasFallback = true, YearRound, now).isLeft shouldBe true
  }

  // Blocked, throttled or broken describes a page that EXISTS.
  it should "not be closed on a failure that is not 404/410" in {
    val forbidden = gone(daysAgo(40), error = "HttpStatusException: HTTP 403 for GET https://www.filmstarts.de/x/")
    VenueClosure.sourceDropped(forbidden, listedEmpty(daysAgo(40)), hasFallback = true, YearRound, now).isLeft shouldBe true
  }

  // A row nobody is probing any more says nothing about today.
  it should "not be closed when the primary has not been probed for over three days" in {
    VenueClosure.sourceDropped(gone(daysAgo(40), lastAttempt = daysAgo(4)), listedEmpty(daysAgo(30)), hasFallback = true, YearRound, now)
      .isLeft shouldBe true
  }

  it should "not be closed while it is serving" in {
    val serving = ArchivedScrape(cinema, None, Some(SuccessfulScrape(daysAgo(1), listingComplete = true, Nil)), None)
    VenueClosure.sourceDropped(serving, listedEmpty(daysAgo(30)), hasFallback = true, YearRound, now).isLeft shouldBe true
  }

  // With no second source to corroborate it, the primary alone has to hold longer.
  "A venue with no fallback" should "be closed only once its primary has been gone four weeks" in {
    VenueClosure.sourceDropped(gone(daysAgo(27)), None, hasFallback = false, YearRound, now).isLeft shouldBe true
    VenueClosure.sourceDropped(gone(daysAgo(28)), None, hasFallback = false, YearRound, now).value.fallback shouldBe
      VenueClosure.FallbackEvidence.NoFallback
  }

  "The verdict's reason" should "name what is still missing" in {
    VenueClosure.sourceDropped(gone(daysAgo(13)), listedEmpty(daysAgo(15)), hasFallback = true, YearRound, now).left.value should
      include ("gone 13 days")
  }

  // Germany's window is May..September; `now` (2026-09-27) is in season.
  private val openAir = Seasonal(SeasonWindow.Germany)

  private def goneAt(probed: Instant, since: Instant) = ArchivedScrape(cinema, None, lastSuccess = None,
    lastBarren = Some(BarrenAttempt(probed, ScrapeOutcome.Failed, Some(NotFound), Some(since))))

  "A seasonal venue" should "never be judged closed out of its season, however long its page has been gone" in {
    val winter        = Instant.parse("2026-11-20T09:00:00Z")
    val goneAllAutumn = goneAt(winter, since = winter.minus(60, DAYS))
    VenueClosure.sourceDropped(goneAllAutumn, None, hasFallback = false, YearRound, winter).isRight shouldBe true
    VenueClosure.sourceDropped(goneAllAutumn, None, hasFallback = false, openAir, winter).left.value should include ("out of season")
  }

  it should "count a gone spell from the day its season opened, not from the winter before it" in {
    val mayTenth          = Instant.parse("2026-05-10T09:00:00Z")
    val goneSinceFebruary = goneAt(mayTenth, since = Instant.parse("2026-02-01T00:00:00Z"))
    VenueClosure.sourceDropped(goneSinceFebruary, None, hasFallback = false, YearRound, mayTenth).isRight shouldBe true
    VenueClosure.sourceDropped(goneSinceFebruary, None, hasFallback = false, openAir, mayTenth).left.value should include ("gone 9 days")
  }

  it should "be judged like any other venue in season" in {
    VenueClosure.sourceDropped(gone(daysAgo(20)), listedEmpty(daysAgo(15)), hasFallback = true, openAir, now).isRight shouldBe true
  }

  // A second listing that answers 404/410 corroborates the primary as much as one that lists nothing.
  "A venue whose fallback dropped it too" should "be dropped by its sources, quoting what the fallback answered" in {
    val error   = "HttpStatusException: HTTP 410 for GET https://www.sensacine.com/cines/cine/E0001/"
    val dropped = listedEmpty(daysAgo(15)).map(_.copy(emptyFallback = None, fallbackSource = "SensaCine",
      droppedFallback = Some(FallbackState.DroppedSpell(daysAgo(15), now, error))))
    val drop = VenueClosure.sourceDropped(gone(daysAgo(20)), dropped, hasFallback = true, YearRound, now).value
    drop.fallback shouldBe VenueClosure.FallbackEvidence.Dropped("SensaCine", daysAgo(15), now, error)
    VenueClosure.listingSignal(drop).value.observed should (include ("second listing SensaCine") and include (error))
  }

  // ---- the verdict: closed only on evidence about the venue itself ----

  private val dropped: Either[String, VenueClosure.SourceDrop] =
    VenueClosure.sourceDropped(gone(daysAgo(20)), listedEmpty(daysAgo(15)), hasFallback = true, YearRound, now)
  import VenueClosure.{Verdict, VenueSignal}

  "The verdict" should "keep a venue its sources still list, naming what is missing" in {
    VenueClosure.judge(Left("serving"), _ => throw new AssertionError("a listed venue is never checked")) shouldBe Verdict.Listed("serving")
  }

  // ACME Screening Room, Lambertville: flicks.us 404'd it, nothing else listed it, and its own
  // site carried an October festival. The aggregators lost it; the venue had not closed.
  it should "only call a venue source-dropped when its sources dropped it but its own site is live (ACME)" in {
    val live = VenueSignal.Life("own site https://www.acmescreeningroom.org/ answered with no closure notice")
    VenueClosure.judge(dropped, _ => Seq(live)) shouldBe Verdict.SourceDropped(dropped.value, Seq(live))
  }

  it should "only call a venue source-dropped when nothing about the venue itself was seen" in {
    VenueClosure.judge(dropped, _ => Nil) shouldBe a[Verdict.SourceDropped]
    VenueClosure.judge(dropped, _ => Seq(VenueSignal.Silent("own site timed out"))) shouldBe a[Verdict.SourceDropped]
  }

  it should "call a venue closed when its sources dropped it and the venue shows it has closed" in {
    val shut = VenueSignal.Closure("own site https://kino.example/ answered HTTP 404")
    VenueClosure.judge(dropped, _ => Seq(shut)) shouldBe Verdict.Closed(dropped.value, Seq(shut))
  }

  // Retiring an open venue hides it from everyone; keeping a closed one costs a page.
  it should "let any sign of life outweigh a sign of closure" in {
    VenueClosure.judge(dropped, _ => Seq(VenueSignal.Closure("second listing dropped it"), VenueSignal.Life("own site live"))) shouldBe
      a[Verdict.SourceDropped]
  }

  it should "take no closure signal from a fallback that only lists nothing" in {
    VenueClosure.listingSignal(dropped.value) shouldBe None
  }
}
