package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ResolverDecision
import services.movies.ListingKey

import java.time.{Duration, Instant}
import scala.concurrent.duration._

class CachingReviewSourceSpec extends AnyFlatSpec with Matchers {
  import ReviewFixtures._

  private val now = Instant.parse("2026-10-06T10:00:00Z")

  // Klondike's slot written an hour ago, Kafka's 47 h 59 min 55 s ago: inside a 48 h look-back now, outside it ten seconds on.
  private val written: Map[ListingKey, SlotFacts] = Map(Matched -> SlotFacts(VenueFacts(), now.minus(Duration.ofHours(1))),
    Held -> SlotFacts(VenueFacts(), now.minus(Duration.ofHours(48)).plusSeconds(5)))

  /** The fixtures' decisions and `written`, counting the whole-corpus reads; `failing` makes the next ones throw. */
  private final class Counting(var held: Seq[ResolverDecision] = Seq(heldDecision)) extends ReviewSource {
    private val source = new InMemoryReviewSource(Nil, written)
    export source.{decisions as _, updatedSince as _, *}
    var decisionReads, slotReads = 0
    var failing = false
    var lastSince: Option[Instant] = None
    def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] = {
      decisionReads += 1; if (failing) throw new IllegalStateException("mirror down")
      new InMemoryReviewSource(held).decisions(unmatchedOnly) }
    def updatedSince(since: Instant): Map[String, Instant] = {
      slotReads += 1; lastSince = Some(since); if (failing) throw new IllegalStateException("mirror down"); source.updatedSince(since) }
  }

  /** The re-reads queued behind the answers, run when a case says so. */
  private final class Behind extends java.util.concurrent.Executor {
    private val queued = scala.collection.mutable.Queue.empty[Runnable]
    def execute(r: Runnable): Unit = synchronized(queued.enqueue(r))
    def runAll(): Unit = Iterator.continually(synchronized(queued.removeHeadOption())).takeWhile(_.isDefined).flatten.foreach(_.run())
  }

  private def caching(underlying: ReviewSource, clock: tools.MutableClock, behind: Behind = new Behind) =
    new CachingReviewSource(underlying, clock, refreshAfter = 30.seconds, expireAfter = 10.minutes, ticker = clock.ticker,
      refreshOn = behind)

  "the whole-corpus reads" should "be read once while they are fresh, each kind of decision read apart" in {
    val underlying = new Counting
    val source     = caching(underlying, new tools.MutableClock(now))
    source.decisions(unmatchedOnly = false) shouldBe Seq(heldDecision)
    source.decisions(unmatchedOnly = false) shouldBe Seq(heldDecision)
    underlying.decisionReads shouldBe 1
    source.decisions(unmatchedOnly = true)
    underlying.decisionReads shouldBe 2
  }

  they should "be answered from what is kept once stale, and read again behind that answer" in {
    val underlying = new Counting
    val clock      = new tools.MutableClock(now)
    val behind     = new Behind
    val source     = caching(underlying, clock, behind)
    source.decisions(unmatchedOnly = false)
    underlying.held = Seq(heldDecision, vetoedDecision)
    clock.advanceSeconds(31)
    source.decisions(unmatchedOnly = false) shouldBe Seq(heldDecision)                  // at once, as kept
    underlying.decisionReads shouldBe 1
    behind.runAll()                                                                      // the re-read queued behind it
    underlying.decisionReads shouldBe 2
    source.decisions(unmatchedOnly = false) shouldBe Seq(heldDecision, vetoedDecision)
  }

  they should "be read again before answering once nobody has looked for longer than they are trusted" in {
    val underlying = new Counting
    val clock      = new tools.MutableClock(now)
    val source     = caching(underlying, clock)
    source.decisions(unmatchedOnly = false)
    underlying.held = Seq(vetoedDecision)
    clock.advance(Duration.ofMinutes(11))
    source.decisions(unmatchedOnly = false) shouldBe Seq(vetoedDecision)
  }

  they should "keep the read a failed refresh would have replaced" in {
    val underlying = new Counting
    val clock      = new tools.MutableClock(now)
    val behind     = new Behind
    val source     = caching(underlying, clock, behind)
    source.decisions(unmatchedOnly = false)
    underlying.failing = true
    clock.advanceSeconds(31)
    source.decisions(unmatchedOnly = false) shouldBe Seq(heldDecision)
    behind.runAll()
    underlying.decisionReads shouldBe 2                                                  // it was tried, and failed
    source.decisions(unmatchedOnly = false) shouldBe Seq(heldDecision)
  }

  "the slot times" should "serve a later look-back of the same window from one read, only the rows written since it" in {
    val underlying = new Counting
    val clock      = new tools.MutableClock(now)
    val source     = caching(underlying, clock)
    val (klondike, kafka) = (ListingKey.serialised(Matched), ListingKey.serialised(Held))
    source.updatedSince(now.minus(Duration.ofHours(48))).keySet shouldBe Set(klondike, kafka)
    clock.advanceSeconds(10)
    source.updatedSince(clock.instant().minus(Duration.ofHours(48))).keySet shouldBe Set(klondike)
    underlying.slotReads shouldBe 1
    source.updatedSince(clock.instant().minus(Duration.ofHours(2)))                    // another window: another read
    underlying.slotReads shouldBe 2
  }

  they should "never be read from later than the look-back asked for" in {
    val underlying = new Counting
    val source     = caching(underlying, new tools.MutableClock(now))
    val since      = now.minus(Duration.ofHours(48)).plusMillis(250)
    source.updatedSince(since)
    underlying.lastSince.get.isAfter(since) shouldBe false
  }

  "the per-card reads" should "be read once while fresh for the same rows, and again behind the answer once stale" in {
    val reads = scala.collection.mutable.ListBuffer.empty[String]
    var feedScreenings = 3
    val underlying: ReviewSource = new ReviewSource {
      private val source = new InMemoryReviewSource(Seq(heldDecision), written)
      export source.{slots as _, venuePages as _, feeds as _, films as _, filmRecords as _, *}
      def slots(listingKeys: Seq[String])        = { reads += "slots"; source.slots(listingKeys) }
      def venuePages(urls: Seq[String])          = { reads += "pages"; source.venuePages(urls) }
      def feeds(listings: Seq[(String, String)]) = { reads += "feeds"
        listings.map(_ -> ListingFeed(Nil, feedScreenings, None, None)).toMap }
      def films(tmdbIds: Seq[Int])               = { reads += "films"; source.films(tmdbIds) }
      def filmRecords(tmdbIds: Seq[Int])         = { reads += "records"; source.filmRecords(tmdbIds) }
    }
    val clock  = new tools.MutableClock(now)
    val behind = new Behind
    val source = caching(underlying, clock, behind)
    val feed   = Seq("Kino Opalenica" -> "FRANZ KAFKA")
    def readAll() = { source.slots(Seq(ListingKey.serialised(Held))); source.venuePages(Seq("https://kino.example/franz"))
      source.films(Seq(1157322)); source.filmRecords(Seq(1157322)); source.feeds(feed) }
    readAll(); readAll()
    reads.sorted shouldBe Seq("feeds", "films", "pages", "records", "slots")
    source.feeds(Seq("Kino Opalenica" -> "MACBETH"))                                     // other rows: another read
    reads.count(_ == "feeds") shouldBe 2

    feedScreenings = 5
    clock.advanceSeconds(31)
    source.feeds(feed)(feed.head).screenings shouldBe 3                                  // at once, as kept
    behind.runAll()
    source.feeds(feed)(feed.head).screenings shouldBe 5
  }
}
