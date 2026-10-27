package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.facebook.InMemoryFacebookFetches
import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration.*
import scala.util.Try

/**
 * The fleet's Facebook re-scrapes: every country's worker drains ONE queue on ONE quota.
 *
 * The case that shaped it (2026-09-25): a film screening in 265 US cities was re-scraped as 265
 * back-to-back requests, a refusal re-sent all of them, and Facebook refused the app for an hour
 * — in every country. Here the same shape is a burst of pages, and the specs pin that it leaves
 * the fleet one request per slot, re-sends only what failed, and stops the whole fleet on a rate
 * limit.
 */
class FacebookRescrapeDrainSpec extends AnyFlatSpec with Matchers {
  import FacebookRescrapeDrain.*
  import services.facebook.AppUsage
  import services.facebook.FacebookUsageBrake.{UsageCeiling, UsageHold}

  private val T0 = Instant.parse("2026-09-25T16:18:00Z")

  /** Records each request with the instant it was made; answers from `answer`, with Facebook's
   *  app usage from `usage`, taking `latency` of the clock to do it, as a real request does. */
  private final class RecordingGraph(clock: MutableClock, answer: String => FacebookScrape = _ => FacebookScrape.Accepted,
                                     latency: java.time.Duration = java.time.Duration.ZERO,
                                     usage: () => Option[AppUsage] = () => None) extends FacebookGraph {
    val sent = collection.mutable.Buffer.empty[(Instant, String)]
    def scrape(url: String): FacebookReply = { sent += clock.instant() -> url; clock.advance(latency); FacebookReply(answer(url), usage()) }
  }

  /** A film's pages — on its country's own host, as every country's are. */
  private def pagesOf(filmId: String, cities: Int, country: String = "us"): Seq[String] =
    (1 to cities).map(i => s"https://$country.showtimes.cc/$filmId/city$i")

  /** Two countries' workers on one store and one clock, each with its own graph (its own app
   *  credentials — the same app), each film having `cities` pages. */
  private final class Fleet(cities: Int, answer: String => FacebookScrape = _ => FacebookScrape.Accepted,
                            latency: java.time.Duration = java.time.Duration.ZERO, usage: () => Option[AppUsage] = () => None,
                            val store: InMemoryFacebookRescrapeStore = new InMemoryFacebookRescrapeStore) {
    val clock = new MutableClock(T0)
    val fetches = new InMemoryFacebookFetches
    val graph = new RecordingGraph(clock, answer, latency, usage)
    val series = new ShareCardMetrics.Series(Seq("us", "uk"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val drains: Map[String, FacebookRescrapeDrain] = Seq("us", "uk").map(cc =>
      cc -> new FacebookRescrapeDrain(store, graph, pagesOf(_, cities, cc), fetches, cc, series.forCountry(cc), clock, _root_.tools.Stopwatch.System)).toMap
    /** A re-scrape of a film whose every page Facebook has fetched — shared, so holding a preview. */
    def request(country: String, filmId: String): Unit = {
      fetches.record(pagesOf(filmId, cities, country).map(_ -> clock.instant()).toMap)
      new FacebookRescrapeQueue(store, country).request(filmId, clock.instant())
    }
    /** Every worker ticks every [[TickEvery]] for `span`. */
    def run(span: FiniteDuration): Unit =
      (1L to span.toMillis / TickEvery.toMillis).foreach { _ => drains.values.foreach(_.tick()); clock.advance(java.time.Duration.ofMillis(TickEvery.toMillis)) }
  }

  "A burst of pages across the fleet" should "be sent one request per slot, whichever country sends it" in {
    val fleet = new Fleet(cities = 40)
    fleet.request("us", "doctor-who"); fleet.request("uk", "doctor-who")
    fleet.run(30.minutes)
    val at = fleet.graph.sent.map(_._1).toSeq
    at.size shouldBe 80
    at.zip(at.tail).map { case (a, b) => java.time.Duration.between(a, b).toMillis }.min should be >= (Spacing - TickEvery).toMillis
    fleet.graph.sent.map(_._2).toSet shouldBe (pagesOf("doctor-who", 40, "us") ++ pagesOf("doctor-who", 40, "uk")).toSet
    fleet.store.waiting shouldBe empty
  }

  // The drain ticks on a fixed DELAY, so a tick that sends starts the next one late by the
  // request's own time. Slots handed out as "now + spacing" let every late tick push the schedule
  // back: live on 2026-09-26 the backlog drained at ~133 pages an hour, not the quota's 180.
  "A backlog" should "drain at the quota's pace, however long each request takes" in {
    val fleet = new Fleet(cities = 60, latency = java.time.Duration.ofSeconds(2))
    fleet.request("us", "film")
    fleet.run(30.minutes)
    val at   = fleet.graph.sent.map(_._1).toSeq.take(60)
    val gaps = at.zip(at.tail).map { case (a, b) => java.time.Duration.between(a, b).toMillis }
    gaps.sum.toDouble / gaps.size should be <= (Spacing.toMillis * 1.05)
    gaps.min should be >= (Spacing - TickEvery).toMillis
  }

  "A second request for a film or page already waiting" should "be absorbed by the entry that waits" in {
    val fleet = new Fleet(cities = 3)
    fleet.request("us", "film"); fleet.request("us", "film")
    fleet.store.waiting.map(_.key) shouldBe Seq("film|us|film")
    fleet.drains("us").tick()                          // expanded, and the first page sent
    fleet.request("us", "film")                        // a later change asks again: two of its pages still wait
    fleet.run(5.minutes)
    // The page already sent goes again (it shows the older card); the two that waited go once.
    fleet.graph.sent.map(_._2).toSeq.sorted shouldBe (pagesOf("film", 3) :+ pagesOf("film", 3).head).sorted
  }

  // A request that lands while its page is being sent names a card newer than the one Facebook is
  // fetching: absorbed into the claimed entry, and the entry deleted once the send succeeded, the
  // newer card was never asked for.
  it should "send the page again when it was asked for while being sent" in {
    var fleet: Fleet = null
    var askedDuringSend = false
    fleet = new Fleet(cities = 1, answer = { _ =>
      if (!askedDuringSend) { askedDuringSend = true; fleet.store.add(Seq(RescrapeEntry("us", RescrapeTarget.Page(pagesOf("film", 1).head), fleet.clock.instant()))) }
      FacebookScrape.Accepted
    })
    fleet.request("us", "film")
    fleet.run(2.minutes)
    fleet.graph.sent.map(_._2).toSeq shouldBe Seq.fill(2)(pagesOf("film", 1).head)
    fleet.store.waiting shouldBe empty
  }

  // Each request is due no sooner than when the card it names can be on web_movies. A second
  // request for a film still waiting names a NEWER card: absorbed, it still has to wait for it.
  it should "wait for the later of the two, so the newer card is on its pages when Facebook looks" in {
    val fleet = new Fleet(cities = 1)
    fleet.fetches.record(Map(pagesOf("film", 1).head -> T0))
    val queue = new FacebookRescrapeQueue(fleet.store, "us")
    queue.request("film", T0.plusSeconds(60))
    queue.request("film", T0.plusSeconds(110))
    fleet.run(100.seconds)
    fleet.graph.sent shouldBe empty
    fleet.run(30.seconds)
    fleet.graph.sent.map(_._2).toSeq shouldBe pagesOf("film", 1)
  }

  "A page Facebook refuses" should "be retried alone, after a back-off, and the others not re-sent" in {
    val refusedOnce = collection.mutable.Set.empty[String]
    val fleet = new Fleet(cities = 3, answer = url =>
      if (url.endsWith("city2") && refusedOnce.add(url)) FacebookScrape.Refused("HTTP 400 code 100 (#100) The url is blocked") else FacebookScrape.Accepted)
    fleet.request("us", "film")
    fleet.run(10.minutes)
    fleet.graph.sent.map(_._2).toSeq shouldBe (pagesOf("film", 3) :+ pagesOf("film", 3)(1))
    val times = fleet.graph.sent.collect { case (at, url) if url.endsWith("city2") => at }
    java.time.Duration.between(times.head, times.last).toMillis should be >= backoff(1).toMillis
    fleet.series.rescrapeCount("us", ShareCardMetrics.RescrapeOutcome.Failed) shouldBe 1.0
    fleet.series.rescrapeCount("us", ShareCardMetrics.RescrapeOutcome.Sent) shouldBe 3.0
  }

  it should "be given up after the last attempt" in {
    val fleet = new Fleet(cities = 1, answer = _ => FacebookScrape.Refused("HTTP 403 code 100 (#100) The url is blocked"))
    fleet.request("us", "film")
    fleet.run(2.hours)
    fleet.graph.sent.size shouldBe MaxAttempts
    fleet.store.waiting shouldBe empty
  }

  "Facebook's rate limit" should "stop every country's requests for the hold, and keep the page" in {
    var limited = true
    val fleet = new Fleet(cities = 2, answer = _ =>
      if (limited) FacebookScrape.RateLimited("HTTP 403 code 4 (#4) Application request limit reached") else FacebookScrape.Accepted)
    fleet.request("us", "us-film")
    fleet.run(1.minute)
    fleet.graph.sent.size shouldBe 1
    limited = false
    fleet.request("uk", "uk-film")                     // another country, same app: it waits too
    fleet.run(RateLimitHold - 2.minutes)
    fleet.graph.sent.size shouldBe 1
    fleet.run(10.minutes)
    fleet.graph.sent.map(_._2).toSet shouldBe (pagesOf("us-film", 2) ++ pagesOf("uk-film", 2, "uk")).toSet
    fleet.series.rescrapeCount("us", ShareCardMetrics.RescrapeOutcome.RateLimited) shouldBe 1.0
  }

  it should "not spend the page's attempts: a limit that outlasts them still delivers the page" in {
    var refusals = MaxAttempts + 2
    val fleet = new Fleet(cities = 1, answer = _ =>
      if (refusals > 0) { refusals -= 1; FacebookScrape.RateLimited("HTTP 429") } else FacebookScrape.Accepted)
    fleet.request("us", "film")
    fleet.run(RateLimitHold * (MaxAttempts + 3).toLong)
    fleet.graph.sent.size shouldBe MaxAttempts + 3
    fleet.store.waiting shouldBe empty
  }

  "Facebook unreachable for longer than a page's attempts" should "still deliver the page, and pause the fleet while it lasts" in {
    var down = true
    val fleet = new Fleet(cities = 1, answer = _ => if (down) FacebookScrape.Unavailable("HttpConnectTimeoutException") else FacebookScrape.Accepted)
    fleet.request("us", "film")
    fleet.run(UnavailableHold * (MaxAttempts + 2).toLong)
    fleet.graph.sent.size should be <= (MaxAttempts + 3)       // one request per hold, not one per tick
    fleet.store.waiting.map(_.key).size shouldBe 1            // not dropped, however long it lasts
    down = false
    fleet.run(UnavailableHold + 1.minute)
    fleet.store.waiting shouldBe empty
    fleet.graph.sent.last._2 shouldBe pagesOf("film", 1).head
  }

  "A worker that dies mid-request" should "leave its page to be claimed again once the lease runs out" in {
    val fleet = new Fleet(cities = 1)
    fleet.request("us", "film")
    fleet.clock.advanceSeconds(1)
    fleet.drains("us").tick()                          // film expanded, page sent and done
    fleet.graph.sent.size shouldBe 1
    fleet.request("us", "other")
    val store = fleet.store
    store.claim("us", RescrapeKind.Film, fleet.clock.instant(), Lease) shouldBe defined   // a worker claims, then dies
    fleet.run(Lease - 1.minute)
    fleet.graph.sent.size shouldBe 1
    fleet.run(2.minutes)
    fleet.graph.sent.map(_._2).last shouldBe pagesOf("other", 1).head
  }

  "A film whose pages cannot be read" should "wait and try again, not be reported done" in {
    val clock = new MutableClock(T0)
    val store = new InMemoryFacebookRescrapeStore
    var readable = false
    val graph = new RecordingGraph(clock)
    val fetches = new InMemoryFacebookFetches
    fetches.record(Map(pagesOf("film", 1).head -> T0))
    val drain = new FacebookRescrapeDrain(store, graph,
      id => if (readable) pagesOf(id, 1) else throw new IllegalStateException("read-model card unreadable"),
      fetches, "us", ShareCardMetrics.noop, clock, _root_.tools.Stopwatch.System)
    new FacebookRescrapeQueue(store, "us").request("film", clock.instant())
    // A tick whose film pages could not be read caught that and completed facebook-rescrape-drain all the same, so a
    // read model unreadable for hours never went stale. It is not a completed run now.
    val ticks = new _root_.tools.RecordingTickRecorder
    def tick(): Unit = _root_.tools.ScheduledTick.recorded("facebook-rescrape-drain", TickEvery, play.api.Logger(getClass), ticks)(drain.tick())
    tick()
    store.waiting.map(_.key) shouldBe Seq("film|us|film")
    graph.sent shouldBe empty
    ticks.completedRuns shouldBe empty
    readable = true
    clock.advance(java.time.Duration.ofMillis(backoff(1).toMillis))
    tick()
    graph.sent.map(_._2).toSeq shouldBe pagesOf("film", 1)
    store.waiting shouldBe empty
    ticks.completedRuns should have size 1
  }

  it should "be dropped after the last attempt, not wait in the fleet's queue for good" in {
    val clock = new MutableClock(T0)
    val store = new InMemoryFacebookRescrapeStore
    var reads = 0
    val graph = new RecordingGraph(clock)
    val drain = new FacebookRescrapeDrain(store, graph,
      _ => { reads += 1; throw new IllegalStateException("read-model card unreadable") }, new InMemoryFacebookFetches, "us", ShareCardMetrics.noop, clock, _root_.tools.Stopwatch.System)
    new FacebookRescrapeQueue(store, "us").request("film", clock.instant())
    (1 to MaxAttempts + 3).foreach { attempt =>
      drain.tick()
      clock.advance(java.time.Duration.ofMillis(backoff(attempt).toMillis + Lease.toMillis))
    }
    reads shouldBe MaxAttempts
    store.waiting shouldBe empty
    graph.sent shouldBe empty
  }

  // Facebook holds a preview only of a page it fetched: one somebody shared — a stale one after the
  // card changes. A page it never fetched is still worth asking for (its first share then finds
  // the card ready), but with what the allowance leaves: on 2026-10-12 Germany's first week queued
  // ~4,100 city pages while Facebook had fetched 30-140 a day.
  "A film's pages Facebook has not fetched within a preview's lifetime" should "be asked for after the ones it fetched" in {
    val fleet = new Fleet(cities = 4)
    val pages = pagesOf("film", 4)
    fleet.fetches.record(Map(pages(0) -> T0, pages(2) -> T0.minusSeconds(29L * 86400), pages(1) -> T0.minusSeconds(31L * 86400)))
    new FacebookRescrapeQueue(fleet.store, "us").request("film", T0)
    fleet.run(5.minutes)
    val sent = fleet.graph.sent.map(_._2).toSeq
    sent.take(2).toSet shouldBe Set(pages(0), pages(2))
    sent.drop(2).toSet shouldBe Set(pages(1), pages(3))
    fleet.store.waiting shouldBe empty
  }

  "Pages Facebook never fetched" should "wait behind fetched pages queued after them" in {
    val fleet = new Fleet(cities = 1)
    val unfetched = (1 to 3).map(i => s"https://us.showtimes.cc/old/city$i")
    fleet.store.add(unfetched.map(url => RescrapeEntry("us", RescrapeTarget.Page(url, previewed = false), T0)))
    fleet.clock.advanceSeconds(60)
    fleet.request("us", "film")
    fleet.run(Spacing * 5)
    fleet.graph.sent.map(_._2).toSeq shouldBe (pagesOf("film", 1) ++ unfetched)
    fleet.store.waiting shouldBe empty
  }

  // Facebook refused the app after 45-105 requests an hour on 2026-10-11/12, and each refusal held
  // the fleet an hour. Its X-App-Usage says how close the app is BEFORE the refusal.
  "Facebook's app usage at the ceiling" should "hold every country's re-scrapes a while, then resume at the quota's pace" in {
    var usage = UsageCeiling
    val fleet = new Fleet(cities = 3, usage = () => Some(AppUsage(usage)))
    fleet.request("us", "us-film"); fleet.request("uk", "uk-film")
    fleet.run(UsageHold - 1.minute)
    fleet.graph.sent.size shouldBe 1
    Seq("us", "uk").map(fleet.series.appUsageFor).filterNot(_.isNaN).max shouldBe UsageCeiling.toDouble
    usage = UsageCeiling - 1
    fleet.run(1.minute + Spacing * 3)
    fleet.graph.sent.size shouldBe 4
    val resumed = fleet.graph.sent.map(_._1).toSeq
    java.time.Duration.between(resumed.head, resumed(1)).toMillis should be >= UsageHold.toMillis
    resumed.drop(1).zip(resumed.drop(2)).map { case (a, b) => java.time.Duration.between(a, b).toMillis }.max should be <= (Spacing + TickEvery).toMillis
  }

  // 2026-10-15: each send spent ~1.5% of the allowance, and the first hour's calls had not aged out
  // yet, so every resume after a flat five-minute hold added one more: 91% → 96% → 99%.
  it should "wait twice as long for every point over the ceiling" in {
    val fleet = new Fleet(cities = 3, usage = () => Some(AppUsage(UsageCeiling + 2)))
    fleet.request("us", "us-film")
    fleet.run(20.minutes - 1.minute)
    fleet.graph.sent.size shouldBe 1
    fleet.run(1.minute + Spacing)
    fleet.graph.sent.size shouldBe 2
  }

  // The worst hour: a backlog from an idle start, so none of the hour's calls age out, each request
  // spending ~1.5% of the allowance (2026-10-15), and Facebook's reading LAGGING five requests behind
  // what it has counted. The fleet still stops short of 90%, ten points under the refusal: what
  // reached 99% under a flat hold at 90 drew no refusal, but where Facebook refuses is unmeasured.
  it should "keep the real usage under 90% through an hour no call ages out of, with a reading five requests late" in {
    val PerRequest = 1.5
    val Lag = 5
    var sent = 0
    val fleet = new Fleet(cities = 200, usage = () => { sent += 1; Some(AppUsage(((sent - Lag).max(0) * PerRequest).toInt)) })
    fleet.request("us", "big-film")
    fleet.run(1.hour)
    (sent * PerRequest) should be < 90.0
  }

  // The hold is written after Facebook answered: a fleet database that refuses it must not leave
  // the accepted page claimed, to be sent again when its lease runs out.
  "A usage hold the fleet database cannot take" should "not send the page Facebook accepted again" in {
    val refusing = new InMemoryFacebookRescrapeStore {
      override def holdSlots(until: Instant): Unit = throw new IllegalStateException("fleet database down")
    }
    val fleet = new Fleet(cities = 1, usage = () => Some(AppUsage(UsageCeiling)), store = refusing)
    fleet.request("us", "film")
    (1L to (Lease + 1.minute).toMillis / TickEvery.toMillis).foreach { _ =>
      Try(fleet.drains("us").tick()); fleet.clock.advance(java.time.Duration.ofMillis(TickEvery.toMillis))
    }
    fleet.graph.sent.size shouldBe 1
    fleet.store.waiting shouldBe empty
  }

  "The waiting gauge" should "count the country's pages still to send, fetched by Facebook or not" in {
    val fleet = new Fleet(cities = 4)
    fleet.request("us", "film")
    fleet.store.add(Seq(RescrapeEntry("us", RescrapeTarget.Page("https://us.showtimes.cc/never/city1", previewed = false), T0)))
    fleet.drains("us").tick()
    fleet.series.rescrapesWaitingFor("us") shouldBe 4.0
  }
}
