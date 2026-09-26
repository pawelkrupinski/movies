package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration.*

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

  private val T0 = Instant.parse("2026-09-25T16:18:00Z")

  /** Records each request with the instant it was made; answers from `answer`, taking `latency`
   *  of the clock to do it, as a real request does. */
  private final class RecordingGraph(clock: MutableClock, answer: String => FacebookScrape = _ => FacebookScrape.Accepted,
                                     latency: java.time.Duration = java.time.Duration.ZERO) extends FacebookGraph {
    val sent = collection.mutable.Buffer.empty[(Instant, String)]
    def scrape(url: String): FacebookScrape = { sent += clock.instant() -> url; clock.advance(latency); answer(url) }
  }

  /** A film's pages — on its country's own host, as every country's are. */
  private def pagesOf(filmId: String, cities: Int, country: String = "us"): Seq[String] =
    (1 to cities).map(i => s"https://$country.showtimes.cc/$filmId/city$i")

  /** Two countries' workers on one store and one clock, each with its own graph (its own app
   *  credentials — the same app), each film having `cities` pages. */
  private final class Fleet(cities: Int, answer: String => FacebookScrape = _ => FacebookScrape.Accepted,
                            latency: java.time.Duration = java.time.Duration.ZERO) {
    val clock = new MutableClock(T0)
    val store = new InMemoryFacebookRescrapeStore
    val graph = new RecordingGraph(clock, answer, latency)
    val series = new ShareCardMetrics.Series(Seq("us", "uk"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val drains: Map[String, FacebookRescrapeDrain] = Seq("us", "uk").map(cc =>
      cc -> new FacebookRescrapeDrain(store, graph, pagesOf(_, cities, cc), cc, series.forCountry(cc), clock)).toMap
    def request(country: String, filmId: String): Unit =
      new FacebookRescrapeQueue(store, country).request(filmId, clock.instant())
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

  // Each request is due no sooner than when the card it names can be on web_movies. A second
  // request for a film still waiting names a NEWER card: absorbed, it still has to wait for it.
  it should "wait for the later of the two, so the newer card is on its pages when Facebook looks" in {
    val fleet = new Fleet(cities = 1)
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
      if (url.endsWith("city2") && refusedOnce.add(url)) FacebookScrape.Refused("HTTP 500") else FacebookScrape.Accepted)
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
    val drain = new FacebookRescrapeDrain(store, graph,
      id => if (readable) pagesOf(id, 1) else throw new IllegalStateException("read-model card unreadable"),
      "us", ShareCardMetrics.noop, clock)
    new FacebookRescrapeQueue(store, "us").request("film", clock.instant())
    drain.tick()
    store.waiting.map(_.key) shouldBe Seq("film|us|film")
    graph.sent shouldBe empty
    readable = true
    clock.advance(java.time.Duration.ofMillis(backoff(1).toMillis))
    drain.tick()
    graph.sent.map(_._2).toSeq shouldBe pagesOf("film", 1)
    store.waiting shouldBe empty
  }

  "The waiting gauge" should "count the country's pages still to send" in {
    val fleet = new Fleet(cities = 4)
    fleet.request("us", "film")
    fleet.drains("us").tick()
    fleet.series.rescrapesWaitingFor("us") shouldBe 3.0
  }
}
