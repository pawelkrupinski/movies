package services.identity

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}
import services.observations.ObservationStore
import settings.{IdentityShadowInterval, IdentityShadowLookupRate}
import tools.{DaemonExecutors, HttpFetch, HttpStatusException, RateLimitedHttpFetch, TestWiring}

import java.time.{Clock, Instant, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._

/** The shadow run's paced live lookup fill: asks exactly the resolver's unobserved questions, once
 *  each, into the observation store only; never more than its rate allows; stops at the first
 *  sign of overload; and the next shadow resolve reads what it filed. */
class ShadowLookupFillSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private def listing(venue: Cinema, title: String, year: Option[Int], director: Seq[String]): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, year, director), title, title, title, year, director, None, None, None)
  // Forty films, each a title search and a director walk the store has never seen.
  private val listings = (1 to 40).map(i => listing(if (i % 2 == 0) Multikino else Helios, s"Film number $i", Some(2026), Seq(s"Director $i")))

  /** The service: every request counted; `failWith` answers the given request numbers with an
   *  error. The body is an empty result set — this spec is about which questions are asked and
   *  where the answers go, not what TMDB says (`ShadowIdentityReaperIntegrationSpec` replays real
   *  answers). */
  private final class Service(failWith: Map[Int, Throwable] = Map.empty) extends HttpFetch {
    val requests = mutable.ArrayBuffer.empty[String]
    private def answer(url: String): String = {
      requests += url
      failWith.get(requests.size).foreach(e => throw e)
      """{"results":[],"crew":[],"cast":[]}"""
    }
    override def get(url: String): String                                    = answer(url)
    override def get(url: String, headers: Map[String, String]): String      = answer(url)
    override def post(url: String, body: String, contentType: String): String = answer(url)
  }

  private def fill(store: ObservationStore, service: HttpFetch, rate: Int = 600, sleeps: mutable.Buffer[Long] = mutable.Buffer.empty,
                   rounds: mutable.Buffer[ShadowLookupRound] = mutable.Buffer.empty) =
    new ShadowLookupFill(() => listings, store, new clients.TmdbClient(_, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()), service, Nil,
      normalizer, IdentityCalibration.default, IdentityShadowLookupRate(rate), IdentityShadowInterval(30.minutes), rounds += _,
      DaemonExecutors.directExecutor(), sleeps += _)

  private def store() = ObservationStore.inMemory(Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC))
  private def gapsOf(s: ObservationStore) = {
    val (lookups, gaps) = ObservedIdentityLookups.over(s, new clients.TmdbClient(_, apiKey = Some(settings.TmdbApiKey("other"))), Nil)
    IdentityResolver.resolve(listings, lookups, normalizer)
    gaps.total
  }

  "a fill round" should "ask every unobserved question once, into the store, so the next shadow resolve has no gap" in {
    val observations = store()
    val service      = new Service
    gapsOf(observations) should be > 0L
    val first = fill(observations, service).round()
    first.asked shouldBe service.requests.size
    first.asked should be > 0
    service.requests.distinct.size shouldBe service.requests.size   // deduplicated by observation key
    first.deferred shouldBe 0
    gapsOf(observations) shouldBe 0L

    // Everything is observed now: the next round asks nothing.
    fill(observations, service).round().asked shouldBe 0
    service.requests.size shouldBe first.asked
  }

  it should "ask no more than its rate allows over the round's window, at its pace, and defer the rest" in {
    val service = new Service
    val sleeps  = mutable.Buffer.empty[Long]
    // 1 a minute over a 30-minute window: 30 asks, one a minute.
    val round = fill(store(), service, rate = 1, sleeps = sleeps).round()
    val unlimited = fill(store(), new Service).round().asked
    unlimited should be > 30
    round.asked shouldBe 30
    service.requests.size shouldBe 30
    round.deferred should be > 0
    sleeps.toSeq shouldBe Seq.fill(29)(60000L)
  }

  it should "stop at the first overload, and run the next round at half the rate, doubling back once clean" in {
    val service = new Service(Map(2 -> new HttpStatusException(429, "GET", "https://api.themoviedb.org/3/search/movie", None)))
    val rounds  = mutable.Buffer.empty[ShadowLookupRound]
    val f       = fill(store(), service, rate = 8, rounds = rounds)
    val first   = f.round()
    first.backedOff shouldBe true
    first.asked shouldBe 2
    service.requests.size shouldBe 2
    f.effectiveRate shouldBe IdentityShadowLookupRate(4)
    f.round().rate shouldBe IdentityShadowLookupRate(4)
    f.effectiveRate shouldBe IdentityShadowLookupRate(8)
    rounds.map(_.rate.perMinute) shouldBe Seq(8, 4)
  }

  "the pipeline's own requests" should "wait on a shared paced host no longer than one interval per shadow ask, the shadow capped by its rate" in {
    // A virtual clock: sleeping advances it. The pipeline asks the paced host every interval — at
    // the host's full capacity — and the shadow at its capped rate, both through ONE shared pacer.
    val interval = 1000L
    var clock    = 0L
    val pacer    = new RateLimitedHttpFetch(new HttpFetch {
      override def get(url: String): String                                    = "{}"
      override def post(url: String, body: String, contentType: String): String = "{}"
    }, _ => Some(interval.millis), now = () => Instant.ofEpochMilli(clock), sleep = ms => clock += ms)
    val rate     = IdentityShadowLookupRate(6)   // one every 10 s
    val budget   = new ShadowLookupBudget(rate.allowanceOver(10.minutes), rate.pace, _ => ())
    val live     = new ShadowLiveFetch(pacer, budget)
    val horizon  = 10.minutes.toMillis
    val events   = ((0L until horizon by interval).map(_ -> "pipeline") ++ (0L until horizon by rate.pace.toMillis).map(_ -> "shadow"))
      .sortBy { case (t, who) => (t, who) }
    var pipelineWait = 0L   // the longest any one pipeline request waited
    var shadowAsks   = 0
    events.foreach { case (t, who) =>
      clock = clock.max(t)
      if (who == "shadow") { if (scala.util.Try(live.get("https://paced.example/x")).isSuccess) shadowAsks += 1 }
      else { val before = clock; pacer.get("https://paced.example/y"); pipelineWait = pipelineWait.max(clock - before) }
    }
    shadowAsks shouldBe rate.allowanceOver(10.minutes)
    pipelineWait should be <= shadowAsks * interval
  }
}
