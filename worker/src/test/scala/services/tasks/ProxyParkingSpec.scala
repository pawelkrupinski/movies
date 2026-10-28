package services.tasks

import models.{Cinema, CinemaMovie, Helios, KinoApollo, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.StubCinemaScraper
import services.cinemas.common.{CinemaScrapeRunner, CinemaScraper}
import services.freshness.InMemoryFreshnessStore
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.scrapes.InMemoryScrapeArchiveRepository
import tools.{AllBackendsFailedException, HttpStatusException, MutableClock, ProxyChain, ProxyProbe}

import java.time.{Instant, LocalDateTime}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._

/**
 * Parking by cause (2026-10-27, Decodo answering every CONNECT with 503): a venue that failed on the residential-proxy
 * leg while the proxy's probe says it is down is parked on the proxy, and every such venue is due again within one
 * probe interval of the SAME probe passing — not a whole freshness window later. A venue failing for its own reasons
 * keeps its own backoff.
 */
class ProxyParkingSpec extends AnyFlatSpec with Matchers {

  private val start = Instant.parse("2026-10-27T18:00:00Z")

  /** A probe the spec flips, counting its asks. */
  private final class FlippableProbe extends ProxyProbe {
    val up    = new AtomicBoolean(false)
    val asked = new AtomicInteger(0)
    def passes(): Boolean = { asked.incrementAndGet(); up.get }
  }

  /** What a proxied venue's fetch throws while Decodo is down: the proxy leg fails, the direct leg is refused. */
  private def proxyDown(): Nothing =
    throw new AllBackendsFailedException("All 2 backends failed for get https://www.cineworld.co.uk/x",
      Seq(ProxyChain.ProxyLeg, "fallback"))

  private final class Rig(clock: MutableClock, proxied: Seq[Cinema], own: Seq[Cinema]) {
    val probe     = new FlippableProbe
    val fresh     = new InMemoryFreshnessStore
    val queue     = new InMemoryTaskQueue
    val parking   = new ProxyParking(probe, clock)
    val policy    = new ScrapeFreshnessPolicy(fresh, clock = clock, proxyParking = Some(parking))
    private def listing(c: Cinema) = Seq(CinemaMovie(Movie("Dune"), c, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.ofInstant(start, java.time.ZoneOffset.UTC).plusDays(1), Some("https://book")))))
    val scrapers: Seq[StubCinemaScraper] =
      proxied.map(c => new StubCinemaScraper(c, if (probe.up.get) listing(c) else proxyDown())) ++
        own.map(c => new StubCinemaScraper(c, throw new HttpStatusException(500, "GET", "https://own.example", None)))
    val reaper    = new ScrapeReaper(scrapers, queue, fresh, clock = clock)
    private val runner = new CinemaScrapeRunner(new services.identity.IdentityListingIntake(
      new InMemoryScrapeArchiveRepository, new InMemoryScrapeArchiveRepository, new services.movies.InMemoryScrapeGuardLedger,
      titleNormalizer, 3, clock, services.movies.ListingIntakeMetrics.noop), clock)
    val handler   = new ScrapeCinemaHandler(
      scrapers.map(s => ScrapeCinemaHandler.scraperKey(s.cinema) -> (s: CinemaScraper)).toMap,
      runner, fresh, new DueWindow(60.minutes), clock, scrapeFreshness = Some(policy))

    /** One reaper tick, then the worker runs everything it enqueued. What it ran, by venue. */
    def round(): Seq[Cinema] = {
      reaper.tick(clock.instant())
      val ran = Seq.newBuilder[Cinema]
      var next = queue.claim("w", 1.minute, clock.instant())
      while (next.isDefined) {
        handler.handle(next.get)
        queue.complete(next.get.id, "w")
        ran += scrapers.find(s => ScrapeCinemaHandler.dedupKey(s.cinema) == next.get.dedupKey).get.cinema
        next = queue.claim("w", 1.minute, clock.instant())
      }
      ran.result()
    }
  }

  "a venue failing on the residential proxy" should "be parked on the proxy and due again within one probe interval of it passing" in {
    val clock = new MutableClock(start)
    val rig   = new Rig(clock, proxied = Seq(Multikino, Helios, KinoApollo), own = Seq(KinoMuza))

    // The proxy is down: every venue spends its retry budget and is parked.
    (1 to 4).foreach { _ => rig.round(); clock.advanceSeconds(60) }
    rig.policy.parkedByCause shouldBe Map(ProxyParking.Cause.ResidentialProxyDown -> 3, ProxyParking.Cause.OwnFailures -> 1)
    rig.round() shouldBe empty // all parked: nothing is due until something releases them

    // While it stays down, the recovery check releases nothing.
    rig.policy.releaseRecoveredProxy() shouldBe 0

    // Decodo is back. Within one probe interval the recovery check runs and releases every venue parked on it…
    rig.probe.up.set(true)
    clock.advance(java.time.Duration.ofMillis(ProxyParking.ProbeInterval.toMillis))
    rig.policy.releaseRecoveredProxy() shouldBe 3

    // …so the very next reaper tick scrapes them, and they land. The venue failing on its own is not released.
    rig.round() should contain theSameElementsAs Seq(Multikino, Helios, KinoApollo)
    rig.policy.parkedByCause shouldBe Map(ProxyParking.Cause.ResidentialProxyDown -> 0, ProxyParking.Cause.OwnFailures -> 1)
  }

  it should "keep its own backoff when the probe says the proxy works — then the failure is the venue's own" in {
    val clock = new MutableClock(start)
    val rig   = new Rig(clock, proxied = Seq(Multikino), own = Nil)
    rig.probe.up.set(true)
    val failing = new ScrapeFreshnessPolicy(rig.fresh, clock = clock, proxyParking = Some(rig.parking))
    val key     = ScrapeCinemaHandler.dedupKey(Multikino)
    (1 to 3).foreach(_ => failing.failed(key, Some(scala.util.Try(proxyDown()).failed.get)))

    failing.parkedByCause shouldBe Map(ProxyParking.Cause.ResidentialProxyDown -> 0, ProxyParking.Cause.OwnFailures -> 1)
    failing.releaseRecoveredProxy() shouldBe 0
  }

  "the recovery check" should "not probe at all while nothing is parked on the proxy" in {
    val clock = new MutableClock(start)
    val rig   = new Rig(clock, proxied = Nil, own = Seq(KinoMuza))
    (1 to 4).foreach { _ => rig.round(); clock.advanceSeconds(60) }
    (1 to 10).foreach(_ => rig.policy.releaseRecoveredProxy())
    rig.probe.asked.get shouldBe 0
  }

  "parking" should "ask the probe at most once per interval, however many venues fail inside it" in {
    val clock   = new MutableClock(start)
    val probe   = new FlippableProbe
    val parking = new ProxyParking(probe, clock)
    (1 to 50).foreach(i => parking.parkIfProxyDown(s"scrape|venue-$i") shouldBe true)
    probe.asked.get shouldBe 1
    clock.advance(java.time.Duration.ofMillis(ProxyParking.ProbeInterval.toMillis + 1))
    parking.parkIfProxyDown("scrape|one-more") shouldBe true
    probe.asked.get shouldBe 2
  }

  "AllBackendsFailedException.legFailed" should "find a failed leg however the scraper wrapped the failure" in {
    val composite = scala.util.Try(proxyDown()).failed.get
    val wrapped   = new RuntimeException("Cineworld scrape failed", composite)
    val retried   = new RuntimeException("gave up after 3 attempts")
    retried.addSuppressed(wrapped)
    AllBackendsFailedException.legFailed(retried, ProxyChain.ProxyLeg) shouldBe true
    AllBackendsFailedException.legFailed(new RuntimeException("parse error"), ProxyChain.ProxyLeg) shouldBe false
    AllBackendsFailedException.legFailed(new AllBackendsFailedException("x", Seq("zyte", "direct")), ProxyChain.ProxyLeg) shouldBe false
  }
}
