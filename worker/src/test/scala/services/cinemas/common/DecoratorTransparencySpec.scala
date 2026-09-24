package services.cinemas.common

import models.{CinemaMovie, KinoMuranow}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.fallback.InMemoryFallbackStore
import tools.HostScrapeStats

import java.util.concurrent.Executors

/**
 * Every scrape DECORATOR must answer for its delegate on everything except `fetch()`.
 *
 * They did not, and it cost the UK its advance-booking programme.
 * `ScrapeChunkReduceHandler` publishes a partial reduce as
 * `PreScrapedCinemaScraper(listingComplete = false)` so `MovieCache` skips its prune —
 * but `WorkerWiring.publishScrape` wraps that in a recording/fallback decorator before
 * `CinemaScrapeRunner` reads the flag, and every decorator hand-copied `cinema` +
 * `scrapeHosts` while silently inheriting `CinemaScraper`'s default for the rest. All
 * three answered `listingIsComplete = true`, so the signal never reached the cache and
 * every film that only screened on a missing date was pruned as "stopped screening".
 *
 * `chain`, `maxFetchAttempts` and `sourceUrl` were in the same state and merely happened
 * to be read off the undecorated scraper at every call site — which is not a property, it
 * is luck. This asserts every member at once, against a delegate that answers the OPPOSITE
 * of the trait default for all of them, so a future field added to `CinemaScraper` and not
 * forwarded by [[DelegatingCinemaScraper]] fails here.
 */
class DecoratorTransparencySpec extends AnyFlatSpec with Matchers {

  /** Answers the non-default value for every member a decorator has to carry. */
  private val delegate = new CinemaScraper {
    val cinema                              = KinoMuranow
    def scrapeHosts: Set[String]            = Set("delegate.test")
    def fetch(): Seq[CinemaMovie]           = Seq.empty
    override def maxFetchAttempts: Int      = 7
    override def chain: Boolean             = true
    override def listingIsComplete: Boolean = false
    override def sourceUrl: Option[String]  = Some("https://delegate.test/repertuar")
    override def sourceKey: Option[String]  = Some("delegate.test/venue-42")
    override def chainVenueId: Option[String] = Some("42")
  }

  private val executor = Executors.newSingleThreadExecutor()

  private val decorators: Seq[(String, CinemaScraper)] = Seq(
    "RetryingCinemaScraper"  -> new RetryingCinemaScraper(delegate),
    "AdaptiveTimeoutScraper" -> new AdaptiveTimeoutScraper(delegate, new HostScrapeStats(), executor),
    "UptimeRecordingScraper" -> new UptimeRecordingScraper(delegate, new UptimeMonitor()),
    "SourceFallbackScraper"  -> new SourceFallbackScraper(delegate,
      fallback = () => None, fallbackName = "Flicks", fallbackRef = () => None,
      new UptimeMonitor(), new InMemoryFallbackStore())
  )

  // The chunked reduce publishes its listing as a PreScraped stand-in for the live
  // scraper, through the same decorators — so it must answer as that scraper would.
  "PreScrapedCinemaScraper.of" should "carry every identity of the scraper it stands in for" in {
    val stand = PreScrapedCinemaScraper.of(delegate, () => Seq.empty)
    withClue("cinema: ")(stand.cinema shouldBe delegate.cinema)
    withClue("scrapeHosts: ")(stand.scrapeHosts shouldBe delegate.scrapeHosts)
    withClue("maxFetchAttempts: ")(stand.maxFetchAttempts shouldBe delegate.maxFetchAttempts)
    withClue("chain: ")(stand.chain shouldBe delegate.chain)
    withClue("sourceUrl: ")(stand.sourceUrl shouldBe delegate.sourceUrl)
    withClue("sourceKey: ")(stand.sourceKey shouldBe delegate.sourceKey)
    withClue("chainVenueId: ")(stand.chainVenueId shouldBe delegate.chainVenueId)
    PreScrapedCinemaScraper.of(delegate, () => Seq.empty, listingComplete = false).listingIsComplete shouldBe false
  }

  decorators.foreach { case (name, decorated) =>
    it should s"carry every delegate answer through $name" in {
      withClue("cinema: ")(decorated.cinema shouldBe delegate.cinema)
      withClue("scrapeHosts: ")(decorated.scrapeHosts shouldBe delegate.scrapeHosts)
      withClue("maxFetchAttempts: ")(decorated.maxFetchAttempts shouldBe delegate.maxFetchAttempts)
      withClue("chain: ")(decorated.chain shouldBe delegate.chain)
      // The one that shipped broken: false here is what stops the cache pruning films a
      // short listing never mentioned.
      withClue("listingIsComplete: ")(decorated.listingIsComplete shouldBe delegate.listingIsComplete)
      withClue("sourceUrl: ")(decorated.sourceUrl shouldBe delegate.sourceUrl)
      withClue("sourceKey: ")(decorated.sourceKey shouldBe delegate.sourceKey)
      withClue("chainVenueId: ")(decorated.chainVenueId shouldBe delegate.chainVenueId)
    }
  }

  it should "reach MovieCache as a short listing once the reduce says so" in {
    // End to end over the seam that broke: the reduce's PreScraped listing, wrapped the way
    // `publishScrape` wraps it, is what the runner reads the flag off.
    val partial = PreScrapedCinemaScraper.of(delegate, () => Seq.empty[CinemaMovie], listingComplete = false)
    new UptimeRecordingScraper(partial, new UptimeMonitor()).listingIsComplete shouldBe false
  }
}
