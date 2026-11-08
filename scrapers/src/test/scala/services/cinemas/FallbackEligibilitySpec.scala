package services.cinemas

import clients.tools.ScriptedByUrlHttpFetch
import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch
import services.cinemas.common.{AggregatorUrls, CinemaScraper, DelegatingCinemaScraper, FallbackEligibility, RetryingCinemaScraper, WebediaMarket, WebediaShowtimesClient}
import services.cinemas.pl.{CinemaCityClient, CinemaCityScraper, FilmwebShowtimesClient, HeliosClient, MultikinoClient}
import services.movies.SingleCountryNormalizer.titleNormalizer

class FallbackEligibilitySpec extends AnyFlatSpec with Matchers {

  private val noHttp: HttpFetch = new ScriptedByUrlHttpFetch(_ => "")

  "CinemaScraper.chain" should "be true for the chain clients and false by default" in {
    new MultikinoClient(noHttp, titles = titleNormalizer).chain                                                shouldBe true
    new HeliosClient(titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay).chain                                                         shouldBe true
    new CinemaCityScraper(new CinemaCityClient(noHttp, titles = titleNormalizer), "1907", Multikino).chain     shouldBe true
    ScriptedCinemaScraper(List(Right(Seq.empty))).chain                              shouldBe false
  }

  "FallbackEligibility" should "exclude chain scrapers" in {
    FallbackEligibility.eligible(new MultikinoClient(noHttp, titles = titleNormalizer)) shouldBe false
    FallbackEligibility.eligible(new HeliosClient(titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay))         shouldBe false
  }

  it should "exclude a Filmweb-fed primary (falling Filmweb back to Filmweb is moot)" in {
    val filmwebPrimary = new FilmwebShowtimesClient(noHttp, cinemaId = 1, cinema = Multikino)
    FallbackEligibility.eligible(filmwebPrimary) shouldBe false
  }

  it should "exclude a Filmweb-fed primary seen through a decorator (its listing source is forwarded, not its class)" in {
    val decorated = new RetryingCinemaScraper(new FilmwebShowtimesClient(noHttp, cinemaId = 1, cinema = Multikino))
    decorated.readsFilmwebListing shouldBe true
    FallbackEligibility.eligible(decorated) shouldBe false
  }

  it should "include an ordinary non-chain, non-Filmweb venue scraper" in {
    val ordinary: CinemaScraper = ScriptedCinemaScraper(List(Right(Seq.empty)), forCinema = Multikino)
    FallbackEligibility.eligible(ordinary) shouldBe true
  }

  // The chunk routing once matched the concrete type while the timeout decision asked the decorator, so a decorated
  // chunked venue was told it was chunked yet never routed to the chunk fan-out: one fetch(), and no timeout either.
  "CinemaScraper.chunkedScraper" should "be the chunked scraper itself, and through any decorator its delegate's" in {
    val chunked = new WebediaShowtimesClient(noHttp, WebediaMarket.Spain, "E0001", Multikino, today = _root_.tools.SpecClock.PinnedDay)
    val decorated: DelegatingCinemaScraper = new DelegatingCinemaScraper(chunked) { def fetch() = chunked.fetch() }
    chunked.chunkedScraper                                  shouldBe Some(chunked)
    decorated.chunkedScraper                                shouldBe Some(chunked)
    new RetryingCinemaScraper(decorated).chunkedScraper     shouldBe Some(chunked)
    ScriptedCinemaScraper(List(Right(Seq.empty))).chunkedScraper shouldBe None
  }

  "AggregatorUrls.isFilmwebFilmUrl" should "recognise the film pages the Filmweb client builds and nothing else" in {
    AggregatorUrls.isFilmwebFilmUrl(FilmwebShowtimesClient.filmPageUrl(123L)) shouldBe true
    AggregatorUrls.isFilmwebFilmUrl("https://kinomuza.pl/film/123")            shouldBe false
  }
}
