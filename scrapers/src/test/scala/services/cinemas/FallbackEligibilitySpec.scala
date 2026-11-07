package services.cinemas

import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{GetOnlyHttpFetch, HttpFetch}
import services.cinemas.common.{AggregatorUrls, CinemaScraper, FallbackEligibility, ListingSource, RetryingCinemaScraper, WebediaMarket, WebediaShowtimesClient}
import services.cinemas.pl.{CinemaCityClient, CinemaCityScraper, FilmwebShowtimesClient, HeliosClient, MultikinoClient}
import services.movies.SingleCountryNormalizer.titleNormalizer

class FallbackEligibilitySpec extends AnyFlatSpec with Matchers {

  private val noHttp: HttpFetch = new GetOnlyHttpFetch { def get(url: String): String = "" }

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
    decorated.listingSource shouldBe ListingSource.Filmweb
    FallbackEligibility.eligible(decorated) shouldBe false
  }

  it should "include an ordinary non-chain, non-Filmweb venue scraper" in {
    val ordinary: CinemaScraper = ScriptedCinemaScraper(List(Right(Seq.empty)), forCinema = Multikino)
    FallbackEligibility.eligible(ordinary) shouldBe true
  }

  "CinemaScraper.scrapedInChunks" should "be true exactly for a chunked scraper, also through a decorator" in {
    val chunked = new WebediaShowtimesClient(noHttp, WebediaMarket.Spain, "E0001", Multikino, today = _root_.tools.SpecClock.PinnedDay)
    chunked.scrapedInChunks                             shouldBe true
    new RetryingCinemaScraper(chunked).scrapedInChunks  shouldBe true
    ScriptedCinemaScraper(List(Right(Seq.empty))).scrapedInChunks shouldBe false
  }

  "AggregatorUrls.isFilmwebFilmUrl" should "recognise the film pages the Filmweb client builds and nothing else" in {
    AggregatorUrls.isFilmwebFilmUrl(FilmwebShowtimesClient.filmPageUrl(123L)) shouldBe true
    AggregatorUrls.isFilmwebFilmUrl("https://kinomuza.pl/film/123")            shouldBe false
  }
}
