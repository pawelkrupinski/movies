package services.cinemas

import models.Multikino
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{GetOnlyHttpFetch, HttpFetch}

class FallbackEligibilitySpec extends AnyFlatSpec with Matchers {

  private val noHttp: HttpFetch = new GetOnlyHttpFetch { def get(url: String): String = "" }

  "CinemaScraper.chain" should "be true for the chain clients and false by default" in {
    new MultikinoClient(noHttp).chain                                                shouldBe true
    new HeliosClient().chain                                                         shouldBe true
    new CinemaCityScraper(new CinemaCityClient(noHttp), "1907", Multikino).chain     shouldBe true
    ScriptedCinemaScraper(List(Right(Seq.empty))).chain                              shouldBe false
  }

  "FallbackEligibility" should "exclude chain scrapers" in {
    FallbackEligibility.eligible(new MultikinoClient(noHttp)) shouldBe false
    FallbackEligibility.eligible(new HeliosClient())         shouldBe false
  }

  it should "exclude a Filmweb-fed primary (falling Filmweb back to Filmweb is moot)" in {
    val filmwebPrimary = new FilmwebShowtimesClient(noHttp, cinemaId = 1, cinema = Multikino)
    FallbackEligibility.eligible(filmwebPrimary) shouldBe false
  }

  it should "include an ordinary non-chain, non-Filmweb venue scraper" in {
    val ordinary: CinemaScraper = ScriptedCinemaScraper(List(Right(Seq.empty)), forCinema = Multikino)
    FallbackEligibility.eligible(ordinary) shouldBe true
  }
}
