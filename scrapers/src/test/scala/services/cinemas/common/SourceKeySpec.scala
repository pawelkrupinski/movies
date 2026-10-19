package services.cinemas.common

import clients.tools.FakeHttpFetch
import models.{KinoBaszta, KinoBasztaSroda}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.Bilety24OrganizerClient
import services.movies.SingleCountryNormalizer.titleNormalizer

class SourceKeySpec extends AnyFlatSpec with Matchers {

  "CinemaScraper.urlKey" should "name one listing however its URL is spelled" in {
    CinemaScraper.urlKey("https://www.Example.pl/repertuar/") shouldBe "example.pl/repertuar"
    CinemaScraper.urlKey("http://example.pl/repertuar") shouldBe "example.pl/repertuar"
  }

  // bilety24 routes an organiser page by its trailing id alone, so Braniewo's
  // `kino-baszta-w-braniewie-477` served Środa's `kino-baszta-477`.
  "a bilety24 organiser's sourceKey" should "be its id, whatever slug fronts it" in {
    val http = new FakeHttpFetch("does-not-exist")
    def organiser(url: String, cinema: models.Cinema) = new Bilety24OrganizerClient(http, url, cinema, titles = titleNormalizer)
    organiser("https://www.bilety24.pl/kino/organizator/kino-baszta-w-braniewie-477", KinoBaszta).sourceKey shouldBe
      organiser("https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBasztaSroda).sourceKey
  }
}
