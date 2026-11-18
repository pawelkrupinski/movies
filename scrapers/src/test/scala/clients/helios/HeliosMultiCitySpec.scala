package clients.helios

import clients.tools.FakeHttpFetch
import models.HeliosBlueCity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{HeliosNuxt, HeliosVenues}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** HeliosClient/HeliosNuxt used to hard-code Poznań's page slug + REST source
 *  UUID + `Helios` cinema. These pin the per-cinema config that lets the same
 *  parser serve every Helios venue. */
class HeliosMultiCitySpec extends AnyFlatSpec with Matchers {

  "A HeliosCinema config" should "derive its page URL from the city + cinema slug" in {
    HeliosVenues.Magnolia.pageUrl shouldBe "https://helios.pl/wroclaw/kino-helios-magnolia/repertuar"
    HeliosVenues.BlueCity.baseUrl shouldBe "https://helios.pl/warszawa/kino-helios-blue-city"
    HeliosVenues.Poznan.pageUrl   shouldBe "https://helios.pl/poznan/kino-helios/repertuar"
    // Trójmiasto: two Gdańsk venues + one Gdynia, each with its own REST UUID.
    HeliosVenues.Metropolia.pageUrl shouldBe "https://helios.pl/gdansk/kino-helios-metropolia/repertuar"
    HeliosVenues.Forum.pageUrl      shouldBe "https://helios.pl/gdansk/kino-helios-forum/repertuar"
    HeliosVenues.Riviera.pageUrl    shouldBe "https://helios.pl/gdynia/kino-helios/repertuar"
    Seq(HeliosVenues.Metropolia, HeliosVenues.Forum, HeliosVenues.Riviera)
      .map(_.sourceId).distinct.size shouldBe 3
  }

  "HeliosNuxt.buildMovies" should "tag rows and links with the configured cinema, not Poznań" in {
    // Parse the recorded Poznań page but with the Blue City config — buildMovies
    // does no I/O, so the output cinema + URLs come purely from the config.
    val html   = new FakeHttpFetch("helios/rest-enrichment").get(HeliosVenues.Poznan.pageUrl)
    val movies = HeliosNuxt.buildMovies(html, HeliosVenues.BlueCity, titleNormalizer)

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(HeliosBlueCity)
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { url =>
      url should include (HeliosVenues.BlueCity.sourceId)
    }
    movies.flatMap(_.filmUrl).foreach { url =>
      url should startWith (HeliosVenues.BlueCity.baseUrl)
    }
  }
}
