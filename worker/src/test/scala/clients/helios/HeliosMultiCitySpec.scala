package clients.helios

import clients.tools.FakeHttpFetch
import models.HeliosBlueCity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.HeliosNuxt

/** HeliosClient/HeliosNuxt used to hard-code Poznań's page slug + REST source
 *  UUID + `Helios` cinema. These pin the per-cinema config that lets the same
 *  parser serve every Helios venue. */
class HeliosMultiCitySpec extends AnyFlatSpec with Matchers {

  "A HeliosCinema config" should "derive its page URL from the city + cinema slug" in {
    HeliosNuxt.Magnolia.pageUrl shouldBe "https://helios.pl/wroclaw/kino-helios-magnolia/repertuar"
    HeliosNuxt.BlueCity.baseUrl shouldBe "https://helios.pl/warszawa/kino-helios-blue-city"
    HeliosNuxt.Poznan.pageUrl   shouldBe "https://helios.pl/poznan/kino-helios/repertuar"
    // Trójmiasto: two Gdańsk venues + one Gdynia, each with its own REST UUID.
    HeliosNuxt.Metropolia.pageUrl shouldBe "https://helios.pl/gdansk/kino-helios-metropolia/repertuar"
    HeliosNuxt.Forum.pageUrl      shouldBe "https://helios.pl/gdansk/kino-helios-forum/repertuar"
    HeliosNuxt.Riviera.pageUrl    shouldBe "https://helios.pl/gdynia/kino-helios/repertuar"
    Seq(HeliosNuxt.Metropolia, HeliosNuxt.Forum, HeliosNuxt.Riviera)
      .map(_.sourceId).distinct.size shouldBe 3
  }

  "HeliosNuxt.buildMovies" should "tag rows and links with the configured cinema, not Poznań" in {
    // Parse the recorded Poznań page but with the Blue City config — buildMovies
    // does no I/O, so the output cinema + URLs come purely from the config.
    val html   = new FakeHttpFetch("helios/rest-enrichment").get(HeliosNuxt.Poznan.pageUrl)
    val movies = HeliosNuxt.buildMovies(html, HeliosNuxt.BlueCity)

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(HeliosBlueCity)
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { url =>
      url should include (HeliosNuxt.BlueCity.sourceId)
    }
    movies.flatMap(_.filmUrl).foreach { url =>
      url should startWith (HeliosNuxt.BlueCity.baseUrl)
    }
  }
}
