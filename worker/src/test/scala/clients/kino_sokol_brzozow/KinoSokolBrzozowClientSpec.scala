package clients.kino_sokol_brzozow

import clients.tools.FakeHttpFetch
import models.KinoSokolBrzozow
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoSokolBrzozowClient

import java.time.LocalDateTime

/** Replays the recorded `bdk.brzozow.pl/kino/` WordPress-Theatre listing for
 *  Kino Sokół (Brzozów) through the client.
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class KinoSokolBrzozowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoSokolBrzozowClient(new FakeHttpFetch("kino-sokol-brzozow")).fetch()

  "KinoSokolBrzozowClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSokolBrzozow)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening with its date read off the event" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("mandalorian")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 16, 45))
  }
}
