package clients.kino_sokol_brzozow

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoSokolBrzozow
import services.cinemas.pl.KinoSokolBrzozowClient

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

  // The venue line ("2D dubbing pl" / "2D napisy pl") is the ONLY place this
  // cinema states its language version — every title on the page is the bare
  // film name, so the central `FormatTags` title strip has nothing to peel and
  // a dubbed screening was indistinguishable from a subtitled one on the badge.
  it should "read the language version off the event's venue line" in {
    val dubbed = movies.find(_.movie.title.toLowerCase.contains("mandalorian")).value
    all(dubbed.showtimes.map(_.format)) shouldBe List("2D", "DUB")

    val subtitled = movies.find(_.movie.title.toLowerCase.contains("objawienia")).value
    all(subtitled.showtimes.map(_.format)) shouldBe List("2D", "NAP")
  }
}
