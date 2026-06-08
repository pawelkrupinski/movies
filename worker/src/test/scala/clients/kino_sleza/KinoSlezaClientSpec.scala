package clients.kino_sleza

import clients.tools.FakeHttpFetch
import models.KinoSleza
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoSlezaClient

import java.time.LocalDateTime

/** Replays the recorded `rcks.pl/kino-sleza/repertuar/` WordPress listing
 *  through the client.
 *
 *  Kino Ślęża was previously scraped from Filmweb, whose API had silently gone
 *  empty for it (every poll returned `[]`) though the cinema is open — this
 *  fixture is the proof its programme is real and reachable on its own site. */
class KinoSlezaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoSlezaClient(new FakeHttpFetch("kino-sleza")).fetch()

  "KinoSlezaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSleza)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening (no booking link — phone reservation only)" in {
    // Fixture: "Toy Story 5" screens 2026-06-26 at 15:30 (6 dates in all).
    val film = movies.find(_.movie.title == "Toy Story 5").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 26, 15, 30))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty
  }

  it should "read genres off the metadata line, not the format tags" in {
    // "Komedia, Dramat // napisy //" → genres are only the part before the `//`.
    val film = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    film.movie.genres shouldBe Seq("Komedia", "Dramat")
  }

  it should "carry a poster for every film" in {
    all(movies.map(_.posterUrl)) shouldBe defined
  }
}
