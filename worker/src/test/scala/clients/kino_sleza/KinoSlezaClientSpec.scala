package clients.kino_sleza

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import models.KinoSleza
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoSlezaClient

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

  // …and the part AFTER the `//` is the language version, which this cinema
  // states nowhere else — its titles are the bare film name, so nothing else on
  // the page tells a dubbed screening from a subtitled one.
  it should "read the language version off the same metadata line" in {
    val subtitled = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    all(subtitled.showtimes.map(_.format)) shouldBe List("NAP")

    val dubbed = movies.find(_.movie.title == "Toy Story 5").value
    all(dubbed.showtimes.map(_.format)) shouldBe List("DUB")
  }

  // "Animacja // PL //" is a Polish-language film, not a version — the unmarked
  // case, and the one a loose reading would have badged.
  it should "leave a Polish-language film unmarked" in {
    val polish = movies.filter(_.showtimes.exists(_.format.isEmpty))
    polish should not be empty
  }

}
