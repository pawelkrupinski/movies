package clients.kino_scena_kultura

import clients.tools.FakeHttpFetch
import models.KinoScenaKultura
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoScenaKulturaClient

import java.time.LocalDateTime

/** Replays the recorded `www.kinoscenakultura.pl/repertuar` listing through the
 *  client — proving it groups each film's screenings by title and reads the
 *  local date+time off the `/repertuar/<slug>/YYYY-MM-DD-HH-mm` link suffix.
 *
 *  Tickets are sold via biletyna.pl, whose events array is currently empty for
 *  the venue, so the cinema's own site is the only live source of its
 *  programme — this fixture is the proof it's real and reachable there. */
class KinoScenaKulturaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new KinoScenaKulturaClient(new FakeHttpFetch("kino-scena-kultura"), KinoScenaKultura).fetch()

  "KinoScenaKulturaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoScenaKultura)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening read off the title-link date suffix" in {
    // Fixture: "Dzień objawienia" screens 2026-06-16 at 17:30 (10 dates in all).
    val film = movies.find(_.movie.title == "Dzień objawienia").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 16, 17, 30))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty
  }

  it should "read genres off the attribute line, not the age/runtime tags" in {
    // "Thriller, Sci-Fi | 12+ | 124 min." → genres are only the part before `|`.
    val film = movies.find(_.movie.title == "Dzień objawienia").value
    film.movie.genres shouldBe Seq("Thriller", "Sci-Fi")
  }
}
