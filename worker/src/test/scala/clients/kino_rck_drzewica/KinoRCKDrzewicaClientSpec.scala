package clients.kino_rck_drzewica

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoRCKDrzewica
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoRCKDrzewicaClient

import java.time.LocalDateTime

/** Replays the recorded `bilety.rck.drzewica.pl/rezerwacja/termin.html?idg=1`
 *  page (the venue's iKsoris booking backend, captured 2026-09-23) through
 *  the client — the structured, complete source; the venue's own news-post
 *  schedule is unstructured text and misses two of these four showings. */
class KinoRCKDrzewicaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoRCKDrzewicaClient(new FakeHttpFetch("kino-rck-drzewica")).fetch()

  "KinoRCKDrzewicaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoRCKDrzewica)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "merge a film's showtimes across the two days it screened" in {
    val film = movies.find(_.movie.title == "Psi Patrol i Dinozaury").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 26, 15, 0), LocalDateTime.of(2026, 9, 27, 15, 0)
    )
  }

  it should "read the runtime and countries off the show-description text" in {
    val film = movies.find(_.movie.title == "Psi Patrol i Dinozaury").value
    film.movie.runtimeMinutes.value shouldBe 89
    film.movie.countries should contain allOf ("Kanada", "USA")
  }

  it should "recognise the dub/subtitle badge among the header labels" in {
    movies.find(_.movie.title == "Psi Patrol i Dinozaury").value.showtimes.head.format shouldBe List("DUB")
    movies.find(_.movie.title == "Niebo nad Normandią").value.showtimes.head.format shouldBe List("NAP")
  }

  it should "carry the iKsoris booking link" in {
    val film = movies.find(_.movie.title == "Psi Patrol i Dinozaury").value
    film.showtimes.flatMap(_.bookingUrl).head should include("bilety.rck.drzewica.pl/rezerwacja/numerowane.html")
  }

  it should "list every film screening in the verified window, including the single Wednesday-morning showing" in {
    movies.map(_.movie.title) should contain allOf ("Mistyczka", "VAIANA")
    val vaiana = movies.find(_.movie.title == "VAIANA").value
    vaiana.showtimes.map(_.dateTime) should contain (LocalDateTime.of(2026, 9, 30, 9, 0))
  }
}
