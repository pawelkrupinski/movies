package clients.kino_zacisze

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoZacisze
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoZaciszeClient

import java.time.LocalDateTime

/** Replays the recorded `kinozacisze.pl/repertuar/` WordPress listing for Kino
 *  Zacisze (Piekary Śląskie) through the client.
 *
 *  Previously scraped via biletyna.pl, which had stopped carrying the film
 *  programme — the schedule now lives only on the venue's own site. */
class KinoZaciszeClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoZaciszeClient(new FakeHttpFetch("kino-zacisze")).fetch()

  "KinoZaciszeClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoZacisze)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening with the date read off the day container" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("toy story")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 15, 30))
  }

  it should "split the trailing release year out of the shouted title" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("toy story")).value
    film.movie.title shouldBe "TOY STORY 5"
    film.movie.releaseYear.value shouldBe 2026
  }

  it should "carry the systembiletowy booking link off each showtime" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("toy story")).value
    val first = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 25, 15, 30)).value
    first.bookingUrl.value should include("systembiletowy.pl")
  }
}
