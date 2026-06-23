package clients.kino_zbyszek

import clients.tools.FakeHttpFetch
import models.KinoZbyszek
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoZbyszekClient

import java.time.LocalDateTime

/** Replays the recorded `dok.pl/kino` Drupal calendar block for Kinoteatr
 *  Zbyszek (Dzierżoniowski Ośrodek Kultury) through the client.
 *
 *  Previously scraped via biletyna.pl, which no longer carries this venue's
 *  film repertoire — only the SCHEDULE moved to dok.pl (tickets still sell on
 *  biletyna, surfaced as each showtime's booking link). */
class KinoZbyszekClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoZbyszekClient(new FakeHttpFetch("kino-zbyszek")).fetch()

  "KinoZbyszekClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoZbyszek)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "carry the real repertoire titles, sentence-cased from the ALL-CAPS source" in {
    val titles = movies.map(_.movie.title)
    titles should contain("Drzewo magii")
    titles should contain("Ojczyzna")
  }

  it should "drop the KINO NIECZYNNE (cinema-closed) placeholder" in {
    movies.map(_.movie.title.toLowerCase) should not contain "kino nieczynne"
  }

  it should "pin a concrete screening with its date+time read off the calendar" in {
    val film = movies.find(_.movie.title.equalsIgnoreCase("Ojczyzna")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 26, 18, 0))
  }

  it should "attach the biletyna booking link to each screening" in {
    val film = movies.find(_.movie.title.equalsIgnoreCase("Ojczyzna")).value
    film.showtimes.flatMap(_.bookingUrl).foreach(_ should include("biletyna.pl"))
    film.showtimes.flatMap(_.bookingUrl) should not be empty
  }
}
