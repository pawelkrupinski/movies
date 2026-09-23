package clients.kino_narew

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoNarew
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoNarewClient

import java.time.LocalDateTime

/** Replays the recorded `mckispultusk.pl/kino-narew/` repertoire (a WordPress
 *  Neve-theme page, captured 2026-09-23) through the client. */
class PultuskClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoNarewClient(new FakeHttpFetch("kino-narew")).fetch()

  "KinoNarewClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoNarew)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "expand a single date range into one showtime per day, with format tags and production metadata" in {
    val film = movies.find(_.movie.title == "Marsupilami").value
    val dates = film.showtimes.map(_.dateTime)
    dates should contain allOf (LocalDateTime.of(2026, 9, 18, 17, 0), LocalDateTime.of(2026, 9, 23, 17, 0))
    dates should have size 6
    all(film.showtimes.map(_.format)) shouldBe List("2D", "DUB")
    film.movie.countries shouldBe Seq("Francja", "Belgia")
    film.movie.releaseYear.value shouldBe 2026
    film.movie.genres shouldBe Seq("Komedia", "Przygodowy")
    film.movie.runtimeMinutes.value shouldBe 99
  }

  it should "union TWO separate date ranges listed for one film at the shared showtime" in {
    val film = movies.find(_.movie.title.toLowerCase == "100 dni misja zeus").value
    val dates = film.showtimes.map(_.dateTime)
    dates should have size 8
    dates should contain allOf (LocalDateTime.of(2026, 10, 6, 17, 0), LocalDateTime.of(2026, 10, 14, 17, 0))
    dates should not contain LocalDateTime.of(2026, 10, 8, 17, 0) // the gap day between the two ranges
  }
}
