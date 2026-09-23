package clients.kino_grunwald

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoGrunwald
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoGrunwaldClient

import java.time.LocalDate
import java.time.LocalDateTime

/** Replays the recorded `kino.olsztynek.com.pl` home page (a "WYSIWYG Web
 *  Builder 8" page shipped as raw ISO-8859-2, no charset header, captured
 *  2026-09-23) through the client. */
class OlsztynekClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoGrunwaldClient(new FakeHttpFetch("kino-grunwald"), today = LocalDate.of(2026, 9, 23)).fetch()

  "KinoGrunwaldClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoGrunwald)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "decode the ISO-8859-2 page correctly and title-case the shouted title, dropping the promo suffix" in {
    val film = movies.find(_.movie.title == "Lalka").value
    val dates = film.showtimes.map(_.dateTime)
    dates should contain allOf (LocalDateTime.of(2026, 10, 2, 16, 0), LocalDateTime.of(2026, 10, 2, 19, 0))
    dates.size should be >= 10
  }

  it should "drop a listed title that carries no schedule yet" in {
    movies.map(_.movie.title) should not contain "100 Dni: Misja Zeus"
  }
}
