package clients.charlie

import clients.tools.FakeHttpFetch
import models.KinoCharlie
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CharlieClient

import java.time.LocalDateTime

/** Replays Kino Charlie's recorded homepage (06-06-2026 capture) through the
 *  client. The page is raw ISO-8859-2 with NO charset declared, so the fixture
 *  is read as bytes via `FakeHttpFetch.getBytes` and decoded ISO-8859-2 in the
 *  client — a UTF-8 decode would mangle every Polish title. Each screening is a
 *  schema.org `ScreeningEvent` JSON-LD block; the booking link lives in the
 *  HTML row preceding it. */
class CharlieClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-charlie")

  "CharlieClient" should "parse film screenings off the ld+json blocks" in {
    val movies = new CharlieClient(http, KinoCharlie).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoCharlie)
    all(movies.map(_.showtimes)) should not be empty

    // Every showtime is on the captured day.
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet shouldBe
      Set(java.time.LocalDate.of(2026, 6, 6))
  }

  it should "decode Polish characters from the ISO-8859-2 page" in {
    val movies = new CharlieClient(http, KinoCharlie).fetch()

    // "Diabeł ubiera się u Prady 2" — ł and ę survive only with the right
    // charset; a UTF-8 decode would leave U+FFFD here.
    val diabel = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    diabel.movie.title should not include "�"
    diabel.director should contain("David Frankel")
    diabel.posterUrl.value should startWith("https://www.charlie.pl/")
  }

  it should "pin a concrete screening with its booking link" in {
    val movies = new CharlieClient(http, KinoCharlie).fetch()

    val diabel = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    diabel.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 12, 10))

    val booking = diabel.showtimes.flatMap(_.bookingUrl)
    all(booking) should startWith("https://bilety.charlie.pl/MSI/Default.aspx?event_id=")
  }
}
