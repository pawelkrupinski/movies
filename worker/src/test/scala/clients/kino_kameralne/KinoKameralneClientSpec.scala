package clients.kino_kameralne

import clients.tools.FakeHttpFetch
import models.KinoKameralne
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoKameralneClient

import java.time.LocalDateTime

/** Replays the recorded Kino Kameralne Cafe programme (06-06-2026 capture of
 *  `biletyna.pl/Gdansk/Kino-Kameralne-Cafe`) through the client. The page is
 *  server-rendered and carries the full schedule as a schema.org `Place`
 *  JSON-LD block whose `events` array is one `ScreeningEvent` per screening —
 *  `name` (title), `startDate` (ISO-8601), `url` (booking/film page), `image`
 *  (poster). */
class KinoKameralneClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-kameralne")

  "KinoKameralneClient" should "parse film screenings off the biletyna JSON-LD" in {
    val movies = new KinoKameralneClient(http, KinoKameralne).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKameralne)

    // Every film has at least one showtime, all in the captured 2026 window.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "read the exact start time and booking link off each ScreeningEvent" in {
    val movies = new KinoKameralneClient(http, KinoKameralne).fetch()

    // Pinned screening from the fixture: "Mikey i Nicky (1976)" opens the
    // 06-06-2026 day at 18:00; the booking link is the biletyna film page.
    val mikey = movies.find(_.movie.title == "Mikey i Nicky (1976)").value
    mikey.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 18, 0))
    val slot = mikey.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 6, 18, 0)).value
    slot.bookingUrl.value shouldBe "https://biletyna.pl/film/Mikey-i-Nicky-1976?eid=667728#opis"
  }
}
