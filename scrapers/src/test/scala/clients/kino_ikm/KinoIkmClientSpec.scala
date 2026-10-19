package clients.kino_ikm

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import models.KinoIkm
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoIkmClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded Kino IKM repertoire (06-06-2026 capture of
 *  `ikm.gda.pl/kino-kunszt-wodny/`) through the client. The WordPress page
 *  server-renders the whole programme as a `div.screeningtable` of
 *  `div.schedulerow`s; each row carries a year-less `DD.MM` date (resolved
 *  against the injected `today`), the film title + page URL, the director line,
 *  and the start time. The fixed `today` makes the year inference
 *  deterministic. */
class KinoIkmClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http  = new FakeHttpFetch("kino-ikm")
  private val today = LocalDate.of(2026, 6, 6)

  "KinoIkmClient" should "parse film screenings off the repertoire page" in {
    val movies = new KinoIkmClient(http, KinoIkm, today).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoIkm)

    // Every film has at least one showtime; all resolve to 2026 (the capture
    // window is 09.06–30.06, inferred from the year-less DD.MM dates).
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "pin a concrete screening with date, time and director off the row" in {
    val movies = new KinoIkmClient(http, KinoIkm, today).fetch()

    // "Drama" (reż. Kristoffer Borgli) screens twice in the captured window:
    // 09.06 at 16:30 and 23.06 at 19:15.
    val drama = movies.find(_.movie.title == "Drama").value
    drama.director should contain("Kristoffer Borgli")
    drama.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 9, 16, 30))
    drama.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 23, 19, 15))

    // The booking link is the cinema's generic ticketing entry point.
    val slot = drama.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 9, 16, 30)).value
    slot.bookingUrl.value should startWith("https://bilety.ikm.gda.pl/rezerwacja/")
  }
}
