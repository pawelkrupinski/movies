package clients.kino_sfinks

import clients.tools.FakeHttpFetch
import models.KinoSfinks
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoSfinksClient

import java.time.LocalDateTime

/** Replays the recorded harmonogram page (06-06-2026 capture, page 1 of 6)
 *  through the client. The schedule is a table whose date cell is rendered once
 *  per day and blank on the day's later rows — the parser carries the date
 *  forward, which is what lets the 18:00/19:00 rows under 06-06-2026 resolve to
 *  the right day. Only the page-1 fixture is on disk, so the paginator's
 *  `-strona-2…6.html` follow-up fetches throw in `FakeHttpFetch` and are
 *  tolerated (dropped), leaving the page-1 screenings. */
class KinoSfinksClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-sfinks")

  "KinoSfinksClient" should "parse film screenings off the harmonogram table" in {
    val movies = new KinoSfinksClient(http, KinoSfinks).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSfinks)

    // Every film has at least one showtime with a plausible 2026 date.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)

    // Pinned concrete screening seen in the captured fixture: the day's first
    // row carries the date, so this verifies the carry-forward anchor too.
    val dyrygent = movies.find(_.movie.title == "DYRYGENT").value
    dyrygent.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 17, 0))
    dyrygent.showtimes.flatMap(_.bookingUrl).head should startWith("https://ksk.systembiletowy.pl/")
  }

  it should "carry the date forward onto rows whose date cell is blank" in {
    val movies = new KinoSfinksClient(http, KinoSfinks).fetch()

    // ORŁY REPUBLIKI's 18:00 row on 06-06-2026 has an empty date cell — it must
    // inherit the 17:00 DYRYGENT row's date rather than dropping out.
    val orly = movies.find(_.movie.title == "ORŁY REPUBLIKI").value
    orly.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 18, 0))
  }
}
