package clients.praha

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.PrahaClient

import java.time.LocalDateTime

/** Replays a recorded 2026-09-08 capture of mteatr.pl/pl/repertuar-kino-praha,
  * taken after the site started inserting a weekday abbreviation in
  * parentheses between the year and the slash — "09 Wrz 2026 (Śr) / 16:00"
  * instead of "09 Wrz 2026 / 16:00". `PrahaClient.StampPat` required the
  * slash immediately after the year, so every stamp on the page failed to
  * parse and the venue went white: 0 films, 0 showtimes. */
class PrahaWeekdayLabelSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new PrahaClient(new FakeHttpFetch("kino-praha-weekday-label-2026-09")).fetch()

  "PrahaClient" should "not be empty against a stamp carrying a weekday abbreviation" in {
    movies should not be empty
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "read '09 Wrz 2026 (Śr) / 16:00' despite the inserted weekday" in {
    val showtimes = movies.flatMap(_.showtimes).map(_.dateTime)
    showtimes should contain(LocalDateTime.of(2026, 9, 9, 16, 0))
  }

  it should "still exclude the badge label, not the weekday-carrying date stamp" in {
    // "17 Wrz 2026" carries a special-1 badge ("Cykl filmowy Wajda: re-wizje")
    // alongside its ordinary date label; the badge text must never be parsed
    // as the stamp.
    val showtimes = movies.flatMap(_.showtimes).map(_.dateTime)
    showtimes.count(_.toLocalDate == java.time.LocalDate.of(2026, 9, 17)) should be > 0
  }
}
