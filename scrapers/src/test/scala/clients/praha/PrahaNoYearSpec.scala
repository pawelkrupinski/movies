package clients.praha

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.PrahaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded 2026-09-12 capture of mteatr.pl/pl/repertuar-kino-praha,
  * taken after the site dropped the 4-digit year from its date stamp
  * entirely — "12 Wrz (Sb) / 16:10" instead of "09 Wrz 2026 (Śr) / 16:00".
  * `PrahaClient.StampPat` required the year, so every stamp on the page
  * failed to parse and the venue went white again: 0 films, 0 showtimes,
  * four days after the weekday-label fix (see PrahaWeekdayLabelSpec). */
class PrahaNoYearSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today = LocalDate.of(2026, 9, 12)
  private val movies = new PrahaClient(new FakeHttpFetch("kino-praha-no-year-2026-09"), today = today).fetch()

  "PrahaClient" should "not be empty against a yearless stamp" in {
    movies should not be empty
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "read '12 Wrz (Sb) / 16:10' as 2026-09-12 16:10, inferring the year from `today`" in {
    val showtimes = movies.flatMap(_.showtimes).map(_.dateTime)
    showtimes should contain(LocalDateTime.of(2026, 9, 12, 16, 10))
  }

  it should "still exclude the badge label, not the weekday-carrying date stamp" in {
    val showtimes = movies.flatMap(_.showtimes).map(_.dateTime)
    showtimes.count(_.toLocalDate == LocalDate.of(2026, 9, 17)) should be > 0
  }
}
