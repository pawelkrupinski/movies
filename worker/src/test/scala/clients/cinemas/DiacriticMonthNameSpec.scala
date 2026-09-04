package clients.cinemas

import clients.tools.FakeHttpFetch
import models.{KinoAgrafka, KinoPodBaranami, OkfIluzja}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{KinoAgrafkaClient, KinoBulgarskaClient, KinoPodBaranamiClient, OkfIluzjaClient}

import java.time.{LocalDate, LocalDateTime}

/** Four hand-rolled Polish scrapers read their day header as "<day> <month
  * name> [<year>]" and looked the month up in `ScraperParse.PolishMonths`. All
  * four spelled the month group `\w`, which in Java is ASCII-only unless the
  * pattern carries UNICODE_CHARACTER_CLASS — so it stops at the first
  * diacritic. Exactly two of the twelve genitive month names carry one:
  * `wrze<s>nia` and `pa<z>dziernika`. The consequence is a bug that hides for
  * ten months and then takes the venue's whole programme out for September and
  * October: OKF Iluzja went white on 1 September 2026, Agrafka and Bułgarska
  * fell through to the Filmweb fallback, and Pod Baranami kept only the handful
  * of November/December special screenings whose month names happen to be plain
  * ASCII.
  *
  * Each case replays that venue's real repertoire page captured on 2026-09-04,
  * so the fixtures carry the diacritic month in the position that broke.
  */
class DiacriticMonthNameSpec extends AnyFlatSpec with Matchers {

  private val today = LocalDate.of(2026, 9, 4)

  private def showtimesOf(movies: Seq[models.CinemaMovie]): Seq[LocalDateTime] =
    movies.flatMap(_.showtimes.map(_.dateTime))

  "OkfIluzjaClient" should "read a 'września' day header" in {
    val client = new OkfIluzjaClient(new FakeHttpFetch("okf-iluzja-diacritic-month"), OkfIluzja, today)
    val movies = client.fetch()
    movies should not be empty
    showtimesOf(movies) should contain(LocalDateTime.of(2026, 9, 4, 16, 15))
  }

  "KinoAgrafkaClient" should "read a 'września' day header" in {
    val client = new KinoAgrafkaClient(new FakeHttpFetch("kino-agrafka-diacritic-month"), KinoAgrafka)
    val movies = client.fetch()
    movies should not be empty
    showtimesOf(movies) should contain(LocalDateTime.of(2026, 9, 4, 16, 0))
  }

  "KinoPodBaranamiClient" should "read a 'września' day header" in {
    val client = new KinoPodBaranamiClient(
      new FakeHttpFetch("kino-pod-baranami-diacritic-month"), KinoPodBaranami, today)
    val movies    = client.fetch()
    val showtimes = showtimesOf(movies)
    // The page's November/December headers ("4 listopada", "16 grudnia") always
    // parsed, so this venue never went empty — it served five far-future special
    // screenings and dropped the current programme. Assert on the September days
    // the ASCII spelling lost, not merely on a non-empty result.
    showtimes should contain(LocalDateTime.of(2026, 9, 4, 20, 5))
    showtimes.count(_.getMonthValue == 9) should be > 100
  }

  "KinoBulgarskaClient" should "read a 'września' day header" in {
    val client = new KinoBulgarskaClient(new FakeHttpFetch("kino-bulgarska-diacritic-month"), today)
    val movies = client.fetch()
    movies should not be empty
    showtimesOf(movies) should contain(LocalDateTime.of(2026, 9, 4, 14, 20))
  }
}
