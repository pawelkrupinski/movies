package clients.filmweb_showtimes

import models.{KinoGrojeckiOsrodekKultury, MultikinoTargowek}
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.FilmwebShowtimesClient

import java.time.{LocalDate, LocalDateTime}

/** Replays 2026-10-30 captures of Filmweb's UNDATED `/cinema/<id>/seances`, which
 *  answers a venue's whole published programme in one request, each seance carrying
 *  its own `date`. The client used to walk seven dated pages, a horizon cap: Grójec
 *  (1683) read white over a slot on day 8, and a venue on the Filmweb fallback lost
 *  everything its own scraper had past a week. Measured live the same day, the
 *  undated list equals the per-day lists summed (Multikino Targówek: 231 seances,
 *  the same day by day through 2027-01-10). */
class FilmwebHorizonSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http  = new FakeHttpFetch("filmweb-horizon")

  "FilmwebShowtimesClient" should "read every published screening, not only the next seven days" in {
    val movies = new FilmwebShowtimesClient(http, 1378, MultikinoTargowek).fetch()
    val showtimes = movies.flatMap(_.showtimes)
    showtimes should have size 523
    showtimes.map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2026, 12, 20)
    showtimes.find(_.dateTime == LocalDateTime.of(2026, 12, 20, 11, 30)).value.bookingUrl.value shouldBe
      "https://www.multikino.pl/rezerwacja-biletow/podsumowanie/0024/HO00002550/119051?SC_CMP=FILMWEB"
  }

  it should "list a venue whose only screening is more than a week out" in {
    val movies = new FilmwebShowtimesClient(http, 1683, KinoGrojeckiOsrodekKultury).fetch()
    movies.flatMap(_.showtimes).map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 11, 7, 16, 0))
  }
}
