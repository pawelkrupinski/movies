package clients.kino_zak

import models.{CinemaMovie, KinoZak}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoZakClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded Klub Żak calendar (`/pl/kalendarz?category=kino`) and
 *  the multi-day events' detail pages through the real parser. The calendar
 *  carries no year, so the client is pinned to the capture date (2026-06-06)
 *  to make the inferred years deterministic. */
class KinoZakClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val captureDate = LocalDate.of(2026, 6, 6)
  private val movies: Seq[CinemaMovie] =
    new KinoZakClient(new clients.tools.FakeHttpFetch("kino-zak"), KinoZak, captureDate).fetch()

  "KinoZakClient" should "return films, all tagged KinoZak with plausible showtimes" in {
    movies.size should be >= 1
    all(movies.map(_.cinema)) shouldBe KinoZak
    movies.foreach { m =>
      m.showtimes should not be empty
      m.showtimes.foreach { s =>
        s.dateTime.getYear should (be >= 2026 and be <= 2027)
        s.dateTime.getHour should (be >= 8 and be <= 23)
        s.bookingUrl.value should startWith("https://klubzak.com.pl/pl/kalendarz/")
      }
      // Showtimes are sorted and de-duplicated.
      m.showtimes.map(_.dateTime) shouldBe m.showtimes.map(_.dateTime).distinct.sorted
    }
  }

  it should "read a single-screening time straight off the listing card" in {
    val miAmor = movies.find(_.movie.title == "Mi Amor | Przegląd Nowego Kina Francuskiego").value
    miAmor.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 18, 20, 15))
  }

  it should "expand a multi-day run with mixed times from the detail Seans block" in {
    // "Wolność po włosku" runs 5–7 czerwca o 18:00 and 8–9 czerwca o 20:00 —
    // five screenings across two time segments, none of which the listing card
    // (a bare "Od 05 Cze Do 09 Cze") could supply.
    val wolnosc = movies.find(_.movie.title == "Wolność po włosku").value
    val times = wolnosc.showtimes.map(_.dateTime)
    times should contain(LocalDateTime.of(2026, 6, 5, 18, 0))
    times should contain(LocalDateTime.of(2026, 6, 7, 18, 0))
    times should contain(LocalDateTime.of(2026, 6, 8, 20, 0))
    times should contain(LocalDateTime.of(2026, 6, 9, 20, 0))
    times should have size 5
  }
}
