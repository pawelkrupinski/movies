package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import org.scalatest.OptionValues
import tools.HttpStatusException
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoPatria
import services.cinemas.pl.KinoPatriaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded kinopatria.com/repertuar/ HTML through the client.
 *
 *  The page exposes two layout blocks:
 *    - `amy-movie-showtimews-daily-1`: "currently showing" section with absolute
 *      `DD-MM-YYYY` dates in each tab and per-movie times;
 *    - `amy-movie-showtimews-1`: upcoming/weekly grid with `DD.MM` dates
 *      (year inferred from `today`, pinned here to the capture date 2026-06-21).
 *
 *  Captured on 2026-06-21: 3 films, 28 showtimes across both sections. */
class KinoPatriaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  // Pin today to the fixture capture date so weekly-section year inference is stable.
  private val today  = LocalDate.of(2026, 6, 21)
  private val movies = new KinoPatriaClient(new FakeHttpFetch("kino-patria"), KinoPatria, today).fetch()

  "KinoPatriaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoPatria)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "tag every film with KinoPatria" in {
    all(movies.map(_.cinema)) shouldBe KinoPatria
  }

  it should "return non-empty titles without format tags" in {
    all(movies.map(_.movie.title)) should not be empty
    // Titles must not carry trailing format tokens
    all(movies.map(_.movie.title)) should not endWith "NAPISY"
    all(movies.map(_.movie.title)) should not endWith "DUB"
  }

  it should "include 'Backrooms. Bez wyjścia' with daily showtimes at 19:15 for Jun 21-27" in {
    val film = movies.find(_.movie.title == "Backrooms. Bez wyjścia").value
    film.cinema shouldBe KinoPatria
    // 7 dates × 1 time = 7 showtimes from the daily block
    film.showtimes should have size 7
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 19, 15))
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 27, 19, 15))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty
  }

  it should "include 'Toy Story 5' with two daily showtimes (15:00 and 17:00) for Jun 21-27" in {
    val film = movies.find(_.movie.title == "Toy Story 5").value
    // 7 dates × 2 times = 14 showtimes
    film.showtimes should have size 14
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 15, 0))
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 17, 0))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty
  }

  it should "include 'Ojczyzna' from the weekly grid with Jun 29 – Jul 5 showtimes at 19:15" in {
    val film = movies.find(_.movie.title == "Ojczyzna").value
    film.showtimes should have size 7
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 29, 19, 15))
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 7, 5, 19, 15))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty
  }

  it should "carry a filmUrl for each movie" in {
    all(movies.flatMap(_.filmUrl)) should startWith("https://kinopatria.com/")
  }

  // A 5xx/timeout from the venue's server must propagate so it surfaces red on
  // /uptime — not be swallowed into an empty list that reads as a successful
  // "0 showtimes" scrape (white, indistinguishable from a film-dormant venue).
  it should "propagate a fetch failure instead of swallowing it into an empty (white) scrape" in {
    val client = new KinoPatriaClient(new FailingHttpFetch(503), KinoPatria, today)
    a[HttpStatusException] should be thrownBy client.fetch()
  }
}
