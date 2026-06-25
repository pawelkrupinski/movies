package clients.kino_spojnia

import clients.tools.FakeHttpFetch
import models.KinoSpojnia
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoSpojniaClient

import java.time.LocalDateTime

/** Replays the recorded `kinospojnia.pl/repertuar.php` listing through the
 *  client. The page is a bespoke server-rendered PHP table layout; each
 *  screening's absolute date comes off its `a.kupbilet` buy-link query.
 *
 *  Kino Spójnia was previously scraped from Filmweb, whose API had silently
 *  gone empty for it (every poll returned `[]`) though the cinema is open —
 *  this fixture is the proof its programme is real and reachable on its own
 *  site. */
class KinoSpojniaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoSpojniaClient(new FakeHttpFetch("kino-spojnia")).fetch()

  "KinoSpojniaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSpojnia)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening with its date, time and booking link" in {
    // Fixture: "Gwiezdne wojny: Mandalorian i Grogu" opens 2026-06-12 at 17:30,
    // booking event_id=87294 on the Charlie/MSI host; release year 2026.
    val film = movies.find(_.movie.title == "Gwiezdne wojny: Mandalorian i Grogu").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 12, 17, 30)).value
    slot.bookingUrl.value should include("event_id=87294")
    film.movie.releaseYear shouldBe Some(2026)
  }

  it should "read runtime, countries and genres off the gray metadata line" in {
    // Gray line: "135', USA, 2026, Sci-fi" — runtime before, country before the
    // year, genre after it.
    val film = movies.find(_.movie.title == "Gwiezdne wojny: Mandalorian i Grogu").value
    film.movie.runtimeMinutes shouldBe Some(135)
    film.movie.countries shouldBe Seq("USA")
    film.movie.genres shouldBe Seq("Sci-fi")
  }

  it should "keep multiple countries (before the year) apart from genres (after it)" in {
    // "104', USA, Chiny, 2026, Animacja, Komedia, Przygodowy" — two countries
    // ahead of the year, three genres behind it.
    val multi = movies.find(_.movie.countries == Seq("USA", "Chiny")).value
    multi.movie.runtimeMinutes shouldBe Some(104)
    multi.movie.genres shouldBe Seq("Animacja", "Komedia", "Przygodowy")
    multi.movie.releaseYear shouldBe Some(2026)
  }

  it should "read the absolute date off the buy link, not infer the year" in {
    // Every showtime dates into the captured June–July 2026 window.
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }
}
