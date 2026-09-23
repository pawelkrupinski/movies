package clients.kino_mdk_myszkow

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoMDKMyszkow
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoMDKMyszkowClient

import java.time.LocalDateTime

/** Replays the recorded `wydarzenia/kategorie/kino/` category listing plus
 *  its one event's own detail page (captured 2026-09-23) through the
 *  client. The category page's Events Manager "Data" field only gives a
 *  date RANGE; the real per-day showtimes are free text on the detail page
 *  ("25, 26, 27.09.2026" + "godz. 18:00"), which is what this asserts. */
class KinoMDKMyszkowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoMDKMyszkowClient(new FakeHttpFetch("kino-mdk-myszkow")).fetch()

  "KinoMDKMyszkowClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMDKMyszkow)
  }

  it should "expand the free-text comma day-list into one showtime per day" in {
    val film = movies.find(_.movie.title == "Dzień dziecka księdza Jana Kaczkowskiego").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 25, 18, 0), LocalDateTime.of(2026, 9, 26, 18, 0), LocalDateTime.of(2026, 9, 27, 18, 0)
    )
  }

  it should "read the runtime off the 'czas trwania' line and pick the synopsis paragraph" in {
    val film = movies.find(_.movie.title == "Dzień dziecka księdza Jana Kaczkowskiego").value
    film.movie.runtimeMinutes.value shouldBe 76
    film.synopsis.value should include("Jan Kaczkowski")
    film.synopsis.value.toLowerCase should not include "czas trwania"
  }

  it should "carry the event's own page as the film URL and its poster, with no booking link (door sales only)" in {
    val film = movies.find(_.movie.title == "Dzień dziecka księdza Jana Kaczkowskiego").value
    film.filmUrl.value shouldBe "https://www.mdk-myszkow.pl/wydarzenia/dzien-dziecka-ksiedza-jana-kaczkowskiego/"
    film.posterUrl.value should include("plakat-kino.png")
    all(film.showtimes.map(_.bookingUrl)) shouldBe None
  }
}
