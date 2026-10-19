package clients.kino_jok

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoJOK
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoJOKClient

import java.time.LocalDateTime

/** Replays the recorded `wp-json/tribe/events/v1/events?categories=kino`
 *  response for Janowski Ośrodek Kultury (Janów Lubelski) through the
 *  client. Each JSON event is a multi-day run at one time of day
 *  (`start_date`/`end_date` span the run, not one row per showing) — the
 *  client expands that into a showtime per day via `ScraperParse.dailyRange`. */
class KinoJOKClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDateTime.of(2026, 9, 23, 0, 0).toLocalDate
  private val movies = new KinoJOKClient(new FakeHttpFetch("kino-jok"), today = today).fetch()

  "KinoJOKClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoJOK)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "expand a multi-day run into one showtime per day, at the run's time of day" in {
    val film = movies.find(_.movie.title == "Minionki i straszydła").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 10, 2, 16, 0), LocalDateTime.of(2026, 10, 3, 16, 0), LocalDateTime.of(2026, 10, 4, 16, 0)
    )
  }

  it should "keep a second concurrent film at its own time, distinct from the first" in {
    val film = movies.find(_.movie.title == "Totalna magia 2").value
    film.showtimes.map(_.dateTime) should contain (LocalDateTime.of(2026, 10, 2, 18, 0))
  }

  it should "carry the event's own page as the film URL, with no booking link (door sales only)" in {
    val film = movies.find(_.movie.title == "Minionki i straszydła").value
    film.filmUrl.value shouldBe "https://jokjanow.pl/wydarzenia/minionki-i-straszydla/"
    all(film.showtimes.map(_.bookingUrl)) shouldBe None
  }

  it should "merge two separate runs of the same title into one film" in {
    val film = movies.find(_.movie.title == "Lalka").value
    film.showtimes.map(_.dateTime.toLocalDate.toString) should contain allOf ("2026-10-09", "2026-10-17")
  }
}
