package clients.kino_marzenie

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoMarzenie
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoMarzenieClient

import java.time.{LocalDate, LocalDateTime}

/** Replays three recorded `kinomarzenie.pl/embed/events?start_date=…` day
 *  partials (2026-09-23 through 2026-09-25) through the client. There is no
 *  whole-programme feed — the client sweeps one request per day in its
 *  window, so `windowDays = 3` here pins it to exactly the three recorded
 *  days rather than the production default of 14. */
class KinoMarzenieClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoMarzenieClient(
    new FakeHttpFetch("kino-marzenie"), KinoMarzenie,
    today = LocalDate.of(2026, 9, 23), windowDays = 3
  ).fetch()

  "KinoMarzenieClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMarzenie)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a film's showtimes across the swept days, read off the request date (not page text)" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 23, 16, 0),
      LocalDateTime.of(2026, 9, 24, 16, 0),
      LocalDateTime.of(2026, 9, 25, 19, 0)
    )
  }

  it should "carry the venue's MSI booking link off each showtime" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 23, 16, 0)).value
    slot.bookingUrl.value should include ("marzenieonline.tck.pl/MSI/Default.aspx?event_id=37013")
  }

  it should "read genres off the 'GENRE, GENRE | AGE+ LAT' metadata line, dropping the age tag" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    film.movie.genres shouldBe Seq("BIOGRAFICZNY", "DRAMAT")
  }

  it should "tag a subtitled showtime NAP off its sibling format div" in {
    val film = movies.find(_.movie.title == "OBCY").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 23, 20, 15)).value
    slot.format shouldBe List("NAP")
  }

  it should "tag a dubbed showtime DUB off its sibling format div" in {
    val film = movies.find(_.movie.title == "MARSUPILAMI").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 24, 14, 0)).value
    slot.format shouldBe List("DUB")
  }

  it should "drop a film listed with no showtime on a given day, keeping it on the day it screens" in {
    // MARSUPILAMI is listed (poster + title) on 2026-09-23 but its showtimes
    // column is empty that day — it only carries a real screening from 09-24.
    val film = movies.find(_.movie.title == "MARSUPILAMI").value
    film.showtimes.map(_.dateTime) should not contain LocalDateTime.of(2026, 9, 23, 0, 0)
    film.showtimes.map(_.dateTime.toLocalDate) should not contain LocalDate.of(2026, 9, 23)
  }

  it should "expose the film's own detail page as filmUrl" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    film.filmUrl.value shouldBe "https://www.kinomarzenie.pl/repertuar/1011,mistyczka"
  }

  it should "declare the venue's own host as its scrape host" in {
    new KinoMarzenieClient(new FakeHttpFetch("kino-marzenie"), KinoMarzenie).scrapeHosts shouldBe
      Set("www.kinomarzenie.pl")
  }
}
