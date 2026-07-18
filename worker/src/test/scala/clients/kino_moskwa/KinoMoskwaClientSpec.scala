package clients.kino_moskwa

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoMoskwa
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoMoskwaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `kinomoskwa.pl/kalendarz/?yr=2026&month=06&dy=7&…`
 *  page (07-06-2026 capture) through the client. Only the base-day fixture is
 *  on disk; future-day fetches throw in `FakeHttpFetch` and are tolerated —
 *  they contribute no screenings. `today` is pinned so mini-calendar date
 *  inference is stable regardless of when the test runs. */
class KinoMoskwaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-moskwa")
  private val client = new KinoMoskwaClient(http, KinoMoskwa, LocalDate.of(2026, 6, 7))

  "KinoMoskwaClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoMoskwa" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoMoskwa)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: POSŁANI on 2026-06-07 at 14:00 in Sala Studyjna" in {
    // Fixture shortdesc: <a …>14:00/S/</a> and <a …>17:45/S/</a>
    val movies  = client.fetch()
    val poslani = movies.find(_.movie.title == "POSŁANI").value
    val st      = poslani.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 14, 0)).value
    st.room shouldBe Some("Sala Studyjna")
  }

  it should "strip the leading ordinal prefix from event titles" in {
    // My Calendar numbers events: "2. POSŁANI" → "POSŁANI"
    val movies = client.fetch()
    movies.map(_.movie.title) should not contain ("2. POSŁANI")
    movies.map(_.movie.title) should contain("POSŁANI")
  }

  it should "drop non-public group screenings (Pokazy grupowe)" in {
    // '1. Pokazy grupowe' has only a time range '8:00-14:30', no /S/ or /D/ links.
    val movies = client.fetch()
    movies.map(_.movie.title) should not contain ("Pokazy grupowe")
  }
}
