package clients.csw_torun

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoCentrumCsw
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoCentrumCswClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar/` page (07-06-2026 capture) through the
 *  client. The dpProEventCalendar plugin renders all upcoming dates as static
 *  HTML `div.box` elements — no JS needed. `today` is pinned so the
 *  "7 czerwca" → year inference stays stable.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("csw-torun", real).get("https://csw.torun.pl/repertuar/")
 *  Fixture directory: test/resources/fixtures/csw-torun/
 *  Fetch URL:   https://csw.torun.pl/repertuar/ */
class KinoCentrumCswClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("csw-torun")
  private val client = new KinoCentrumCswClient(http, KinoCentrumCsw, LocalDate.of(2026, 6, 7))

  "KinoCentrumCswClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoCentrumCsw" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoCentrumCsw)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Drzewo magii on 2026-06-07 at 12:30" in {
    val movies = client.fetch()
    val drzewo = movies.find(_.movie.title == "Drzewo magii").value
    drzewo.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 12, 30))
    drzewo.filmUrl.value should include("csw.torun.pl/pec-events/")
  }
}
