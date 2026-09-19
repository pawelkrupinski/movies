package clients.csw_torun

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoCentrumCsw
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoCentrumCswClient

import java.time.LocalDateTime

/** Replays a 2026-09-19 capture of `csw/v1/cinema-repertoire` — the JSON
 *  endpoint the site's Nuxt-rebuilt cinema page now fetches its schedule
 *  from, after the old static `/repertuar/` HTML page (dpProEventCalendar
 *  `div.box` markup) was replaced by a client-rendered widget.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("csw-torun", real).get("https://api.csw.torun.pl/wp-json/csw/v1/cinema-repertoire")
 *  Fixture directory: test/resources/fixtures/csw-torun/
 *  Fetch URL:   https://api.csw.torun.pl/wp-json/csw/v1/cinema-repertoire */
class KinoCentrumCswClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("csw-torun")
  private val client = new KinoCentrumCswClient(http, KinoCentrumCsw)

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

  it should "pin a concrete screening: Obcy on 2026-09-22 at 15:30" in {
    val movies = client.fetch()
    val obcy = movies.find(_.movie.title == "Obcy").value
    obcy.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 22, 15, 30))
    obcy.filmUrl.value shouldBe "https://csw.torun.pl/kino/obcy/"
  }

  it should "merge a film's screenings across different days into one entry" in {
    val movies = client.fetch()
    val obcy = movies.find(_.movie.title == "Obcy").value
    obcy.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 22, 15, 30),
      LocalDateTime.of(2026, 9, 24, 20, 0)
    )
  }
}
