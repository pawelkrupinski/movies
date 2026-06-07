package clients.kino_agrafka

import clients.tools.FakeHttpFetch
import models.KinoAgrafka
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoAgrafkaClient

import java.time.LocalDateTime

/** Replays the recorded `/rep.php` page (07-06-2026 capture) through the client.
 *  The schedule spans several weeks in one HTML response; the fixture includes
 *  screenings starting from 7 czerwca 2026. */
class KinoAgrafkaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-agrafka")
  private val client = new KinoAgrafkaClient(http, KinoAgrafka)

  "KinoAgrafkaClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoAgrafka" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoAgrafka)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Milcząca przyjaciółka on 2026-06-07 at 13:30" in {
    val movies = client.fetch()
    val film   = movies.find(_.movie.title == "MILCZĄCA PRZYJACIÓŁKA").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 13, 30))
  }
}
