package clients.kino_paradox

import clients.tools.FakeHttpFetch
import models.KinoParadox
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoParadoxClient

import java.time.LocalDateTime

/** Replays the recorded `/repertuar/` page (07-06-2026 capture) through the
 *  client. Each screening is a `div.list-item__content__row[data-date]`; the
 *  fixture contains all days rendered server-side in one response. */
class KinoParadoxClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-paradox")
  private val client = new KinoParadoxClient(http, KinoParadox)

  "KinoParadoxClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoParadox" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoParadox)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Zawieście czerwone latarnie on 2026-06-07 at 18:00" in {
    val movies = client.fetch()
    val zawies = movies.find(_.movie.title == "Zawieście czerwone latarnie").value
    zawies.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 18, 0))
    zawies.showtimes.flatMap(_.bookingUrl).head should include("bilety.kinoparadox.pl")
  }
}
