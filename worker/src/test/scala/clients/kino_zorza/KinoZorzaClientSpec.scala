package clients.kino_zorza

import clients.tools.FakeHttpFetch
import models.KinoZorza
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoZorzaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar` page (07-06-2026 capture) through the
 *  client. The page lists all days on a single server-rendered HTML response —
 *  no per-day pagination, no JS needed. The `today` parameter is pinned so date
 *  inference is stable regardless of when the test runs. */
class KinoZorzaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-zorza")
  private val client = new KinoZorzaClient(http, KinoZorza, LocalDate.of(2026, 6, 7))

  "KinoZorzaClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoZorza" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoZorza)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Ojczyzna on 2026-06-07 at 12:30" in {
    // On the 07.06 fixture, Ojczyzna screens at 12:30 and 16:15.
    val movies = client.fetch()
    val ojczyzna = movies.find(_.movie.title == "Ojczyzna").value
    ojczyzna.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 12, 30))
  }
}
