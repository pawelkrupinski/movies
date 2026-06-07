package clients.ada_kino_studyjne

import clients.tools.FakeHttpFetch
import models.AdaKinoStudyjne
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.AdaKinoStudyjneClient

import java.time.LocalDateTime

/** Replays the recorded biletyna.pl place page (07-06-2026 capture) for ADA
 *  Kino Studyjne through the client. The page embeds a single JSON-LD block
 *  of type `Place` whose `events` array lists every upcoming ScreeningEvent —
 *  no per-film detail fetches needed. The fixture is at:
 *
 *    test/resources/fixtures/ada-kino-studyjne/biletyna.pl/Warszawa/ADA-Kino-Studyjne.html
 *
 *  Pinned concrete screenings seen live on 07-06-2026:
 *    "Posłani" on 2026-06-07 at 15:00 and 18:00
 */
class AdaKinoStudyjneClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("ada-kino-studyjne")
  private val client = new AdaKinoStudyjneClient(http, AdaKinoStudyjne)

  "AdaKinoStudyjneClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with AdaKinoStudyjne" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(AdaKinoStudyjne)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Posłani on 2026-06-07 at 15:00" in {
    val movies  = client.fetch()
    val poslani = movies.find(_.movie.title == "Posłani").value
    poslani.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 15, 0))
  }

  it should "include a booking URL for each showtime" in {
    val movies = client.fetch()
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).head should startWith("https://biletyna.pl/film/")
  }
}
