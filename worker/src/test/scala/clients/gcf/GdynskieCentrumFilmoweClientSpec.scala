package clients.gcf

import clients.tools.FakeHttpFetch
import models.GdynskieCentrumFilmowe
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.GdynskieCentrumFilmoweClient

import java.time.LocalDateTime

/** Replays the recorded GCF repertoire page (07-06-2026 capture) through the
 *  client. The page is a server-rendered WordPress template at
 *  `https://gcf.org.pl/kino-studyjne/repertuar/`. Each `div.film-width` block
 *  holds one film; its `a.film-hours` anchors carry `data-date` + `data-hour`
 *  + booking URL — no per-film detail fetches needed. The fixture is at:
 *
 *    test/resources/fixtures/gcf/gcf.org.pl/kino-studyjne/repertuar/.html
 *
 *  Pinned concrete screenings seen live on 07-06-2026:
 *    "Diabeł ubiera się u Prady 2" on 2026-06-07 at 20:15
 */
class GdynskieCentrumFilmoweClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("gcf")
  private val client = new GdynskieCentrumFilmoweClient(http, GdynskieCentrumFilmowe)

  "GdynskieCentrumFilmoweClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with GdynskieCentrumFilmowe" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(GdynskieCentrumFilmowe)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Diabeł ubiera się u Prady 2 on 2026-06-07 at 20:15" in {
    val movies = client.fetch()
    val diabel = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    diabel.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 20, 15))
  }

  it should "include booking URLs pointing to bilet.gcf.org.pl" in {
    val movies = client.fetch()
    val bookings = movies.flatMap(_.showtimes).flatMap(_.bookingUrl)
    bookings.head should startWith("https://bilet.gcf.org.pl/MSI/")
  }
}
