package clients.okf_iluzja

import clients.tools.FakeHttpFetch
import models.OkfIluzja
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.OkfIluzjaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded weekly repertoire page (07-06-2026 capture) through
 *  the client.  The page lists all seven days of the current week in a single
 *  server-rendered HTML response — no per-day pagination, no JavaScript.
 *  `today` is pinned so the fixture is found regardless of when the test runs.
 */
class OkfIluzjaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("okf-iluzja")
  private val client = new OkfIluzjaClient(http, OkfIluzja, LocalDate.of(2026, 6, 7))

  "OkfIluzjaClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with OkfIluzja" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(OkfIluzja)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Erupcja on 2026-06-07 at 15:30" in {
    // On the 07.06 fixture, Erupcja screens at 15:30 and also on subsequent days.
    val movies  = client.fetch()
    val erupcja = movies.find(_.movie.title == "Erupcja").value
    erupcja.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 15, 30))
  }

  it should "aggregate the same film across multiple days into one CinemaMovie" in {
    val movies  = client.fetch()
    // Erupcja appears on multiple days in the weekly fixture; it should collapse
    // into a single CinemaMovie entry with all its showtimes.
    val erupcja = movies.filter(_.movie.title == "Erupcja")
    erupcja should have size 1
    erupcja.head.showtimes.size should be >= 2
  }

  it should "strip inner programme-label spans from h3 titles" in {
    val movies = client.fetch()
    // 'Podziemny krąg' has a POWRÓT NA EKRAN <span> inside its h3;
    // the title must be just the film name, no span text.
    val podziemny = movies.find(_.movie.title == "Podziemny krąg")
    podziemny should not be empty
  }

  it should "have no booking URLs (site has only a generic ticket page)" in {
    val movies = client.fetch()
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl) shouldBe empty
  }
}
