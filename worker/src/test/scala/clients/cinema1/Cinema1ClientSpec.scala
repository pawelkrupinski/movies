package clients.cinema1

import clients.tools.FakeHttpFetch
import models.Cinema1Gdansk
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.Cinema1Client

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `bilety.cinemaone.pl/MSI/mvc/pl?sort=Name&date=2026-06`
 *  page through the client.  `today` is pinned to 2026-06-07 so the client
 *  fetches the June-2026 fixture and attempts July-2026 (which will fail with a
 *  FileNotFoundException and be swallowed — the client treats a missing month as
 *  an empty result rather than a fatal error). */
class Cinema1ClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("cinema1")
  // TODO: replace `Cinema1Gdansk` with the real `Cinema1Gdansk` case object once it
  // has been added to Cinema.scala and City.scala.
  private val client = new Cinema1Client(http, Cinema1Gdansk, today = LocalDate.of(2026, 6, 7))

  "Cinema1Client" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with the supplied cinema" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(Cinema1Gdansk)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "strip format suffixes and apply sentence-case to titles" in {
    val movies = client.fetch()
    // Raw title in fixture: "BACKROOMS. BEZ WYJŚCIA (2D NAPISY)"
    // After cleaning: "Backrooms. Bez wyjścia" (sentence-cased, no format tag)
    val found = movies.find(_.movie.title.startsWith("Backrooms"))
    found shouldBe defined
    found.value.movie.title should not include "(2D"
    found.value.movie.title should not include "(3D"
  }

  it should "pin a concrete screening: 90. Urodziny pavarottiego on 2026-06-09 at 19:00" in {
    // Raw title: "90. URODZINY PAVAROTTIEGO (2D NAPISY)" → "90. Urodziny pavarottiego"
    // Event text in fixture: "09 cze 19:00"
    val movies = client.fetch()
    val film = movies.find(_.movie.title.startsWith("90.")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 9, 19, 0))
  }
}
