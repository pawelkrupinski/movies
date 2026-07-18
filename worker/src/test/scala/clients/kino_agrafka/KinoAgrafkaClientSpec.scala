package clients.kino_agrafka

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoAgrafka
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoAgrafkaClient

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

  it should "parse the single director and production year from the title cell" in {
    val film = client.fetch().find(_.movie.title == "MILCZĄCA PRZYJACIÓŁKA").value
    film.director shouldBe Seq("Ildiko Enyedi")
    film.movie.releaseYear shouldBe Some(2026)
  }

  it should "parse multiple comma-separated directors, stopping before the country/year block" in {
    val film = client.fetch().find(_.movie.title == "ZNAKI PANA ŚLIWKI").value
    film.director shouldBe Seq("Urszula Morga", "Bartosz Mikołajczyk")
    film.movie.releaseYear shouldBe Some(2025)
  }

  // A country whose name carries no digit ("Polska", "Wlk. Brytania") sits in
  // its own comma segment, so the director cut must drop it rather than read it
  // as a second director. Guard the whole corpus, not one film.
  it should "never capture a country name as a director" in {
    client.fetch().flatMap(_.director).filter(services.cinemas.CountryNames.isPolish) shouldBe empty
  }
}
