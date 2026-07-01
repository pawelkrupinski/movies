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

  it should "fetch per-film detail (synopsis, director, genre, runtime, country, poster, language) from the /naekranie page" in {
    // Deferred detail: the EnrichDetails task calls fetchFilmDetail with the
    // slot's /naekranie/<slug> filmUrl. Replays the recorded detail page.
    val d = client.fetchFilmDetail("https://kinoparadox.pl/naekranie/chlopiec-na-krancach-swiata").value
    d.director        should contain("Grzegorz Wacławek")
    d.genres          should contain("animacja")
    d.runtimeMinutes  shouldBe Some(88)
    d.releaseYear     shouldBe Some(2026)
    d.countries       should contain allOf ("Polska", "Kanada", "Hiszpania")
    d.synopsis.value  should include("Omul")
    d.posterUrl.value should include("uploads/event")
    // "Wersja językowa: polski lektor" → a per-film LEK badge for the showings.
    d.format          shouldBe List("LEK")
  }

  it should "expose itself as a deferred DetailEnricher resolving TMDB from the listing" in {
    client                       shouldBe a[services.cinemas.DetailEnricher]
    client.detailGroup           shouldBe "kino-paradox"
    client.defersTmdbResolution  shouldBe false
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

  // `div.item-director` is "reż. <dirs> / <countries> / <year> / <runtime>’".
  it should "parse the director and production year from item-director" in {
    val zawies = client.fetch().find(_.movie.title == "Zawieście czerwone latarnie").value
    zawies.director shouldBe Seq("Yimou Zhang")
    zawies.movie.releaseYear shouldBe Some(1991)
  }

  // The same slash line carries countries + runtime after the director/year —
  // "reż. Yimou Zhang / Chiny, Hong Kong / 1991 / 125’".
  it should "parse the production countries and runtime from item-director" in {
    val zawies = client.fetch().find(_.movie.title == "Zawieście czerwone latarnie").value
    zawies.movie.countries shouldBe Seq("Chiny", "Hong Kong")
    zawies.movie.runtimeMinutes shouldBe Some(125)
  }

  it should "parse multiple directors and skip the country / runtime slashes" in {
    val bum = client.fetch().find(_.movie.title == "Bum!").value
    bum.director shouldBe Seq("Marta Selecka", "Andra Doršs")
    bum.movie.releaseYear shouldBe Some(2024)
  }

  it should "never read a country name as a director" in {
    client.fetch().flatMap(_.director).filter(services.cinemas.CountryNames.isPolish) shouldBe empty
  }
}
