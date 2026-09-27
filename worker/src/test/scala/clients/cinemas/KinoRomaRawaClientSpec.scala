package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoRomaRawa
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoRomaRawaClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded MDK Rawa Mazowiecka "Plan seansów" page (2026-09-27
 *  capture) — nine `li.tile--film` cards, one per film — and each card's
 *  `/wpisy/<slug>/` post, whose `div.single__schedule` table lists every
 *  screening date and time of that film.
 *
 *  Fixture directory: test/resources/fixtures/kino-roma-rawa/ */
class KinoRomaRawaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 27)
  private val client = new KinoRomaRawaClient(new FakeHttpFetch("kino-roma-rawa"), KinoRomaRawa, today)
  private lazy val movies = client.fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoRomaRawaClient" should "return one film per card of the plan, every one with showtimes" in {
    movies.map(_.movie.title) should contain allOf (
      "Lalka", "Nasza rewolucja", "Supermocni", "Baranek Shaun i kudłata bestia", "Tedi i magiczna lampa")
    movies.size shouldBe 9
    all(movies.map(_.showtimes)) should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoRomaRawa)
  }

  it should "read each film's full schedule table, placing the year-less dates in the right year" in {
    film("Lalka").showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 30, 16, 0), LocalDateTime.of(2026, 9, 30, 19, 0),
      LocalDateTime.of(2026, 10, 2, 18, 0), LocalDateTime.of(2026, 10, 2, 21, 0))
    film("Nasza rewolucja").showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 11, 13, 20, 0), LocalDateTime.of(2026, 11, 20, 9, 0), LocalDateTime.of(2026, 11, 20, 18, 0))
    // Tickets are sold at the box office only — there is no booking link to surface.
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl) shouldBe empty
  }

  it should "emit the schema.org identity signals: year, director, countries, runtime, genres, age" in {
    val lalka = film("Lalka")
    lalka.movie.releaseYear    shouldBe Some(2026)
    lalka.director             shouldBe Seq("Maciej Kawalski")
    lalka.movie.countries      shouldBe Seq("Polska")
    lalka.movie.runtimeMinutes shouldBe Some(160)
    lalka.movie.genres         shouldBe Seq("dramat", "romans")
    lalka.ageRating            shouldBe Some("13+")
    lalka.filmUrl.value        shouldBe "https://mdkrawa.pl/wpisy/lalka/"
    lalka.posterUrl.value      should startWith("https://mdkrawa.pl/wp-content/uploads/")
    // A card with no director keeps the other signals.
    val shaun = film("Baranek Shaun i kudłata bestia")
    shaun.director                shouldBe empty
    shaun.movie.releaseYear       shouldBe Some(2025)
    shaun.movie.countries         shouldBe Seq("Wielka Brytania")
  }

  it should "keep the plot as the synopsis, not the box-office notices bolded under it" in {
    val synopsis = film("Lalka").synopsis.value
    synopsis should include("Wokulski")
    synopsis should not include "Cennik"
    synopsis should not include "przedsprzedaż"
    film("Nasza rewolucja").trailerUrl.value shouldBe "https://www.youtube.com/watch?v=7zKcevg6Wm8"
  }

  it should "propagate a fetch failure instead of reporting an empty scrape" in {
    a[HttpStatusException] should be thrownBy
      new KinoRomaRawaClient(new FailingHttpFetch(503), KinoRomaRawa, today).fetch()
  }
}
