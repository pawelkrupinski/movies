package clients.kino_zorza

import models.KinoZorza
import org.scalatest.OptionValues
import clients.tools.{FakeHttpFetch, FilmPages}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoZorzaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar` page (07-06-2026 capture) through the
 *  client. The page lists all days on a single server-rendered HTML response —
 *  no per-day pagination, no JS needed. The `today` parameter is pinned so date
 *  inference is stable regardless of when the test runs. */
class KinoZorzaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-zorza")
  private val client = new KinoZorzaClient(http, KinoZorza, LocalDate.of(2026, 6, 7))
  // Every case only reads the parsed result, so the fixture is replayed once per suite.
  private lazy val fetched = client.fetch()

  "KinoZorzaClient" should "return a non-empty film list" in {
    val movies = fetched
    movies should not be empty
  }

  it should "tag every film with KinoZorza" in {
    val movies = fetched
    movies.map(_.cinema).toSet shouldBe Set(KinoZorza)
  }

  it should "give every film at least one showtime" in {
    val movies = fetched
    all(movies.map(_.showtimes)) should not be empty
  }

  // The identity resolver reads these: without the director "KLAPS! - Rozważna i romantyczna" read as Ang Lee's 1995 film.
  // Recorded 2026-10-05 from https://www.kinozorza.pl/film/klaps---rozwazna-i-romantyczna-napisy-2d and /film/lalka-polski-2d
  it should "read each film page's director, countries, running time and premiere year" in {
    val sense = FilmPages.detailOf(client, "https://www.kinozorza.pl/film/klaps---rozwazna-i-romantyczna-napisy-2d")
    sense.director shouldBe Seq("Georgia Oakley")
    sense.countries shouldBe Seq("USA")
    sense.runtimeMinutes shouldBe None
    sense.releaseYear shouldBe None
    val lalka = FilmPages.detailOf(client, "https://www.kinozorza.pl/film/lalka-polski-2d")
    lalka.director shouldBe Seq("Maciej Kawalski")
    lalka.countries shouldBe Seq("Polska")
    lalka.runtimeMinutes shouldBe Some(162)
    lalka.releaseYear shouldBe Some(2026)
  }

  it should "pin a concrete screening: Ojczyzna on 2026-06-07 at 12:30" in {
    // On the 07.06 fixture, Ojczyzna screens at 12:30 and 16:15.
    val movies = fetched
    val ojczyzna = movies.find(_.movie.title == "Ojczyzna").value
    ojczyzna.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 12, 30))
  }
}
