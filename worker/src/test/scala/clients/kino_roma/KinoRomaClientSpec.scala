package clients.kino_roma

import clients.tools.FakeHttpFetch
import models.KinoRoma
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoRomaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar` page (07-06-2026 capture) through the
 *  client. Each card on this page represents exactly one screening; multiple
 *  screenings of the same film appear as multiple cards. `today` is pinned
 *  so the "DD.MM" → year inference is stable.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("kino-roma", real).get("https://www.kinoroma.zabrze.pl/repertuar")
 *  Fixture directory: test/resources/fixtures/kino-roma/
 *  Fetch URL:   https://www.kinoroma.zabrze.pl/repertuar */
class KinoRomaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-roma")
  private val client = new KinoRomaClient(http, KinoRoma, LocalDate.of(2026, 6, 7))

  "KinoRomaClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoRoma" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoRoma)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Tom i Jerry: Przygoda w muzeum on 2026-06-07 at 15:00" in {
    val movies = client.fetch()
    val film   = movies.find(_.movie.title == "Tom i Jerry: Przygoda w muzeum").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 15, 0))
  }
}
