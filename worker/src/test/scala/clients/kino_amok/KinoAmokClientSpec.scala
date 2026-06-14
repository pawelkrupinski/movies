package clients.kino_amok

import clients.tools.FakeHttpFetch
import models.KinoAmok
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoAmokClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar/` page (07-06-2026 capture) through the
 *  client. The page lists 14 date sections (~6 screenings each) in one
 *  static response. `today` is pinned so the "7 czerwca" → year inference
 *  stays stable regardless of when the test runs.
 *
 *  Fixture recorder:
 *    new RecordingHttpFetch("kino-amok", real).get("https://amok.gliwice.pl/repertuar/")
 *  Fixture directory: test/resources/fixtures/kino-amok/
 *  Fetch URL:   https://amok.gliwice.pl/repertuar/ */
class KinoAmokClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-amok")
  private val client = new KinoAmokClient(http, KinoAmok, LocalDate.of(2026, 6, 7))

  "KinoAmokClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoAmok" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoAmok)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Diabeł ubiera się u Prady 2 on 2026-06-07 at 15:30 in Duża sala" in {
    val movies = client.fetch()
    val diabel = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    val slot   = diabel.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 15, 30)).value
    slot.room shouldBe Some("Duża sala")
    slot.bookingUrl.value should include("kup-bilet")
  }
}
