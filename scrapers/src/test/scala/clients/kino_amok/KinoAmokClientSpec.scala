package clients.kino_amok

import models.KinoAmok
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoAmokClient

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
  // Every case only reads the parsed result, so the fixture is replayed once per suite.
  private lazy val fetched = client.fetch()

  "KinoAmokClient" should "drop the venue's own live evenings, keeping its films" in {
    // The recorded page with one film retitled as Kino Amok listed a concert evening on 2026-10-02.
    val evening = "Siesta – Trylogia Afrykańska | Spotkanie autorskie z Marcinem Kydryńskim i koncert muzyki na żywo"
    val page    = new FakeHttpFetch("kino-amok").get("https://amok.gliwice.pl/repertuar/").replace("Diabeł ubiera się u Prady 2", evening)
    val titles  = new KinoAmokClient(tools.RoutingHttpFetch.getOnly(Seq("amok.gliwice.pl" -> page)), KinoAmok, LocalDate.of(2026, 6, 7))
      .fetch().map(_.movie.title)
    titles should not be empty
    titles should not contain (evening)
  }

  "KinoAmokClient" should "return a non-empty film list" in {
    val movies = fetched
    movies should not be empty
  }

  it should "tag every film with KinoAmok" in {
    val movies = fetched
    movies.map(_.cinema).toSet shouldBe Set(KinoAmok)
  }

  it should "give every film at least one showtime" in {
    val movies = fetched
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Diabeł ubiera się u Prady 2 on 2026-06-07 at 15:30 in Duża sala" in {
    val movies = fetched
    val diabel = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    val slot   = diabel.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 15, 30)).value
    slot.room shouldBe Some("Duża sala")
    slot.bookingUrl.value should include("kup-bilet")
  }
}
