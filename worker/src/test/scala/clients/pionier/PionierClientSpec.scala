package clients.pionier

import clients.tools.FakeHttpFetch
import models.KinoPionier
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.PionierClient

import java.time.LocalDateTime

/** Replays Kino Pionier 1907's recorded `/repertuar/` page (06-06-2026 capture,
 *  the whole programme on one page — no pagination) through the client. The
 *  page is a flat `div.repertuar-list` where `h2.naglowek[id=dzien_YYYY-MM-DD]`
 *  day headings and `div.event_box` screenings are SIBLINGS, so the parser
 *  carries the heading's date forward onto the boxes that follow it. The pinned
 *  06-07 screening verifies that carry-forward anchor, and the room + biletomat
 *  booking URL prove the per-box fields land. */
class PionierClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("szczecin")

  "PionierClient" should "parse the whole-programme repertoire list" in {
    val movies = new PionierClient(http, KinoPionier).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoPionier)

    // The capture holds 48 distinct films / 104 screenings spanning
    // 07-06-2026 → 15-08-2026; every film carries at least one slot. (The two
    // event_boxes with no biletomat link still count as bookless screenings.)
    movies should have size 48
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes) should have size 104
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "resolve each screening's date from the carried-forward day heading" in {
    val movies = new PionierClient(http, KinoPionier).fetch()

    // The day's first box ("OSTATNIA SESJA W PARYŻU", 13:00) inherits the
    // 2026-06-07 heading that precedes it as a sibling node.
    val first = movies.find(_.movie.title == "OSTATNIA SESJA W PARYŻU").value
    val slot  = first.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 13, 0)).value
    slot.room.value shouldBe "Sala Czerwona"
    slot.bookingUrl.value should startWith("https://biletomat.pl/embedded/rezerwacja/")
  }

  it should "expose each film's /event/ page as filmUrl for deferred detail" in {
    val movies = new PionierClient(http, KinoPionier).fetch()
    movies.flatMap(_.filmUrl) should not be empty
    all(movies.flatMap(_.filmUrl).map(_.startsWith("https://pionier1907.pl/event/"))) shouldBe true
  }

  // ── Per-film detail page (deferred enrichment) ─────────────────────────────

  private val client = new PionierClient(http, KinoPionier)

  it should "harvest the production year off the event page so a yearless arthouse title can resolve" in {
    // "Federico Fellini: Słodkie życie" — the bare title "Słodkie życie" collides
    // with a 2024 TMDB film, so without the year the row is tmdbNoMatch. The event
    // page carries `Rok: 1960`, which breaks the collision. This is the fix.
    val detail = client.fetchFilmDetail("https://pionier1907.pl/event/federico-fellini-slodkie-zycie").value
    detail.releaseYear    shouldBe Some(1960)
    detail.countries      shouldBe Seq("Włochy", "Francja")
    detail.runtimeMinutes shouldBe Some(176)
    detail.cast           should contain allOf ("Marcello Mastroianni", "Anita Ekberg")
    // This film leaves Reżyseria blank (credits everything to Scenariusz), so the
    // director field is empty — the year alone still resolves it.
    detail.director shouldBe empty
  }

  it should "harvest director, year, countries, genre, cast, runtime and synopsis when the page populates them" in {
    val detail = client.fetchFilmDetail("https://pionier1907.pl/event/ojczyzna").value
    detail.director       shouldBe Seq("Paweł Pawlikowski")
    detail.releaseYear    shouldBe Some(2026)
    detail.genres         shouldBe Seq("dramat")
    detail.countries      shouldBe Seq("Francja", "Niemcy", "Polska", "Włochy")
    detail.runtimeMinutes shouldBe Some(82) // "1h 22min"
    detail.cast           should contain allOf ("Sandra Hüller", "August Diehl")
    // Synopsis lives in its own <p> here (no awards interleaving), so it is read.
    detail.synopsis.value should startWith("„Ojczyzna")
  }

  it should "recover the year from the Produkcja field when there is no separate Rok field" in {
    // The Fellini-retrospective pages fold the year into Produkcja
    // ("Włochy, Francja, 1957") and carry no `Rok:` line. The year must still be
    // read, and it must NOT leak into the country list.
    val detail = client.fetchFilmDetail("https://pionier1907.pl/event/federico-fellini-noce-cabirii").value
    detail.releaseYear shouldBe Some(1957)
    detail.countries   shouldBe Seq("Włochy", "Francja")
    detail.director    shouldBe Seq("Federico Fellini")
    detail.runtimeMinutes shouldBe Some(110) // "110’" (prime, not "min")
  }

  it should "return None when the detail-page fetch fails (no fixture)" in {
    client.fetchFilmDetail("https://pionier1907.pl/event/nie-ma-takiego-filmu") shouldBe None
  }
}
