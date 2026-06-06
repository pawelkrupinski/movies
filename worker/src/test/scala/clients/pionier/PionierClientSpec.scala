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
}
