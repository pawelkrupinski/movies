package clients.kino_spektrum

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoSpektrum
import services.cinemas.pl.KinoSpektrumClient

import java.time.LocalDateTime

/** Replays the recorded Kino Spektrum repertoire (06-06-2026 capture of
 *  `bilety.kinospektrum.pl/index.php`) through the client. The page renders one
 *  `div.event-item` per screening; the booking-URL slug tail
 *  (`…-YYYY-MM-DD-HH-MM`) is the exact start, and the anchor text is the clean
 *  title (decoration baked into the slug is ignored). */
class KinoSpektrumClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-spektrum")

  "KinoSpektrumClient" should "parse film screenings off the repertoire page" in {
    val movies = new KinoSpektrumClient(http, KinoSpektrum).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSpektrum)

    // Every film has at least one showtime with a plausible 2026 date.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "read the exact date+time off the booking-URL slug tail" in {
    val movies = new KinoSpektrumClient(http, KinoSpektrum).fetch()

    // Pinned screening seen in the captured fixture: "Zimna wojna" opens the
    // 06-06-2026 day at 18:45 in Sala Duża, booking on the ticketing host.
    val zimnaWojna = movies.find(_.movie.title == "Zimna wojna").value
    zimnaWojna.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 18, 45))
    val slot = zimnaWojna.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 6, 18, 45)).value
    slot.bookingUrl.value should startWith("https://bilety.kinospektrum.pl/index.php/kup-bilet/")
    slot.room.value shouldBe "Sala Duża"
  }

  it should "attach the director from the description's 'Reżyseria:' line" in {
    val movies = new KinoSpektrumClient(http, KinoSpektrum).fetch()
    // Only films whose description carries a labelled `Reżyseria:` line get a
    // director; the captured fixture has "Propozycja" (reż. John Hillcoat).
    movies.filter(_.director.nonEmpty).flatMap(_.director) should contain("John Hillcoat")
    // The labelled line is the only source — a film with a plain synopsis and no
    // `Reżyseria:` label keeps an empty director (no prose leakage).
    movies.find(_.movie.title == "Zimna wojna").value.director shouldBe empty
  }

  it should "read the runtime off the fa-history clock element" in {
    val movies = new KinoSpektrumClient(http, KinoSpektrum).fetch()
    // "Zimna wojna" carries `…fa-history"></i> <i>88''</i>` → 88 minutes.
    movies.find(_.movie.title == "Zimna wojna").value.movie.runtimeMinutes shouldBe Some(88)
  }
}
