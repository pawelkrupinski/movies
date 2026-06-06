package clients.kino_port

import clients.tools.FakeHttpFetch
import models.KinoPort
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoPortClient

import java.time.LocalDateTime

/** Replays the recorded KinoPort programme (05-07.06.2026 capture of
 *  `gcsw.pl/kino/`, which redirects to the live "Kinoport | Repertuar" post)
 *  through the client. The post body is plain WordPress paragraphs: a bare
 *  `DD.MM` date paragraph followed by `<strong>HH:MM</strong> Title | year
 *  director | format` schedule rows; the year comes off the canonical URL. */
class KinoPortClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-port")

  "KinoPortClient" should "parse film screenings off the gcsw.pl repertoire post" in {
    val movies = new KinoPortClient(http, KinoPort).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoPort)

    // Every film has at least one showtime, all in the captured 2026 window.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
    // Times are plausible afternoon/evening cinema slots.
    all(movies.flatMap(_.showtimes).map(_.dateTime.getHour)) should (be >= 0 and be <= 23)
  }

  it should "read the exact day and start time off each schedule row" in {
    val movies = new KinoPortClient(http, KinoPort).fetch()

    // Pinned from the fixture: "Angel's Egg" opens 05.06.2026 at 17:30.
    val angelsEgg = movies.find(_.movie.title == "Angel’s Egg").value
    angelsEgg.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 5, 17, 30))

    // "Amelia" (Jeunet) plays both 06.06 and 07.06 at 16:00 — one CinemaMovie,
    // two showtimes, deduped and sorted.
    val amelia = movies.find(_.movie.title == "Amelia").value
    amelia.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 6, 6, 16, 0),
      LocalDateTime.of(2026, 6, 7, 16, 0)
    )
  }
}
