package clients.kino_forum

import models.KinoForum
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoForumClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded Kino Forum repertoire (06-06-2026 capture of
 *  `bok.bialystok.pl/repertuar/`, trimmed to the current-and-future rows) through
 *  the client. The page renders one `div.repertoire-row` per screening, tagged
 *  with `data-date`/`data-hour`; runtime presence (`NN'`) is the film filter and
 *  `today` gates out the venue's multi-year archive of past screenings. */
class KinoForumClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http  = new FakeHttpFetch("bialystok")
  // Pin to the fixture's capture date so the archive-gating cut-off is stable.
  private val today = LocalDate.of(2026, 6, 6)

  "KinoForumClient" should "parse film screenings off the repertoire table" in {
    val movies = new KinoForumClient(http, today).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoForum)

    // Every film has at least one showtime with a plausible 2026 date.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "read date, time, runtime, country and booking URL off a row" in {
    val movies = new KinoForumClient(http, today).fetch()

    // "Orły republiki" recurs across the captured week — its six rows collapse to
    // one film with six distinct showtimes.
    val orly = movies.find(_.movie.title == "Orły republiki").value
    orly.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 6, 20, 15))
    orly.showtimes.size shouldBe 6
    orly.movie.runtimeMinutes.value shouldBe 127
    orly.movie.countries shouldBe Seq("Dania", "Francja", "Szwecja")
    val slot = orly.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 6, 20, 15)).value
    slot.bookingUrl.value shouldBe "https://bilety.bok.bialystok.pl/rezerwacja/termin.html?id=4533"
  }

  it should "keep programme-prefixed titles verbatim and decode HTML entities" in {
    val movies = new KinoForumClient(http, today).fetch()

    // Festival-cycle prefix kept (TitleNormalizer handles it downstream).
    movies.map(_.movie.title) should contain("17. PRZEGLĄD NOWEGO KINA FRANCUSKIEGO: Fałszerz stulecia")
    // `&#038;` → `&` (jsoup entity-decodes the title text).
    movies.map(_.movie.title) should contain("Turner & Constable. Przełomowa wystawa")
  }

  it should "gate out screenings before `today`" in {
    val movies = new KinoForumClient(http, today).fetch()

    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min should be >= today
  }
}
