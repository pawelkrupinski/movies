package clients.cinemas

import models._
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.MsiClient

import java.time.{LocalDate, LocalDateTime}

/** Max Kino (Świebodzin) — repertuar.maxkino.eu — runs the MSI ticketing
 *  platform, but renders the older "list-group" skin: each film is a
 *  `div.list-group-item.event-background` and showtimes are `a.badge-purple`
 *  anchors in `ul.repo-event-dates`, not the `div.movies-movie__single` blocks
 *  the standard skin uses. The generic `MsiClient`/`MsiScraper` returned an
 *  empty list for it (white on /uptime) until the scraper learned to fall back
 *  to that skin. This replays the recorded June 2026 month page and pins a
 *  concrete screening, proving the migration off Filmweb onto the venue's own
 *  MSI portal works.
 *
 *  `today` is pinned into June 2026 so the client fetches the recorded June
 *  fixture and then attempts July 2026 (its own recorded — empty — page). */
class KinoMaxKinoMsiSpec extends AnyFlatSpec with Matchers with OptionValues {

  private lazy val movies =
    new MsiClient(new FakeHttpFetch("kino-max-swiebodzin"), "https://repertuar.maxkino.eu",
      KinoMaxKino, today = LocalDate.of(2026, 6, 21)).fetch()

  it should "return a non-empty, single-cinema film list off the list-group skin" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMaxKino)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "carry plausible June 2026 dates only" in {
    val dates = movies.flatMap(_.showtimes).map(_.dateTime)
    dates should not be empty
    all(dates.map(_.getYear)) shouldBe 2026
    all(dates.map(_.getMonthValue)) shouldBe 6
  }

  it should "pin a concrete Backrooms screening (badge-purple anchor in repo-event-dates)" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("backrooms")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 18, 20))
  }

  it should "drop the Premiera! badge and bare format suffix from the title" in {
    // The `.event-title` div is polluted with a `Premiera!` badge span and some
    // titles carry a bare DUB suffix ("Straszny Film DUB"); the clean title comes
    // from the poster anchor's `title` attr, sentence-cased and format-stripped.
    val titles = movies.map(_.movie.title)
    all(titles.map(_.toLowerCase)) should not include "premiera"
    titles.find(_.toLowerCase.contains("straszny film")).value shouldBe "Straszny film"
  }
}
