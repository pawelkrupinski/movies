package clients.praha

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import models.KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.PrahaClient

import java.time.LocalDateTime

/** Replays the recorded `mteatr.pl/pl/repertuar-kino-praha` listing through the
 *  client. The page is a bespoke server-rendered layout; each screening's
 *  absolute date+time is read off its `div.label` stamp ("10 Cze 2026 / 15:30").
 *
 *  Kino Praha was closed 18 May – 9 Jun 2026 and only fed Filmweb thinly while
 *  reopening; this fixture is the proof its programme is real and reachable on
 *  its own site — the reason we scrape mteatr.pl rather than Filmweb id 2180. */
class PrahaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new PrahaClient(new FakeHttpFetch("kino-praha")).fetch()

  "PrahaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "group a film's repeat screenings under one title with absolute dates" in {
    // "Orły republiki" plays several days; the 10 Jun 20:30 slot is pinned.
    val film = movies.find(_.movie.title == "Orły republiki").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 10, 20, 30))
    film.showtimes.size should be > 1
    film.filmUrl.value should startWith("https://www.mteatr.pl/pl/")
  }

  it should "read the real stamp on a badged screening, not the badge label" in {
    // "Drugie życie" carries a "przedpremiera" badge (div.label.special-1) next
    // to its date stamp; the parser must take the date, never the badge text.
    val film = movies.find(_.movie.title == "Drugie życie").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 10, 18, 10))
  }

  it should "date every screening into the recorded window, never inferring the year" in {
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }
}
