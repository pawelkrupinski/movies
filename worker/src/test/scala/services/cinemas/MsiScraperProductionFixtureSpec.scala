package services.cinemas

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.{Codec, Source}

/** Fixture-replay coverage proving the release year mined from the
 *  `RepertoireEvents` Description survives end-to-end through
 *  `MsiScraper.parseMonthWithYear` for a real recorded Zamek Szczecin month
 *  page. The director-only parse left the year `None`, so same-title films fell
 *  to titleOnly TMDB resolution; the production-line parse fixes that. Lives in
 *  `services.cinemas` to reach the package-private `MsiScraper`. */
class MsiScraperProductionFixtureSpec extends AnyFlatSpec with Matchers with OptionValues {

  // Recorded `GET https://bilety.zamek.szczecin.pl/MSI/mvc/pl?sort=Name&date=2026-06`.
  private val fixture =
    "test/resources/fixtures/kino-zamek/bilety.zamek.szczecin.pl/MSI/mvc/pl.8781cb26"

  private val html = {
    val src = Source.fromFile(fixture)(using Codec.UTF8)
    try src.mkString finally src.close()
  }

  // For this assertion we keep the raw title verbatim and emit no format tokens,
  // so the rendered block's `title` attr matches the `RepertoireEvents` Name and
  // the mined metadata joins back on.
  private val slots =
    MsiScraper.parseMonthWithYear(html, java.time.YearMonth.of(2026, 6),
      "https://bilety.zamek.szczecin.pl", raw => (raw.trim, Nil))

  private val movies = MsiScraper.toMovies(slots, models.KinoZamekSzczecin)

  "MsiScraper" should "carry the release year for a foreign film" in {
    // Description: "(Casablanca, USA 1942, 102’) ... REŻYSERIA: Michael Curtiz"
    val casablanca = movies.find(_.movie.title.toUpperCase.startsWith("CASABLANCA")).value
    casablanca.movie.releaseYear shouldBe Some(1942)
  }

  it should "carry the release year for a Polish film (paren opens on the country)" in {
    // Description: "(Polska 1976, 153’) ..."
    val czlowiek = movies.find(_.movie.title.toUpperCase.contains("CZŁOWIEK Z MARMURU")).value
    czlowiek.movie.releaseYear shouldBe Some(1976)
  }
}
