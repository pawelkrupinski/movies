package clients.bilety24

import models._
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.Bilety24SubdomainClient

import java.time.{LocalDate, LocalDateTime}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Kino Astra (Oborniki Śląskie) went silently empty on Filmweb, but its films
 *  are live on the surviving `kulturalne-oborniki.bilety24.pl` subdomain — whose
 *  `/repertuar/` page only renders one day at a time behind `?b24_day=YYYY-MM-DD`.
 *  This replays two recorded days (an empty Thursday + a populated Friday)
 *  through the client and pins a concrete Friday screening, proving the day-walk
 *  reaches and merges the subdomain's programme. */
class Bilety24SubdomainClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  // today=Thu 2026-06-18 → walks from an empty Thursday onto 2026-06-19
  // (Dzień objawienia 17:00, Ojczyzna 20:00). Both fixtures recorded; the days
  // past them have none, which reads as a quiet venue and ends the walk.
  private lazy val movies = new Bilety24SubdomainClient(
    new FakeHttpFetch("kino-astra"),
    "https://kulturalne-oborniki.bilety24.pl/repertuar/",
    KinoAstra,
    today     = LocalDate.of(2026, 6, 18),
    titles    = titleNormalizer
  ).fetch()

  it should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoAstra: Cinema)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete Friday screening read off the ?b24_day page" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("dzień objawienia")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 19, 17, 0))
  }

  // ── The programme past the fixed window ──────────────────────────────────
  //
  // This venue screens on scattered days, not in a block: measured 2026-08-05 it
  // ran on the 12th, 17th, 24th and 25th day ahead — every one of them past the
  // nine-day window the client used to ask for, and separated by blank runs of up
  // to a week. A fixed window cannot express that; following the programme can
  // (`ScrapeHorizon.liveDays`), and the blank runs are well inside the stop rule.

  private val start = LocalDate.of(2026, 8, 5)
  private val screeningDays = Set(12, 17, 24, 25).map(start.plusDays(_))

  private def dayHtml(day: LocalDate): String =
    if (!screeningDays.contains(day)) "<html><body>Brak seansów</body></html>"
    else s"""<a href="/kino/jakis-film" title="Film: Jakiś film - $day 18:00 - Oborniki">seans</a>"""

  it should "reach screenings scattered weeks out, across the blank days between them" in {
    val stub = new tools.GetOnlyHttpFetch {
      def get(url: String): String =
        dayHtml(LocalDate.parse("""b24_day=(\d{4}-\d{2}-\d{2})""".r.findFirstMatchIn(url).map(_.group(1)).get))
    }
    val walked = new Bilety24SubdomainClient(
      stub, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra,
      today = start, titles = titleNormalizer
    ).fetch()

    walked.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet shouldBe screeningDays
  }
}
