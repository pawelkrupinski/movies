package clients.bilety24

import models._
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.Bilety24SubdomainClient

import java.time.{LocalDate, LocalDateTime}

/** Kino Astra (Oborniki Śląskie) went silently empty on Filmweb, but its films
 *  are live on the surviving `kulturalne-oborniki.bilety24.pl` subdomain — whose
 *  `/repertuar/` page only renders one day at a time behind `?b24_day=YYYY-MM-DD`.
 *  This replays two recorded days (an empty Thursday + a populated Friday)
 *  through the client and pins a concrete Friday screening, proving the day-walk
 *  reaches and merges the subdomain's programme. */
class Bilety24SubdomainClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  // today=Thu 2026-06-18, daysAhead=1 → fetches ?b24_day=2026-06-18 (empty) and
  // 2026-06-19 (Dzień objawienia 17:00, Ojczyzna 20:00). Both fixtures recorded.
  private lazy val movies = new Bilety24SubdomainClient(
    new FakeHttpFetch("kino-astra"),
    "https://kulturalne-oborniki.bilety24.pl/repertuar/",
    KinoAstra,
    daysAhead = 1,
    today     = LocalDate.of(2026, 6, 18)
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
}
