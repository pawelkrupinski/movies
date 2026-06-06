package services.cinemas

import models.{Cinema, City}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

/** The composition root (`Wiring.cinemaScrapers`) is the only place that maps a
 *  modelled `Cinema` to a live scraper. This invariant catches the two failure
 *  modes a multi-city expansion is prone to: a cinema added to the model but
 *  never wired (so its city shows a venue that never gets showtimes), and a
 *  scraper wired for a cinema no city scopes (dead scrape work). */
class CinemaScraperCoverageSpec extends AnyFlatSpec with Matchers {

  // FixtureTestWiring builds the same scraper list as production, offline
  // (FakeHttpFetch, no Mongo). Constructing the scrapers does no network I/O —
  // fetch() is never called here.
  private val wiredCinemas: Set[Cinema] =
    new FixtureTestWiring("multikino").cinemaScrapers.map(_.cinema).toSet

  "Every modelled cinema" should "have exactly one wired scraper" in {
    wiredCinemas shouldBe Cinema.all.toSet
    new FixtureTestWiring("multikino").cinemaScrapers.size shouldBe Cinema.all.size
  }

  "Every scraped cinema" should "be scoped to some city" in {
    wiredCinemas shouldBe City.all.flatMap(_.cinemas).toSet
  }
}
