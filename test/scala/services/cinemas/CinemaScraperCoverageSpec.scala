package services.cinemas

import models.{Cinema, City}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

/** The composition root (`Wiring.cinemaScrapers`) is the only place that maps a
 *  modelled `Cinema` to a live scraper. This guards the two failure modes a
 *  multi-city expansion is prone to: a scraper wired for a cinema no city
 *  scopes (orphan scrape work), and a regression that drops a Poznań venue.
 *
 *  Wrocław/Warszawa cinemas are modelled and have ready scrapers, but those
 *  entries are commented out until the multi-city rollout goes live — so this
 *  asserts the *active* set rather than full `Cinema.all` coverage. When the
 *  rollout enables them, tighten `wiredCinemas shouldBe Cinema.all.toSet`. */
class CinemaScraperCoverageSpec extends AnyFlatSpec with Matchers {

  // FixtureTestWiring builds the same scraper list as production, offline
  // (FakeHttpFetch, no Mongo). Constructing the scrapers does no network I/O —
  // fetch() is never called here.
  private val wiredCinemas: Set[Cinema] =
    new FixtureTestWiring("multikino").cinemaScrapers.map(_.cinema).toSet

  "Every wired scraper" should "be for a modelled, city-scoped cinema (no orphans)" in {
    val scoped = City.all.flatMap(_.cinemas).toSet
    wiredCinemas.subsetOf(Cinema.all.toSet) shouldBe true
    wiredCinemas.subsetOf(scoped)           shouldBe true
  }

  "Every Poznań cinema" should "have exactly one wired scraper" in {
    wiredCinemas shouldBe Cinema.poznan.toSet
  }
}
