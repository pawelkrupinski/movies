package services.cinemas

import clients.tools.FakeHttpFetch
import models.{Cinema, City}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/** The scraper catalogue is the only place that maps a modelled `Cinema` to a
 *  live scraper. This invariant catches the two failure modes a multi-city /
 *  multi-country expansion is prone to: a cinema added to the model but never
 *  wired (so its city shows a venue that never gets showtimes), and a scraper
 *  wired for a cinema no city scopes (dead scrape work). Checked over the WHOLE
 *  catalogue — every country's cities, not one deployment's scoped subset. */
class CinemaScraperCoverageSpec extends AnyFlatSpec with Matchers {

  // Constructing the catalogue does no network I/O — fetch() is never called here.
  private val catalog = new CinemaScraperCatalog(new FakeHttpFetch("multikino"), LocalDate.of(2026, 6, 8))
  private val wiredCinemas: Set[Cinema] = catalog.all.map(_.cinema).toSet

  "Every modelled cinema" should "have exactly one wired scraper" in {
    wiredCinemas shouldBe Cinema.all.toSet
    catalog.all.size shouldBe Cinema.all.size
  }

  "Every scraped cinema" should "be scoped to some city" in {
    wiredCinemas shouldBe City.all.flatMap(_.cinemas).toSet
  }
}
