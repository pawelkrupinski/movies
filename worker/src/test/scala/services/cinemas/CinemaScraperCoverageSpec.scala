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

  // `byCity` wires EVERY modelled city's cinemas — including the UK cities
  // currently disabled, which drop out of the live `City.all` / `catalog.all`
  // view but stay fully modelled. Coverage is checked here (not against
  // `catalog.all`) so "a cinema added to the model but never wired" is still
  // caught while its city is offline.
  private val wiredCinemas: Seq[Cinema] = catalog.byCity.values.flatten.map(_.cinema).toSeq
  private val modelledSlugs: Set[String] = City.allModelled.map(_.slug).toSet

  "Every modelled cinema" should "have exactly one wired scraper" in {
    wiredCinemas.toSet shouldBe Cinema.all.toSet
    wiredCinemas.size shouldBe Cinema.all.size
    // No scraper wired under a slug that no modelled city scopes (a typo would
    // drop that city's cinemas out of the worker's `byCity` lookup).
    catalog.byCity.keySet.subsetOf(modelledSlugs) shouldBe true
  }

  "Every live-scraped cinema" should "be scoped to a live city" in {
    // `catalog.all` is the live view the worker actually operates (detail
    // enrichers, markers, source URLs), so it must cover exactly the live
    // cities' cinemas — no dead scrapers for disabled cities.
    catalog.all.map(_.cinema).toSet shouldBe City.all.flatMap(_.cinemas).toSet
  }
}
