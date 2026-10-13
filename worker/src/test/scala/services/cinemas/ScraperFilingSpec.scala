package services.cinemas

import clients.tools.FakeHttpFetch
import models.{Cinema, City, Country, Helios, KinoApollo, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How the catalog files its scrapers under city pages ([[CinemaScraperCatalog.file]]):
 *  every scraper it builds ends up somewhere, none dropped and none overwritten. */
class ScraperFilingSpec extends AnyFlatSpec with Matchers {

  private val poznan  = Country.Poland.bySlug("poznan")
  private val wroclaw = Country.Poland.bySlug("wroclaw")

  "Filing" should "keep every scraper, two of one page side by side rather than one over the other" in {
    val a = new StubCinemaScraper(Helios)
    val b = new StubCinemaScraper(KinoApollo)
    val c = new StubCinemaScraper(Multikino)
    val pages: Map[Cinema, City] = Map(Helios -> poznan, KinoApollo -> poznan, Multikino -> wroclaw)
    val filed = CinemaScraperCatalog.file(Seq(Seq(a), Seq(b, c)), pages.get)
    filed.byCity(poznan) shouldBe Seq(a, b)          // the second group's scraper did not replace the first's
    filed.byCity(wroclaw) shouldBe Seq(c)
    filed.unplaced shouldBe empty
  }

  it should "keep a scraper whose cinema no page lists, as unplaced, instead of dropping it" in {
    val lost  = new StubCinemaScraper(Multikino)
    val filed = CinemaScraperCatalog.file(Seq(Seq(lost)), _ => None)
    filed.byCity shouldBe empty
    filed.unplaced shouldBe Seq(lost)
  }

  "The catalog" should "file every scraper it builds, so the scraped set is all of them" in {
    val catalog = new CinemaScraperCatalog(new FakeHttpFetch("multikino"), models.VenueClock.fixedOn(java.time.LocalDate.of(2026, 6, 8)),
                                           tools.SpecClock.Pinned)
    catalog.unplaced shouldBe empty
    catalog.byCity.values.flatten.size shouldBe catalog.built
  }
}
