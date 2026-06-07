package clients

import clients.tools.FakeHttpFetch
import models.{AdaKinoStudyjne, Cinema, KinoKameralne}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog

import java.time.LocalDate

/**
 * Guards the per-cinema fetch seams the catalog wires. biletyna.pl 403s our
 * datacenter IP (Cloudflare waiting-room), so its venues — Kino Kameralne and
 * ADA Kino Studyjne — must fetch through the injected `bnFetch` (Zyte in prod,
 * the fixture fake in tests), NOT the shared `http`. If a refactor re-buries
 * the fetch on `http`, the live scrape regresses (CI sets ZYTE_API_KEY, so
 * `http` would route through real Zyte → biletyna) and these tests catch it:
 * the `http` here has no biletyna fixture, so a scrape that leaked onto it
 * throws instead of returning the recorded programme.
 */
class CinemaScraperCatalogSpec extends AnyFlatSpec with Matchers with OptionValues {

  // `http` deliberately points at a fixture dir without any biletyna capture, so
  // any cinema that wrongly fetches through it (instead of its own seam) fails.
  private val http = new FakeHttpFetch("does-not-exist")

  /** Catalog whose biletyna seam reads `fixtureDir`; `http`/`mkFetch` are the
   *  fixture-less fake so a leaked fetch onto the shared path throws. */
  private def catalogWithBiletyna(fixtureDir: String): CinemaScraperCatalog =
    new CinemaScraperCatalog(
      http, mkFetch = http, bnFetch = new FakeHttpFetch(fixtureDir), today = LocalDate.of(2026, 6, 6)
    )

  "CinemaScraperCatalog" should "route Kino Kameralne through the injected biletyna seam, not the shared http" in {
    val scraper = catalogWithBiletyna("kino-kameralne").all.find(_.cinema == KinoKameralne).value
    val movies  = scraper.fetch()  // reads the kino-kameralne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKameralne)
  }

  it should "route ADA Kino Studyjne through the injected biletyna seam, not the shared http" in {
    val scraper = catalogWithBiletyna("ada-kino-studyjne").all.find(_.cinema == AdaKinoStudyjne).value
    val movies  = scraper.fetch()  // reads the ada-kino-studyjne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(AdaKinoStudyjne)
  }

  // A `Cinema` that's modelled (so it shows on the web/in a city) but has no
  // scraper is silently never populated — the city renders empty forever. This
  // also catches a `City.slug` ↔ `byCity` key mismatch: `catalog.all` resolves
  // scrapers by `c.slug`, so a typo drops that city's cinemas out of `all`.
  it should "wire a scraper for every modelled cinema" in {
    val scraped  = catalogWithBiletyna("kino-kameralne").all.map(_.cinema).toSet
    val modelled = Cinema.all.toSet
    withClue(s"modelled but unscraped: ${(modelled diff scraped).map(_.displayName).toSeq.sorted}") {
      (modelled diff scraped) shouldBe empty
    }
  }
}
