package clients

import clients.tools.FakeHttpFetch
import models.KinoKameralne
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog

import java.time.LocalDate

/**
 * Guards the per-cinema fetch seams the catalog wires. biletyna.pl 403s our
 * datacenter IP, so Kino Kameralne must fetch through the injected `bnFetch`
 * (Zyte in prod, the fixture fake in tests) — NOT the shared `http`. If a
 * refactor re-buries the fetch on `http`, the live scrape regresses (CI sets
 * ZYTE_API_KEY, so `http` would route through real Zyte → biletyna) and this
 * test catches it: `http` here has no biletyna fixture, so a Kameralne scrape
 * that leaked onto it would throw instead of returning the recorded programme.
 */
class CinemaScraperCatalogSpec extends AnyFlatSpec with Matchers with OptionValues {

  // `http` deliberately points at a fixture dir without the biletyna capture, so
  // any cinema that wrongly fetches through it (instead of its own seam) fails.
  private val http    = new FakeHttpFetch("does-not-exist")
  private val bnFetch = new FakeHttpFetch("kino-kameralne")

  private val catalog =
    new CinemaScraperCatalog(http, mkFetch = http, bnFetch = bnFetch, today = LocalDate.of(2026, 6, 6))

  "CinemaScraperCatalog" should "route Kino Kameralne through the injected biletyna seam, not the shared http" in {
    val scraper = catalog.all.find(_.cinema == KinoKameralne).value
    val movies  = scraper.fetch()  // reads the kino-kameralne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKameralne)
  }
}
