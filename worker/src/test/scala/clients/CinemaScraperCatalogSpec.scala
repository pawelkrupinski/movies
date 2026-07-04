package clients

import clients.tools.FakeHttpFetch
import models.{AdaKinoStudyjne, Cinema, KinoFenomen, KinoKameralne, KinoKryterium, KinoPort}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraperCatalog
import _root_.tools.CachingDetailFetch

import java.time.LocalDate
import scala.concurrent.duration._

/**
 * Guards the per-cinema fetch seams the catalog wires for venues whose origin
 * blocks our Fly datacenter IP, so they must fetch through their own injected
 * seam — NOT the shared `http` — or the live scrape regresses:
 *   - biletyna.pl 403s our IP (Cloudflare waiting-room) → Kino Kameralne /
 *     ADA Kino Studyjne fetch through `bnFetch` (Zyte in prod, fixture fake here).
 *   - bilety.ck105.koszalin.pl (Kino Kryterium) times out our IP AND every Decodo
 *     proxy IP at the TCP layer → fetches through `zyteFetch` (Zyte's
 *     true-residential network in prod, the one egress that reaches it).
 * Each seam's fixture-less `http` makes a leaked fetch throw / come back empty,
 * so a refactor that re-buries the fetch on `http` is caught here. (CI also sets
 * ZYTE_API_KEY, so a leak onto `http` would route biletyna through real Zyte.)
 */
class CinemaScraperCatalogSpec extends AnyFlatSpec with Matchers with OptionValues {

  // `http` deliberately points at a fixture directory without any biletyna/ck105
  // capture, so any cinema that wrongly fetches through it (instead of its own
  // seam) fails.
  private val http = new FakeHttpFetch("does-not-exist")

  /** Catalog with each Fly-IP-blocked seam pointed at a fixture directory (or the
   *  fixture-less `http` by default); a cinema that leaks onto the wrong seam
   *  throws / returns empty. */
  private def catalog(biletyna: String = "does-not-exist",
                      zyte:     String = "does-not-exist"): CinemaScraperCatalog =
    new CinemaScraperCatalog(
      http, mkFetch = http, bnFetch = new FakeHttpFetch(biletyna), today = LocalDate.of(2026, 6, 6),
      chainDetailCache = (h, ttl) => new CachingDetailFetch(h, ttl),
      zyteFetch = new FakeHttpFetch(zyte)
    )

  "CinemaScraperCatalog" should "route Kino Kameralne through the injected biletyna seam, not the shared http" in {
    val scraper = catalog(biletyna = "kino-kameralne").all.find(_.cinema == KinoKameralne).value
    val movies  = scraper.fetch()  // reads the kino-kameralne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKameralne)
  }

  it should "route ADA Kino Studyjne through the injected biletyna seam, not the shared http" in {
    val scraper = catalog(biletyna = "ada-kino-studyjne").all.find(_.cinema == AdaKinoStudyjne).value
    val movies  = scraper.fetch()  // reads the ada-kino-studyjne fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(AdaKinoStudyjne)
  }

  // Kino Fenomen (WDK) is iframe639.biletyna.pl — a biletyna host whose per-film
  // /artist/view/id detail pages 403 our Fly IP behind Cloudflare, so its deferred
  // detail enrichment must fetch through `bnFetch`, not the shared `http`. Wired
  // on `http` (as it was when the venue was added), every detail fetch 403s → the
  // enrichment /uptime bar goes red; here the fixture-less `http` makes the leak a
  // hard failure. Fixture captured 2026-07-04 from `iframe639.biletyna.pl/?display=events`.
  it should "route Kino Fenomen through the injected biletyna seam, not the shared http" in {
    val scraper = catalog(biletyna = "kino-fenomen").all.find(_.cinema == KinoFenomen).value
    val movies  = scraper.fetch()  // reads the kino-fenomen fixture via bnFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoFenomen)
  }

  // Kino Kryterium's origin (bilety.ck105.koszalin.pl) silently times out our Fly
  // egress IP AND every Decodo proxy IP at the TCP layer, so a direct scrape came
  // back empty → a permanent white /uptime bar. It must fetch through the Zyte
  // seam (the one egress that reaches it). The shared `http` has no ck105 fixture,
  // so a fetch that leaked onto it returns empty and this fails. Fixture captured
  // 2026-06-15 from the live month pages `/MSI/mvc/pl?sort=Name&date=2026-06` (+ 2026-07).
  it should "route Kino Kryterium through the injected Zyte seam, not the shared http" in {
    val scraper = catalog(zyte = "kino-kryterium").all.find(_.cinema == KinoKryterium).value
    val movies  = scraper.fetch()  // reads the kino-kryterium fixture via zyteFetch
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKryterium)
  }

  // KinoPort lost its gcsw.pl/kino/ programme alias in a 2026-06 site rebuild
  // (the old KinoPortClient 404'd in prod), so it's now served off Filmweb's
  // seances API for the Gdańsk venue. Guard the seam: it must read filmweb.pl,
  // never the retired gcsw.pl host.
  it should "scrape KinoPort off Filmweb, not the retired gcsw.pl alias" in {
    val scraper = catalog(biletyna = "kino-kameralne").all.find(_.cinema == KinoPort).value
    scraper.scrapeHosts should contain ("www.filmweb.pl")
    scraper.scrapeHosts should not contain "gcsw.pl"
  }

  // A `Cinema` that's modelled (so it shows on the web/in a city) but has no
  // scraper is silently never populated — the city renders empty forever. This
  // also catches a `City.slug` ↔ `byCity` key mismatch: `catalog.all` resolves
  // scrapers by `c.slug`, so a typo drops that city's cinemas out of `all`.
  it should "wire a scraper for every modelled cinema" in {
    val scraped  = catalog(biletyna = "kino-kameralne").all.map(_.cinema).toSet
    val modelled = Cinema.all.toSet
    withClue(s"modelled but unscraped: ${(modelled diff scraped).map(_.displayName).toSeq.sorted}") {
      (modelled diff scraped) shouldBe empty
    }
  }

  // `MonitoringHttpFetch` suppresses these hosts so cinema scrapes don't
  // double-record under their host in the uptime page's "Other" bucket. A
  // scraper that returns an EMPTY set leaks its host into "Other" — exactly the
  // bug this guards. The abstract `CinemaScraper.scrapeHosts` makes the compiler
  // demand the method; this makes the runtime demand it be non-trivial.
  it should "declare a non-empty scrapeHosts for every scraper" in {
    val leaking = catalog(biletyna = "kino-kameralne").all.filter(_.scrapeHosts.isEmpty).map(_.cinema.displayName)
    withClue(s"scrapers with empty scrapeHosts: ${leaking.sorted}") { leaking shouldBe empty }
  }

  // Spot-check the union covers the host shapes that were leaking into "Other":
  // bespoke per-cinema domains, shared national-chain hosts, and the per-venue
  // bilety24 subdomains. A missed client or a re-spelled host fails here.
  it should "cover representative bespoke, shared-chain and per-venue hosts" in {
    val hosts = catalog(biletyna = "kino-kameralne").scrapeHosts
    val expected = Set(
      "kinomuranow.pl", "amok.gliwice.pl", "stacjafalenica.pl",   // bespoke per-cinema
      "www.multikino.pl", "www.cinema-city.pl", "restapi.helios.pl", "www.filmweb.pl", "www.novekino.pl", // shared chains
      "kinoluna.bilety24.pl", "www.bilety24.pl",   // legacy per-venue bilety24 subdomain + migrated organizer host
    )
    withClue(s"missing from catalog.scrapeHosts: ${(expected diff hosts).toSeq.sorted}") {
      (expected diff hosts) shouldBe empty
    }
  }

  // Helios detail (`/api/movie/{id}`) is identical across all Helios venues and
  // shared through one chain cache; it refreshes more eagerly than Cinema City's
  // film page so ratings/runtime changes surface within 2h, not 6h.
  it should "cache Helios chain detail for 2h and Cinema City for 6h" in {
    val built = catalog(biletyna = "kino-kameralne")
    built.heliosDetailTtl shouldBe 2.hours
    built.cinemaCityDetailTtl shouldBe 6.hours
  }

  it should "expose only bare lower-case hosts (no scheme, port or path)" in {
    catalog(biletyna = "kino-kameralne").scrapeHosts.foreach { h =>
      withClue(s"malformed host: '$h'") {
        h shouldBe h.toLowerCase
        h should not include "/"
        h should not include ":"
      }
    }
  }
}
