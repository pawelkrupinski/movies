package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaCityArkadia, CinemaCityKinepolis, CinemaCityWroclavia, CinemaMovie, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.tasks.ScrapeCinemaHandler

import java.time.{Clock, Instant, ZoneOffset}

/**
 * Locks the roster-staleness signal the worker exposes for the Grafana panel:
 * per country, how long the LEAST recently scraped cinema has gone without a
 * successful scrape, and how many cinemas have never scraped at all.
 *
 * The load-bearing behaviour is that the two are kept apart — a never-scraped
 * cinema must not read as a 0s-old one, which would let the worst case hide
 * behind a healthy-looking maximum.
 */
class CinemaScrapeCensusSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-07-28T20:00:00Z")

  private class FakeScraper(val cinema: Cinema) extends CinemaScraper {
    def scrapeHosts: Set[String]  = Set.empty
    def fetch(): Seq[CinemaMovie] = Seq.empty
  }

  private def keyOf(cinema: Cinema) = ScrapeCinemaHandler.dedupKey(cinema)

  private val fresh  = CinemaCityKinepolis
  private val stale  = CinemaCityWroclavia
  private val nevers = CinemaCityArkadia

  private def freshness(): InMemoryFreshnessStore = {
    val f = new InMemoryFreshnessStore
    f.markFresh(keyOf(fresh), FreshnessKind.CinemaScrape, now.minusSeconds(120))
    f.markFresh(keyOf(stale), FreshnessKind.CinemaScrape, now.minusSeconds(7200))
    // `nevers` is deliberately left unstamped.
    f
  }

  "census" should "report the age of the least recently scraped cinema" in {
    val staleness = CinemaScrapeCensus.census(Seq(keyOf(fresh), keyOf(stale)), freshness().lastFetchedAt, now)

    staleness.oldestAgeSeconds shouldBe 7200.0
    staleness.neverScraped     shouldBe 0
  }

  it should "count a never-scraped cinema instead of reading it as zero seconds old" in {
    val staleness = CinemaScrapeCensus.census(
      Seq(keyOf(fresh), keyOf(stale), keyOf(nevers)), freshness().lastFetchedAt, now)

    // The never-scraped cinema must NOT drag the maximum down to 0 — the whole
    // point of holding the two apart.
    staleness.oldestAgeSeconds shouldBe 7200.0
    staleness.neverScraped     shouldBe 1
  }

  it should "report a zero age when nothing has ever scraped" in {
    val staleness = CinemaScrapeCensus.census(Seq(keyOf(nevers)), new InMemoryFreshnessStore().lastFetchedAt, now)

    staleness.oldestAgeSeconds shouldBe 0.0
    staleness.neverScraped     shouldBe 1
  }

  it should "clamp a future stamp to zero rather than going negative" in {
    val f = new InMemoryFreshnessStore
    f.markFresh(keyOf(fresh), FreshnessKind.CinemaScrape, now.plusSeconds(30))

    CinemaScrapeCensus.census(Seq(keyOf(fresh)), f.lastFetchedAt, now).oldestAgeSeconds shouldBe 0.0
  }

  "sample" should "publish both gauges under this country's label" in {
    val registry                    = new PrometheusRegistry()
    val (oldestAge, neverScraped)   = CinemaScrapeCensus.gauges(registry)
    val scrapers                    = Seq(fresh, stale, nevers).map(new FakeScraper(_))

    val census = new CinemaScrapeCensus(scrapers, freshness(), oldestAge, neverScraped, Country.Poland,
      Clock.fixed(now, ZoneOffset.UTC))
    census.sample()

    val exposition = PrometheusExposition.render(registry)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.OldestAgeName, """country="pl"""")    shouldBe Some(7200.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.NeverScrapedName, """country="pl"""") shouldBe Some(1.0)
  }

  it should "seed both series at zero from construction so the panel has no boot gap" in {
    val registry                  = new PrometheusRegistry()
    val (oldestAge, neverScraped) = CinemaScrapeCensus.gauges(registry)

    // Constructed but never sampled — the series must already exist.
    new CinemaScrapeCensus(Seq(new FakeScraper(stale)), freshness(), oldestAge, neverScraped, Country.Poland,
      Clock.fixed(now, ZoneOffset.UTC))

    val exposition = PrometheusExposition.render(registry)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.OldestAgeName, """country="pl"""")    shouldBe Some(0.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.NeverScrapedName, """country="pl"""") shouldBe Some(0.0)
  }
}
