package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaCityArkadia, CinemaCityKinepolis, CinemaCityWroclavia, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.StubCinemaScraper
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.tasks.{DueWindow, PhaseOffset, ScrapeCinemaHandler}

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._

/**
 * Locks the roster-staleness signal the worker exposes for the Grafana panel:
 * per country, how long the LEAST recently scraped cinema has gone without a
 * successful scrape, and how many cinemas have never scraped at all.
 *
 * The load-bearing behaviour is that the two are kept apart — a never-scraped
 * cinema must not read as a 0s-old one, which would let the worst case hide
 * behind a healthy-looking maximum.
 *
 * Beside them: the median and p90 age (so one stale group shows apart from one
 * stale venue), and how many venues are due right now, direct vs chunked (so
 * "nothing is due" shows apart from a scrape or projection stall).
 */
class CinemaScrapeCensusSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-07-28T20:00:00Z")

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

  // Hourly boundaries at :30 past — `now` (20:00) sits mid-window, so `fresh`
  // (19:58) is not due, `stale` (18:00, before the 18:30 boundary) is, and `nevers`
  // (no stamp) is.
  private object HalfPast extends PhaseOffset {
    def millis(dedupKey: String, periodMillis: Long): Long = 30.minutes.toMillis
  }
  private val dueWindow = new DueWindow(_ => 1.hour, 1.hour, HalfPast)

  private def gauges(registry: PrometheusRegistry) = {
    val (oldestAge, neverScraped) = CinemaScrapeCensus.gauges(registry)
    (oldestAge, neverScraped, CinemaScrapeCensus.ageQuantileGauge(registry), CinemaScrapeCensus.dueVenuesGauge(registry))
  }

  private def censusOver(registry: PrometheusRegistry, cinemas: Seq[Cinema], chunked: Cinema => Boolean = _ => false) = {
    val (oldestAge, neverScraped, ageQuantile, dueVenues) = gauges(registry)
    new CinemaScrapeCensus(cinemas.map(new StubCinemaScraper(_)), freshness(), oldestAge, neverScraped, ageQuantile,
      dueVenues, dueWindow, chunked, Country.Poland, Clock.fixed(now, ZoneOffset.UTC))
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

  it should "report the median and p90 age of the cinemas that have scraped, leaving never-scraped ones out" in {
    val f    = new InMemoryFreshnessStore
    val keys = (1 to 10).map(i => s"scrape|venue-$i")
    keys.zipWithIndex.foreach { case (key, i) => f.markFresh(key, FreshnessKind.CinemaScrape, now.minusSeconds(100L * (i + 1))) }

    val quantiles = CinemaScrapeCensus.census(keys :+ keyOf(nevers), f.lastFetchedAt, now).ageQuantiles
      .map { case (q, seconds) => q.label -> seconds }.toMap

    // Nearest rank over ten ages 100..1000 s: the fifth and the ninth.
    quantiles shouldBe Map("0.5" -> 500.0, "0.9" -> 900.0)
  }

  it should "report zero quantiles when nothing has ever scraped" in {
    CinemaScrapeCensus.census(Seq(keyOf(nevers)), new InMemoryFreshnessStore().lastFetchedAt, now)
      .ageQuantiles.map(_._2) shouldBe Seq(0.0, 0.0)
  }

  "dueCount" should "split the due venues into direct and chunked" in {
    val due = CinemaScrapeCensus.dueCount(Seq("a", "b", "c", "d"), chunkedKeys = Set("b", "c"), isDue = Set("a", "b", "d"))

    due shouldBe CinemaScrapeCensus.DueVenues(direct = 2, chunked = 1)
  }

  "sample" should "publish both gauges under this country's label" in {
    val registry = new PrometheusRegistry()
    val census   = censusOver(registry, Seq(fresh, stale, nevers))
    census.sample()

    val exposition = PrometheusExposition.render(registry)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.OldestAgeName, """country="pl"""")    shouldBe Some(7200.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.NeverScrapedName, """country="pl"""") shouldBe Some(1.0)
  }

  it should "publish the age quantiles and the due venues per mode from the reaper's due schedule" in {
    val registry = new PrometheusRegistry()
    // `stale` is chunked; `fresh` (chunked too) is not due, so it must not count.
    censusOver(registry, Seq(fresh, stale, nevers), chunked = Set[Cinema](fresh, stale)).sample()

    val exposition = PrometheusExposition.render(registry)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.AgeQuantileName, """country="pl",quantile="0.5"""") shouldBe Some(120.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.AgeQuantileName, """country="pl",quantile="0.9"""") shouldBe Some(7200.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.DueVenuesName, """country="pl",mode="direct"""")   shouldBe Some(1.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.DueVenuesName, """country="pl",mode="chunked"""")  shouldBe Some(1.0)
  }

  it should "seed every series at zero from construction so the panel has no boot gap" in {
    val registry = new PrometheusRegistry()

    // Constructed but never sampled — the series must already exist.
    censusOver(registry, Seq(stale))

    val exposition = PrometheusExposition.render(registry)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.OldestAgeName, """country="pl"""")    shouldBe Some(0.0)
    PrometheusExposition.sample(exposition, CinemaScrapeCensus.NeverScrapedName, """country="pl"""") shouldBe Some(0.0)
    Seq("0.5", "0.9").foreach { q =>
      PrometheusExposition.sample(exposition, CinemaScrapeCensus.AgeQuantileName, s"""country="pl",quantile="$q"""") shouldBe Some(0.0)
    }
    Seq("direct", "chunked").foreach { mode =>
      PrometheusExposition.sample(exposition, CinemaScrapeCensus.DueVenuesName, s"""country="pl",mode="$mode"""") shouldBe Some(0.0)
    }
  }
}
