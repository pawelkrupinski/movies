package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaCityArkadia, CinemaCityKinepolis, CinemaCityWroclavia, CinemaMovie, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.scrapes.{ArchivedScrape, ScrapeArchiveRepository}

import java.time.{Clock, Instant, ZoneOffset}

/**
 * Locks the signal that separates a cinema whose parser has silently stopped
 * matching from one that is merely closed for the season — the distinction
 * `/uptime` cannot draw, because both render as an identical white "zero results"
 * row and neither errors.
 *
 * The load-bearing behaviour is the failed-read case. This census counts cinemas
 * that produced nothing, so a short or failed archive read looks exactly like the
 * whole roster falling silent at once. Publishing that would invert the metric's
 * entire purpose: a Mongo blip would page as a country-wide outage.
 */
class CinemaContentCensusSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-08-03T12:00:00Z")

  private class FakeScraper(val cinema: Cinema) extends CinemaScraper {
    def scrapeHosts: Set[String]  = Set.empty
    def fetch(): Seq[CinemaMovie] = Seq.empty
  }

  private val producing = CinemaCityKinepolis   // had films this morning
  private val quiet     = CinemaCityWroclavia   // last had films 40 days ago
  private val never     = CinemaCityArkadia     // has never produced any

  private val roster = Seq(producing, quiet, never).map(new FakeScraper(_))

  /** An archive that answers with exactly `stamps` — including, when empty, the
   *  read that could not be completed. */
  private class StubArchive(var stamps: Map[String, Option[Instant]]) extends ScrapeArchiveRepository {
    def enabled: Boolean = true
    protected def storeSuccess(c: Cinema, city: Option[String], s: services.scrapes.SuccessfulScrape): Unit = ()
    protected def storeBarren(c: Cinema, city: Option[String], a: services.scrapes.BarrenAttempt): Unit     = ()
    def find(cinema: Cinema): Option[ArchivedScrape] = None
    def scan(consume: Seq[ArchivedScrape] => Unit): Boolean = true
    def lastContentAt(): Map[String, Option[Instant]] = stamps
  }

  private val healthyStamps = Map(
    producing.displayName -> Some(now.minusSeconds(3600)),
    quiet.displayName     -> Some(now.minusSeconds(40 * 86400)),
    never.displayName     -> Option.empty[Instant]
  )

  private def census(archive: ScrapeArchiveRepository) = {
    val (c, oldestAge, neverContent, _) = censusWithStale(archive)
    (c, oldestAge, neverContent)
  }

  private def censusWithStale(archive: ScrapeArchiveRepository, scrapers: Seq[CinemaScraper] = roster) = {
    val registry = new PrometheusRegistry()
    val (oldestAge, neverContent) = CinemaContentCensus.gauges(registry)
    val stale = CinemaContentCensus.staleVenuesGauge(registry)
    val c = new CinemaContentCensus(scrapers, archive, oldestAge, neverContent, stale, Country.Poland,
      Clock.fixed(now, ZoneOffset.UTC))
    (c, oldestAge, neverContent, stale)
  }

  private def valueOf(g: io.prometheus.metrics.core.metrics.Gauge) =
    g.labelValues(Country.Poland.code).get()

  "CinemaContentCensus" should "report the longest-quiet cinema's age and the never-produced count" in {
    val (c, oldestAge, neverContent) = census(new StubArchive(healthyStamps))
    c.sample()
    valueOf(oldestAge) shouldBe (40 * 86400).toDouble
    valueOf(neverContent) shouldBe 1.0
  }

  // Split for the same reason CinemaScrapeCensus splits its pair: a cinema with no
  // content has no age, so folding it in would either read as 0s — hiding the worst
  // case behind a healthy maximum — or need a sentinel that flattens the chart.
  it should "keep a never-produced cinema out of the age gauge rather than calling it 0s old" in {
    val (c, oldestAge, neverContent) = census(new StubArchive(Map(never.displayName -> None)))
    c.sample()
    valueOf(oldestAge) shouldBe 0.0    // no age to report, not "brand new"
    valueOf(neverContent) shouldBe 3.0 // the two absent from the archive count too
  }

  it should "count a cinema absent from the archive as never having produced" in {
    val (c, _, neverContent) = census(new StubArchive(Map(producing.displayName -> Some(now))))
    c.sample()
    valueOf(neverContent) shouldBe 2.0
  }

  // THE case this metric would otherwise invert. `lastContentAt` returns an empty
  // map when its scan could not complete, precisely so a partial read is never
  // mistaken for data — publishing it here would turn a Mongo blip into "every
  // cinema in the country has gone silent".
  it should "hold its last reading when the archive read fails, not report the whole roster as silent" in {
    // ONE census whose later read fails — the production shape. A second instance
    // would re-materialize the gauges at 0 and prove nothing about the tick.
    val archive = new StubArchive(healthyStamps)
    val (c, oldestAge, neverContent) = census(archive)
    c.sample()
    val (ageBefore, neverBefore) = (valueOf(oldestAge), valueOf(neverContent))

    archive.stamps = Map.empty // the scan came back incomplete
    c.sample()

    valueOf(oldestAge) shouldBe ageBefore
    valueOf(neverContent) shouldBe neverBefore
    neverBefore should be < roster.size.toDouble // and emphatically not "all of them"
  }

  it should "read an empty archive as no cinemas when the roster is empty too" in {
    val registry = new PrometheusRegistry()
    val (oldestAge, neverContent) = CinemaContentCensus.gauges(registry)
    new CinemaContentCensus(Seq.empty, new StubArchive(Map.empty), oldestAge, neverContent,
      CinemaContentCensus.staleVenuesGauge(registry), Country.Poland, Clock.fixed(now, ZoneOffset.UTC)).sample()
    valueOf(neverContent) shouldBe 0.0
  }

  // THE STALE-VENUE COUNT. The oldest age is one venue — the roster's worst — so it cannot say
  // whether a second venue has gone quiet behind it. DE read 1,383h on 2026-09-24: one venue
  // quiet for eight weeks hides any number of others quiet for one.
  "the stale-venue count" should "count every venue quiet past a week, not only the quietest" in {
    val justQuiet = CinemaCityArkadia                                // a week and an hour
    val stamps = Map(
      producing.displayName -> Some(now.minusSeconds(3600)),
      quiet.displayName     -> Some(now.minusSeconds(40 * 86400)),
      justQuiet.displayName -> Some(now.minusSeconds(7 * 86400 + 3600)))
    val (c, oldestAge, _, stale) = censusWithStale(new StubArchive(stamps))
    c.sample()
    valueOf(oldestAge) shouldBe (40 * 86400).toDouble                // unchanged: still the worst one
    valueOf(stale) shouldBe 2.0                                      // …which is not the only one
  }

  it should "leave out a venue that has never produced — its own gauge counts it, and it has no age" in {
    val (c, _, neverContent, stale) = censusWithStale(new StubArchive(healthyStamps))
    c.sample()
    valueOf(stale) shouldBe 1.0                                      // the 40-day one only
    valueOf(neverContent) shouldBe 1.0
  }

  it should "not count a venue quiet for less than a week — a repertory programme gap" in {
    val (c, _, _, stale) = censusWithStale(new StubArchive(Map(
      producing.displayName -> Some(now.minusSeconds(6 * 86400)),
      quiet.displayName     -> Some(now.minusSeconds(7 * 86400)))))  // exactly a week: not past it
    c.sample()
    valueOf(stale) shouldBe 0.0
  }

  // Its alert compares today with yesterday's LOWEST reading, so a zero published at boot would
  // read as "yesterday none were stale" after every restart and page on a healthy roster.
  it should "publish nothing until its first reading, and hold the last one when the archive read fails" in {
    val archive = new StubArchive(healthyStamps)
    val (c, _, _, stale) = censusWithStale(archive)
    stale.collect().getDataPoints.size shouldBe 0
    c.sample()
    archive.stamps = Map.empty
    c.sample()
    valueOf(stale) shouldBe 1.0
  }
}
