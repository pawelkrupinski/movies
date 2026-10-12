package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaCityArkadia, CinemaCityKinepolis, CinemaCityWroclavia, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.StubCinemaScraper
import services.cinemas.common.CinemaScraper
import services.scrapes.{ArchivedScrape, ContentStamp, ScrapeArchiveRepository}

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

  private val producing = CinemaCityKinepolis   // had films this morning
  private val quiet     = CinemaCityWroclavia   // last had films 40 days ago
  private val never     = CinemaCityArkadia     // has never produced any

  private val roster = Seq(producing, quiet, never).map(new StubCinemaScraper(_))

  /** An archive that answers with exactly `stamps` — including, when empty, the
   *  read that could not be completed. */
  private class StubArchive(var stamps: Map[String, ContentStamp]) extends ScrapeArchiveRepository {
    var reads = 0
    def enabled: Boolean = true
    protected def storeSuccess(c: Cinema, city: Option[String], s: services.scrapes.SuccessfulScrape): Unit = ()
    protected def storeBarren(c: Cinema, city: Option[String], a: services.scrapes.BarrenAttempt): Unit     = ()
    def find(cinema: Cinema): Option[ArchivedScrape] = None
    def scan(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome = tools.ScanOutcome.complete
    def contentStamps(): Map[String, ContentStamp] = { reads += 1; stamps }
  }

  private val healthyStamps = Map(
    producing.displayName -> ContentStamp(Some(now.minusSeconds(3600))),
    quiet.displayName     -> ContentStamp(Some(now.minusSeconds(40 * 86400))),
    never.displayName     -> ContentStamp(Option.empty[Instant])
  )

  private def census(archive: ScrapeArchiveRepository) = {
    val (c, oldestAge, neverContent, _) = censusWithStale(archive)
    (c, oldestAge, neverContent)
  }

  private def censusWithStale(archive: ScrapeArchiveRepository, scrapers: Seq[CinemaScraper] = roster,
                              clock: Clock = Clock.fixed(now, ZoneOffset.UTC)) = {
    val (c, oldestAge, neverContent, stale, _) = censusWithAll(archive, scrapers, clock)
    (c, oldestAge, neverContent, stale)
  }

  private def censusWithAll(archive: ScrapeArchiveRepository, scrapers: Seq[CinemaScraper], clock: Clock) = {
    val registry = new PrometheusRegistry()
    val (oldestAge, neverContent) = CinemaContentCensus.gauges(registry)
    val stale       = CinemaContentCensus.staleVenuesGauge(registry)
    val notReopened = CinemaContentCensus.notReopenedGauge(registry)
    val c = new CinemaContentCensus(scrapers, archive, oldestAge, neverContent, stale, notReopened, Country.Poland, clock)
    (c, oldestAge, neverContent, stale, notReopened)
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
    val (c, oldestAge, neverContent) = census(new StubArchive(Map(never.displayName -> ContentStamp(None))))
    c.sample()
    valueOf(oldestAge) shouldBe 0.0    // no age to report, not "brand new"
    valueOf(neverContent) shouldBe 3.0 // the two absent from the archive count too
  }

  it should "count a cinema absent from the archive as never having produced" in {
    val (c, _, neverContent) = census(new StubArchive(Map(producing.displayName -> ContentStamp(Some(now)))))
    c.sample()
    valueOf(neverContent) shouldBe 2.0
  }

  // THE case this metric would otherwise invert. `contentStamps` returns an empty
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
      CinemaContentCensus.staleVenuesGauge(registry), CinemaContentCensus.notReopenedGauge(registry), Country.Poland, Clock.fixed(now, ZoneOffset.UTC)).sample()
    valueOf(neverContent) shouldBe 0.0
  }

  // THE STALE-VENUE COUNT. The oldest age is one venue — the roster's worst — so it cannot say
  // whether a second venue has gone quiet behind it. DE read 1,383h on 2026-09-24: one venue
  // quiet for eight weeks hides any number of others quiet for one.
  "the stale-venue count" should "count every venue quiet past a week, not only the quietest" in {
    val justQuiet = CinemaCityArkadia                                // a week and an hour
    val stamps = Map(
      producing.displayName -> ContentStamp(Some(now.minusSeconds(3600))),
      quiet.displayName     -> ContentStamp(Some(now.minusSeconds(40 * 86400))),
      justQuiet.displayName -> ContentStamp(Some(now.minusSeconds(7 * 86400 + 3600))))
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
      producing.displayName -> ContentStamp(Some(now.minusSeconds(6 * 86400))),
      quiet.displayName     -> ContentStamp(Some(now.minusSeconds(7 * 86400))))))  // exactly a week: not past it
    c.sample()
    valueOf(stale) shouldBe 0.0
  }

  // 2026-09-27..29: 17 US drive-ins closed for the season held CinemaContentStaleVenuesGrowing
  // firing for 44 hours. Their Flicks pages parse and SAY there are no times — that is the venue
  // talking, not a parser that stopped matching. One quiet for as long with nothing vouching for
  // its silence still counts.
  it should "leave out a venue whose source last said it has no schedule, but keep one nothing vouches for" in {
    val (c, oldestAge, _, stale) = censusWithStale(new StubArchive(Map(
      producing.displayName -> ContentStamp(Some(now.minusSeconds(3600))),
      quiet.displayName     -> ContentStamp(Some(now.minusSeconds(40 * 86400)), noScheduleListed = true),
      never.displayName     -> ContentStamp(Some(now.minusSeconds(30 * 86400))))))
    c.sample()
    valueOf(stale) shouldBe 1.0                                      // the unvouched 30-day one only
    valueOf(oldestAge) shouldBe (40 * 86400).toDouble                // still visible as the quietest
  }

  // An open-air screen dark since September is not a parser gone quiet: off its season it counts in
  // none of the three gauges. In season it is held to the same week as every other venue.
  "a seasonal venue" should "count in no gauge off its season, and like any venue in it" in {
    val rooftop = models.RooftopFilmClubStratfordRoofEast           // UK window: May..September
    val stamps  = (at: Instant) => Map(
      producing.displayName -> ContentStamp(Some(at.minusSeconds(3600))),
      rooftop.displayName   -> ContentStamp(Some(at.minusSeconds(60 * 86400))))
    val scrapers = Seq(producing, rooftop).map(new StubCinemaScraper(_))

    val december = Instant.parse("2026-12-01T12:00:00Z")
    val (winter, winterAge, winterNever, winterStale) =
      censusWithStale(new StubArchive(stamps(december)), scrapers, Clock.fixed(december, ZoneOffset.UTC))
    winter.sample()
    valueOf(winterStale) shouldBe 0.0
    valueOf(winterAge) shouldBe 3600.0
    valueOf(winterNever) shouldBe 0.0

    val (summer, _, _, summerStale) = censusWithStale(new StubArchive(stamps(now)), scrapers)  // 2026-08-03
    summer.sample()
    valueOf(summerStale) shouldBe 1.0
  }

  // On 1 May every open-air screen re-enters its season with its last content from September, and
  // most list nothing until June. Counted from either date, they would all turn stale together —
  // on 1 May, or a week later — and page `CinemaContentStaleVenuesGrowing` every spring. So a
  // seasonal venue counts only once it has listed this season; from then it is held to the same
  // week as any venue.
  it should "count only once it has listed this season, then like any venue" in {
    val rooftop  = models.RooftopFilmClubStratfordRoofEast          // UK window: May..September
    val scrapers = Seq(new StubCinemaScraper(rooftop))
    def readingAt(at: String, lastContent: String) = {
      val clock  = Clock.fixed(Instant.parse(at), ZoneOffset.UTC)
      val stamps = Map(rooftop.displayName -> ContentStamp(Some(Instant.parse(lastContent))))
      val (c, age, _, stale) = censusWithStale(new StubArchive(stamps), scrapers, clock)
      c.sample()
      (valueOf(age), valueOf(stale))
    }

    readingAt("2027-05-01T12:00:00Z", "2026-09-20T18:00:00Z") shouldBe ((0.0, 0.0))     // not reopened yet
    readingAt("2027-05-20T00:00:00Z", "2026-09-20T18:00:00Z") shouldBe ((0.0, 0.0))     // still not: no spring step
    readingAt("2027-06-12T00:00:00Z", "2027-06-02T00:00:00Z") shouldBe ((10 * 86400).toDouble, 1.0)  // listed, then quiet
  }

  // One that never reopens — a site rebuilt over winter, parsing to nothing — must not stay hidden all
  // season. But every seasonal venue in a country shares one window, so counting the never-reopened
  // ones as stale from the halfway point stepped the stale gauge on one day a year (~16 July) and
  // paged `CinemaContentStaleVenuesGrowing` as a parser break. They get a gauge of their own instead.
  it should "count one that has listed nothing by its season's halfway point as not reopened, never as stale" in {
    val rooftop  = models.RooftopFilmClubStratfordRoofEast          // UK window: May..September, halfway 2027-07-16
    val scrapers = Seq(new StubCinemaScraper(rooftop))
    val stamps   = Map(rooftop.displayName -> ContentStamp(Some(Instant.parse("2026-09-20T18:00:00Z"))))
    def readingAt(at: String) = {
      val (c, _, _, stale, notReopened) =
        censusWithAll(new StubArchive(stamps), scrapers, Clock.fixed(Instant.parse(at), ZoneOffset.UTC))
      c.sample()
      (valueOf(stale), valueOf(notReopened))
    }
    readingAt("2027-07-10T00:00:00Z") shouldBe ((0.0, 0.0))
    readingAt("2027-07-20T00:00:00Z") shouldBe ((0.0, 1.0))
  }

  it should "leave a seasonal venue that has never listed anything to the never-content gauge alone" in {
    val rooftop = models.RooftopFilmClubStratfordRoofEast
    val (c, _, neverContent, _, notReopened) = censusWithAll(
      new StubArchive(Map(rooftop.displayName -> ContentStamp(None))), Seq(new StubCinemaScraper(rooftop)),
      Clock.fixed(Instant.parse("2027-07-20T00:00:00Z"), ZoneOffset.UTC))
    c.sample()
    (valueOf(neverContent), valueOf(notReopened)) shouldBe ((1.0, 0.0))
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

  // It read every venue's stamp every 30 minutes — 5,000 documents a reading on the US — to learn what each scrape
  // had already told the worker as it was archived.
  "the content census" should "follow each archived scrape without reading the archive again" in {
    val archive = new StubArchive(healthyStamps)
    val clock   = new tools.MutableClock(now)
    val (c, oldestAge, neverContent, stale) = censusWithStale(archive, clock = clock)
    val filing  = c.watching(new services.scrapes.InMemoryScrapeArchiveRepository)
    c.sample()
    valueOf(stale) shouldBe 1.0                                      // the 40-day-quiet venue

    // The quiet venue lists films again, and the never-produced one produces its first.
    filing.record(services.scrapes.ScrapeAttempt(quiet, Cinema.cityOf(quiet), now, listingComplete = true, films = Seq(film)))
    filing.record(services.scrapes.ScrapeAttempt(never, Cinema.cityOf(never), now, listingComplete = true, films = Seq(film)))
    clock.advance(java.time.Duration.ofHours(2))
    c.sample()

    archive.reads shouldBe 1
    valueOf(stale) shouldBe 0.0
    valueOf(neverContent) shouldBe 0.0
    valueOf(oldestAge) shouldBe (3 * 3600).toDouble                  // the one that produced an hour before `now`
  }

  it should "take an empty scrape's word that the venue lists no schedule, but not over newer content" in {
    val archive = new StubArchive(Map(
      producing.displayName -> ContentStamp(Some(now.minusSeconds(3600))),
      quiet.displayName     -> ContentStamp(Some(now.minusSeconds(40 * 86400)))))
    val (c, _, _, stale) = censusWithStale(archive)
    val filing = c.watching(new services.scrapes.InMemoryScrapeArchiveRepository)
    c.sample()
    valueOf(stale) shouldBe 1.0
    filing.record(services.scrapes.ScrapeAttempt(quiet, Cinema.cityOf(quiet), now, listingComplete = true, films = Seq.empty,
      noScheduleListed = true))
    // An empty attempt older than the venue's newest content changes nothing.
    filing.record(services.scrapes.ScrapeAttempt(producing, Cinema.cityOf(producing), now.minusSeconds(7200), listingComplete = true,
      films = Seq.empty, noScheduleListed = true))
    c.sample()
    valueOf(stale) shouldBe 0.0                                      // closed for the season, on its own word
    archive.reads shouldBe 1
  }

  it should "read again until a read succeeds, then once a day" in {
    val archive = new StubArchive(Map.empty)                         // the first read fails
    val clock   = new tools.MutableClock(now)
    val (c, _, _, stale) = censusWithStale(archive, clock = clock)
    c.sample()
    stale.collect().getDataPoints.size shouldBe 0
    archive.stamps = healthyStamps
    c.sample()
    valueOf(stale) shouldBe 1.0
    archive.reads shouldBe 2
    clock.advance(java.time.Duration.ofHours(23)); c.sample()
    archive.reads shouldBe 2
    clock.advance(java.time.Duration.ofHours(1)); c.sample()
    archive.reads shouldBe 3
  }

  private val film = models.CinemaMovie(models.Movie("Film"), quiet, None, None, None, Nil, Nil, Nil)
}
