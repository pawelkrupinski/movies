package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Country, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CacheKey, MovieCacheReader, StoredMovieRecord}
import services.tasks.RatingTasks

import java.time.Instant

/**
 * Locks the "never run" rating backlog the worker exposes for the Grafana panel:
 * per site, how many TMDB-resolved films have never had that rating run, and how
 * long the oldest has waited. The eligibility (imdb needs an imdbId; rt/mc/fw a
 * tmdbId) and the "already run = a freshness stamp exists" rule are the load-bearing
 * behaviour, so they're asserted directly.
 */
class RatingRunCensusSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-06-23T20:00:00Z")

  private def key(title: String, year: Int) = CacheKey(title, Some(year))

  // Resolved an hour ago, both ids; imdb + fw have already run, rt + mc never have.
  private val resolvedRated = key("Resolved Rated", 2025)
  private val recordRated   = MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"))

  // Resolved 100s ago, tmdbId only (no imdbId): rt/mc/fw eligible & never run; imdb not eligible.
  private val resolvedBare = key("Resolved Bare", 2024)
  private val recordBare   = MovieRecord(tmdbId = Some(2))

  private def freshness(): InMemoryFreshnessStore = {
    val f = new InMemoryFreshnessStore
    f.markFresh(RatingTasks.tmdbResolvedAtKey(1), FreshnessKind.TmdbResolve, now.minusSeconds(3600))
    f.markFresh(RatingTasks.tmdbResolvedAtKey(2), FreshnessKind.TmdbResolve, now.minusSeconds(100))
    // resolvedRated has already had imdb + fw run; rt + mc deliberately left un-run.
    f.markFresh(RatingTasks.dedupKey(FreshnessKind.ImdbRating, resolvedRated, Some(1)),    FreshnessKind.ImdbRating,    now.minusSeconds(1800))
    f.markFresh(RatingTasks.dedupKey(FreshnessKind.FilmwebRating, resolvedRated, Some(1)),  FreshnessKind.FilmwebRating, now.minusSeconds(1800))
    f
  }

  private val entries: Seq[(CacheKey, MovieRecord)] =
    Seq(resolvedRated -> recordRated, resolvedBare -> recordBare)

  "census" should "count a site as never-run only for eligible rows lacking a stamp" in {
    val stats = RatingRunCensus.census(entries, freshness().lastFetchedAt, now)

    // imdb: only resolvedRated is eligible, and it has already run → no backlog.
    stats.get(FreshnessKind.ImdbRating.label) shouldBe None

    // rt/mc: both rows eligible, neither ran → count 2, oldest = resolvedRated's 3600s.
    stats(FreshnessKind.RtRating.label).count            shouldBe 2
    stats(FreshnessKind.RtRating.label).oldestAgeSeconds shouldBe 3600.0
    stats(FreshnessKind.McRating.label).count            shouldBe 2
    stats(FreshnessKind.McRating.label).oldestAgeSeconds shouldBe 3600.0

    // fw: resolvedRated ran, only resolvedBare is outstanding → count 1, oldest = 100s.
    stats(FreshnessKind.FilmwebRating.label).count            shouldBe 1
    stats(FreshnessKind.FilmwebRating.label).oldestAgeSeconds shouldBe 100.0
  }

  it should "report zero backlog when every eligible site has run" in {
    val f = freshness()
    f.markFresh(RatingTasks.dedupKey(FreshnessKind.RtRating, resolvedRated, Some(1)), FreshnessKind.RtRating, now)
    f.markFresh(RatingTasks.dedupKey(FreshnessKind.McRating, resolvedRated, Some(1)), FreshnessKind.McRating, now)
    val stats = RatingRunCensus.census(Seq(resolvedRated -> recordRated), f.lastFetchedAt, now)
    stats shouldBe empty
  }

  "RatingRunCensus.sample" should "publish per-site backlog gauges onto the registry, seeded at 0" in {
    val registry           = new PrometheusRegistry()
    val (notRun, oldestAge) = RatingRunCensus.gauges(registry)
    val census   = new RatingRunCensus(cacheOf(entries), freshness(), notRun, oldestAge, Country.Poland, java.time.Clock.fixed(now, java.time.ZoneOffset.UTC))

    // Before sampling, every site series exists at 0 (no Grafana gaps).
    val seeded = PrometheusExposition.render(registry)
    Seq("imdb", "fw", "rt", "mc")
      .foreach(site => gauge(seeded, RatingRunCensus.NotRunName, site) shouldBe Some(0.0))

    census.sample()
    val text = PrometheusExposition.render(registry)
    gauge(text, RatingRunCensus.NotRunName, "rt") shouldBe Some(2.0)
    gauge(text, RatingRunCensus.NotRunName, "fw") shouldBe Some(1.0)
    gauge(text, RatingRunCensus.NotRunName, "imdb") shouldBe Some(0.0)
    gauge(text, RatingRunCensus.OldestAgeName, "rt") shouldBe Some(3600.0)
    gauge(text, RatingRunCensus.OldestAgeName, "fw") shouldBe Some(100.0)
  }

  it should "NOT publish a Filmweb series at all for a non-Filmweb country (UK)" in {
    val registry            = new PrometheusRegistry()
    val (notRun, oldestAge) = RatingRunCensus.gauges(registry)
    val census = new RatingRunCensus(cacheOf(entries), freshness(), notRun, oldestAge, Country.UnitedKingdom, java.time.Clock.fixed(now, java.time.ZoneOffset.UTC))

    census.sample()
    val text = PrometheusExposition.render(registry)
    // Global sites still reported for the UK deployment...
    ukGauge(text, RatingRunCensus.NotRunName, "rt") shouldBe Some(2.0)
    ukGauge(text, RatingRunCensus.NotRunName, "imdb") shouldBe Some(0.0)
    // ...but Filmweb never runs in the UK, so its series is absent (not a stuck 0/backlog).
    ukGauge(text, RatingRunCensus.NotRunName, "fw") shouldBe None
    ukGauge(text, RatingRunCensus.OldestAgeName, "fw") shouldBe None
  }

  // The shape panel-56 was structurally blind to: IMDb eligibility is imdbId-based
  // (RatingSources), not tmdbId-based, so a row with an imdbId and no tmdbId joins
  // the backlog while the tmdbId-keyed TMDB-resolve stamp the age was measured from
  // cannot exist. The age then fell to `getOrElse(0.0)` and `max(prev, 0.0)` kept it
  // there, so the panel showed a flat 0 next to a non-zero count — exactly the state
  // the UK carried on 2026-07-28 (count 2, age 0, unchanged for six hours) while its
  // help text promised a climbing diagonal.
  private val imdbOnly       = key("Imdb Only", 2026)
  private val recordImdbOnly = MovieRecord(imdbId = Some("tt9"))

  "a backlog row with no TMDB resolve stamp" should "still age from when the census first saw it" in {
    val registry            = new PrometheusRegistry()
    val (notRun, oldestAge) = RatingRunCensus.gauges(registry)
    val clock               = new MovingClock(now)
    val census = new RatingRunCensus(
      cacheOf(Seq(imdbOnly -> recordImdbOnly)), freshness(), notRun, oldestAge, Country.Poland, clock)

    census.sample()
    gauge(PrometheusExposition.render(registry), RatingRunCensus.NotRunName, "imdb")    shouldBe Some(1.0)
    withClue("the first sighting IS the clock start — no wait accrued yet: ")(
      gauge(PrometheusExposition.render(registry), RatingRunCensus.OldestAgeName, "imdb") shouldBe Some(0.0))

    clock.advance(600)
    census.sample()
    val text = PrometheusExposition.render(registry)
    gauge(text, RatingRunCensus.NotRunName, "imdb") shouldBe Some(1.0)
    withClue("the age must climb with the wait, not sit at a flat 0: ")(
      gauge(text, RatingRunCensus.OldestAgeName, "imdb") shouldBe Some(600.0))
  }

  it should "forget its sighting once the rating runs, so a relapse starts a fresh clock" in {
    val registry            = new PrometheusRegistry()
    val (notRun, oldestAge) = RatingRunCensus.gauges(registry)
    val clock               = new MovingClock(now)
    val f                   = freshness()
    val census = new RatingRunCensus(
      cacheOf(Seq(imdbOnly -> recordImdbOnly)), f, notRun, oldestAge, Country.Poland, clock)

    val stamp = RatingTasks.dedupKey(FreshnessKind.ImdbRating, imdbOnly, None)

    census.sample()
    clock.advance(600)
    // The rating runs — the row leaves the backlog, and its sighting must go with it.
    f.markFresh(stamp, FreshnessKind.ImdbRating, clock.instant())
    census.sample()
    gauge(PrometheusExposition.render(registry), RatingRunCensus.NotRunName, "imdb") shouldBe Some(0.0)

    // A merge/re-key drops the stamp and the row relapses into the backlog. The clock
    // starts over from the relapse rather than resuming the abandoned 600s — which is
    // also what stops a drained row's sighting from being retained forever.
    clock.advance(60)
    f.invalidate(stamp)
    census.sample()
    clock.advance(30)
    census.sample()
    val relapsed = PrometheusExposition.render(registry)
    gauge(relapsed, RatingRunCensus.NotRunName, "imdb") shouldBe Some(1.0)
    withClue("the relapse must time from itself, not from the first sighting 690s ago: ")(
      gauge(relapsed, RatingRunCensus.OldestAgeName, "imdb") shouldBe Some(30.0))
  }

  // ── helpers ────────────────────────────────────────────────────────────────
  private class MovingClock(start: Instant) extends java.time.Clock {
    private var at: Instant                                  = start
    def advance(seconds: Long): Unit                         = at = at.plusSeconds(seconds)
    def instant(): Instant                                   = at
    def getZone: java.time.ZoneId                            = java.time.ZoneOffset.UTC
    override def withZone(zone: java.time.ZoneId): java.time.Clock = this
  }

  private def cacheOf(rows: Seq[(CacheKey, MovieRecord)]): MovieCacheReader = new MovieCacheReader {
    def hasResolvedSiblingByTitle(rawTitle: String): Boolean = false
    def snapshot(): Seq[StoredMovieRecord]                   = Nil
    def lastModified: Instant                                = Instant.EPOCH
    private[services] def keyOf(title: String, year: Option[Int]): CacheKey = CacheKey(title, year)
    private[services] def canonicalKeyFor(k: CacheKey): Option[CacheKey]    = Some(k)
    private[services] def get(k: CacheKey): Option[MovieRecord]             = rows.find(_._1 == k).map(_._2)
    private[services] def isNegative(k: CacheKey): Boolean                  = false
    private[services] def entries: Seq[(CacheKey, MovieRecord)]             = rows
  }

  private def gauge(text: String, name: String, site: String): Option[Double] =
    PrometheusExposition.sample(text, name, s"""country="pl",site="$site"""")

  private def ukGauge(text: String, name: String, site: String): Option[Double] =
    PrometheusExposition.sample(text, name, s"""country="uk",site="$site"""")
}
