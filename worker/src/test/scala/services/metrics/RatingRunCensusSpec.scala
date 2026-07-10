package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
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
    val census   = new RatingRunCensus(cacheOf(entries), freshness(), notRun, oldestAge, "pl", java.time.Clock.fixed(now, java.time.ZoneOffset.UTC))

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

  // ── helpers ────────────────────────────────────────────────────────────────
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
}
