package services.movies

import services.enrichment.{FilmwebClient, ImdbClient, MetacriticClient, RottenTomatoesClient}

import clients.TmdbClient
import models.{MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.RoutingHttpFetch

/**
 * Tests for `MovieService.reEnrichSync`.
 *
 * Design contract:
 *   - Re-enrichment always re-resolves via TMDB title search. It is NOT
 *     anchored on the existing row's imdbId.
 *   - The cinema's `releaseYear` is unreliable (often the scheduling year, not
 *     the film's production year), so the year-less fallback inside
 *     `tmdb.search` is the correction path for ambiguous Polish titles like
 *     "Powrót do przyszłości" (anniversary screening of Back to the Future):
 *     year-scoped search returns no exact-title match for the cinema's year,
 *     fallback returns the famous film, which is what the cinema actually
 *     screens.
 *   - A transient TMDB failure must NOT blank an existing row — we invalidate
 *     only AFTER the new TMDB hit is in hand.
 */
class MovieServiceReEnrichSpec extends AnyFlatSpec with Matchers {

  private val deadFetch        = RoutingHttpFetch.dead("unused")
  private def deadFilmweb()    = new FilmwebClient(http = deadFetch)
  private def deadImdb()       = new ImdbClient(http = deadFetch)
  private def deadMetacritic() = new MetacriticClient(http = deadFetch)
  private def deadRt()         = new RottenTomatoesClient(http = deadFetch)

  private def mkEnrichment(imdbId: String, orig: Option[String] = None): MovieRecord =
    MovieRecord(
      imdbId = Some(imdbId),
      tmdbId = Some(42),
      data   = orig.map(o => Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some(o)))).getOrElse(Map.empty)
    )

  // ── Setup: TMDB index where the Polish title "Powrót do przyszłości"
  // ── resolves to "Back to the Future" (1985). The cinema reports year=2026
  // ── (a 40th-anniversary screening), so year-scoped search returns nothing
  // ── and the year-less fallback inside `tmdb.search` is what produces the
  // ── correct famous-film answer.
  private val YearScopedEmpty = """{"results":[]}"""
  private val YearlessHasBttF =
    """{"results":[
      |  {"id":105, "title":"Powrót do przyszłości", "original_title":"Back to the Future",
      |   "release_date":"1985-07-03", "popularity":80.0}
      |]}""".stripMargin
  private val BttFExternalIds = """{"id":105, "imdb_id":"tt0088763"}"""

  private def tmdbWithYearFallback(): RoutingHttpFetch = new RoutingHttpFetch(Map(
    // TMDB returns no hits for `query=...&year=2026&primary_release_year=2026`
    // (the year filter excludes the 1985 film). The order matters: more
    // specific stubs go first in our substring search.
    "year=2026"     -> YearScopedEmpty,
    "/search/movie" -> YearlessHasBttF,
    "/external_ids" -> BttFExternalIds
  ))

  // ── Core contract ──────────────────────────────────────────────────────────

  "reEnrichSync" should "re-resolve via TMDB title search, even when an existing row pins a different imdbId" in {
    // The seeded row holds the WRONG film (e.g. a Korean SF film TMDB once
    // returned for the same Polish title before its index drifted). Re-enrich
    // should accept the new TMDB answer rather than anchoring on the stale id.
    val tmdbHttp = tmdbWithYearFallback()
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val repository     = new InMemoryMovieRepository(Seq(
      ("Powrót do przyszłości", Some(2026), mkEnrichment("tt-old-wrong-id"))
    ))
    val cache = new CaffeineMovieCache(repository)
    val service   = new MovieService(cache, new InProcessEventBus(), tmdb)

    val result = service.reEnrichSync("Powrót do przyszłości", Some(2026))

    // The new TMDB hit replaces the stale id.
    result.flatMap(_.imdbId) shouldBe Some("tt0088763")
    // And we proved it by hitting `/search/movie`. `findByImdbId` (`/find/...`)
    // is never called.
    tmdbHttp.calls.map(_._2).exists(_.contains("/search/movie"))    shouldBe true
    tmdbHttp.calls.map(_._2).exists(_.contains("/find/"))           shouldBe false
  }

  // The user-facing scenario this captures: cinema reports year=2026 for an
  // anniversary screening of a 1985 film. TMDB's year-scoped search returns
  // nothing (no 2026 film with that title), the year-less fallback returns
  // the famous film. The row gets the correct imdbId.
  it should "resolve the famous title via year-less fallback when the cinema's year is the scheduling year, not the release year" in {
    val tmdbHttp = tmdbWithYearFallback()
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val repository     = new InMemoryMovieRepository()
    val cache    = new CaffeineMovieCache(repository)
    val service      = new MovieService(cache, new InProcessEventBus(), tmdb)

    val result = service.reEnrichSync("Powrót do przyszłości", Some(2026))

    result.flatMap(_.imdbId)        shouldBe Some("tt0088763")
    result.flatMap(_.originalTitle) shouldBe Some("Back to the Future")
    // Two search calls expected — year-scoped (empty), then year-less.
    tmdbHttp.calls.map(_._2).count(_.contains("/search/movie")) shouldBe 2
  }

  it should "leave the existing row untouched when TMDB lookup fails entirely" in {
    val tmdbHttp = new RoutingHttpFetch(Map(
      // Both year-scoped and year-less return zero results.
      "/search/movie" -> """{"results":[]}"""
    ))
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val original = mkEnrichment("tt-original", orig = Some("Keep me"))
    val repository     = new InMemoryMovieRepository(Seq(("Title", Some(2024), original)))
    val cache    = new CaffeineMovieCache(repository)
    val service      = new MovieService(cache, new InProcessEventBus(), tmdb)

    val result = service.reEnrichSync("Title", Some(2024))

    // Re-enrich returns None when TMDB can't resolve, AND the existing row
    // stays in the cache (the old `invalidate-first` semantics would have
    // blanked it on TMDB miss).
    result shouldBe None
    cache.get(cache.keyOf("Title", Some(2024))) shouldBe Some(original)
  }

  it should "resolve via search for a brand-new key (no existing row)" in {
    val tmdbHttp = tmdbWithYearFallback()
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val repository     = new InMemoryMovieRepository()  // empty
    val cache    = new CaffeineMovieCache(repository)
    val service      = new MovieService(cache, new InProcessEventBus(), tmdb)

    val result = service.reEnrichSync("Powrót do przyszłości", Some(2026))

    result.flatMap(_.imdbId) shouldBe Some("tt0088763")
  }

  // ── resolveTmdbOnce(force = true) — the per-movie `/debug` button path ────────
  // Unlike `reEnrichSync` (silent — returns the record only), this forces a
  // re-resolve even of an already-resolved row and writes the TMDB-side fields
  // (tmdbId, imdbId) back through the cache — from where the EnrichmentReaper
  // re-runs the row's ratings. It's the work the worker's `ResolveTmdbHandler`
  // runs for an operator re-enrich task.

  "resolveTmdbOnce(force = true)" should "write the resolved imdbId through the cache and return true (concluded)" in {
    val tmdbHttp = tmdbWithYearFallback()
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository())
    val service  = new MovieService(cache, new InProcessEventBus(), tmdb)

    service.resolveTmdbOnce("Powrót do przyszłości", Some(2026), None, None, force = true) shouldBe true

    cache.get(cache.keyOf("Powrót do przyszłości", Some(2026))).flatMap(_.imdbId) shouldBe Some("tt0088763")
  }

  // The forced re-resolve strips the row to scraped data (dropping its scores); it
  // must kick a forced rating re-fetch so those scores come back, rather than
  // leaving the reaper's cadence gate to (wrongly) skip the still-stamped sources.
  it should "kick a forced rating re-fetch for the re-resolved row (carrying the resolved ids the sources key on)" in {
    val tmdb    = new TmdbClient(http = tmdbWithYearFallback(), apiKey = Some("stub"))
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository())
    val kicked  = scala.collection.mutable.ListBuffer.empty[(CacheKey, MovieRecord)]
    val service = new MovieService(cache, new InProcessEventBus(), tmdb,
      forceRatingRefresh = (k, r) => { kicked += ((k, r)); () })

    service.resolveTmdbOnce("Powrót do przyszłości", Some(2026), None, None, force = true) shouldBe true

    kicked should have size 1
    val (_, record) = kicked.head
    record.tmdbId shouldBe Some(105)             // the resolved film
    record.imdbId shouldBe Some("tt0088763")     // the ids the rating sources gate + key on
  }
}
