package services.movies

import services.enrichment.{FilmwebClient, ImdbClient, MetacriticClient, RottenTomatoesClient}

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.EventBus
import tools.HttpFetch

import scala.collection.mutable

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

  private class RecordingStub(routes: Map[String, String]) extends HttpFetch {
    val requested = mutable.ListBuffer.empty[String]
    override def get(url: String): String = {
      requested.append(url)
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
    }
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  private val deadFetch = new HttpFetch {
    override def get(url: String): String = throw new RuntimeException(s"unused: $url")
    override def post(url: String, body: String, contentType: String): String = get(url)
  }
  private def deadFilmweb()    = new FilmwebClient(http = deadFetch)
  private def deadImdb()       = new ImdbClient(http = deadFetch)
  private def deadMetacritic() = new MetacriticClient(http = deadFetch)
  private def deadRt()         = new RottenTomatoesClient(http = deadFetch)

  private def mkEnrichment(imdbId: String, orig: Option[String] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = None, metascore = None,
               originalTitle = orig, tmdbId = Some(42))

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

  private def tmdbWithYearFallback(): RecordingStub = new RecordingStub(Map(
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
    val repo     = new InMemoryMovieRepo(Seq(
      ("Powrót do przyszłości", Some(2026), mkEnrichment("tt-old-wrong-id"))
    ))
    val cache = new MovieCache(repo)
    val svc   = new MovieService(cache, new EventBus(), tmdb)

    val result = svc.reEnrichSync("Powrót do przyszłości", Some(2026))

    // The new TMDB hit replaces the stale id.
    result.flatMap(_.imdbId) shouldBe Some("tt0088763")
    // And we proved it by hitting `/search/movie`. `findByImdbId` (`/find/...`)
    // is never called.
    tmdbHttp.requested.exists(_.contains("/search/movie"))    shouldBe true
    tmdbHttp.requested.exists(_.contains("/find/"))           shouldBe false
  }

  // The user-facing scenario this captures: cinema reports year=2026 for an
  // anniversary screening of a 1985 film. TMDB's year-scoped search returns
  // nothing (no 2026 film with that title), the year-less fallback returns
  // the famous film. The row gets the correct imdbId.
  it should "resolve the famous title via year-less fallback when the cinema's year is the scheduling year, not the release year" in {
    val tmdbHttp = tmdbWithYearFallback()
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val repo     = new InMemoryMovieRepo()
    val cache    = new MovieCache(repo)
    val svc      = new MovieService(cache, new EventBus(), tmdb)

    val result = svc.reEnrichSync("Powrót do przyszłości", Some(2026))

    result.flatMap(_.imdbId)        shouldBe Some("tt0088763")
    result.flatMap(_.originalTitle) shouldBe Some("Back to the Future")
    // Two search calls expected — year-scoped (empty), then year-less.
    tmdbHttp.requested.count(_.contains("/search/movie")) shouldBe 2
  }

  it should "leave the existing row untouched when TMDB lookup fails entirely" in {
    val tmdbHttp = new RecordingStub(Map(
      // Both year-scoped and year-less return zero results.
      "/search/movie" -> """{"results":[]}"""
    ))
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val original = mkEnrichment("tt-original", orig = Some("Keep me"))
    val repo     = new InMemoryMovieRepo(Seq(("Title", Some(2024), original)))
    val cache    = new MovieCache(repo)
    val svc      = new MovieService(cache, new EventBus(), tmdb)

    val result = svc.reEnrichSync("Title", Some(2024))

    // Re-enrich returns None when TMDB can't resolve, AND the existing row
    // stays in the cache (the old `invalidate-first` semantics would have
    // blanked it on TMDB miss).
    result shouldBe None
    cache.get(cache.keyOf("Title", Some(2024))) shouldBe Some(original)
  }

  it should "resolve via search for a brand-new key (no existing row)" in {
    val tmdbHttp = tmdbWithYearFallback()
    val tmdb     = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val repo     = new InMemoryMovieRepo()  // empty
    val cache    = new MovieCache(repo)
    val svc      = new MovieService(cache, new EventBus(), tmdb)

    val result = svc.reEnrichSync("Powrót do przyszłości", Some(2026))

    result.flatMap(_.imdbId) shouldBe Some("tt0088763")
  }
}
