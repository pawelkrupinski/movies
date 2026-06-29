package services.enrichment

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import tools.GetOnlyHttpFetch

import java.net.URLDecoder

/**
 * Tests for `OmdbBackfill` — the OMDb IDENTIFIER backfill. It fills a missing
 * `imdbId` (by title search) and a missing `rottenTomatoesUrl` (OMDb tomatoURL),
 * never a rating VALUE and never overriding an identifier a canonical writer
 * already supplied. The rating numbers are left for the canonical refreshers.
 *
 * OMDb HTTP is stubbed: a `?t=` request echoes the queried title back with a
 * canned imdbID (so the title-match guard passes), a `?i=` request returns a
 * tomatoURL.
 */
class OmdbBackfillSpec extends AnyFlatSpec with Matchers {

  private val RtUrl = "https://www.rottentomatoes.com/m/the_film"

  /** Echoes the `?t=` title back (guard passes) with a canned id; serves a
   *  tomatoURL for `?i=`. Key present so the calls actually fire. */
  private def omdbStub: OMDbClient = new OMDbClient(
    http = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("?t=")) {
          val t = URLDecoder.decode(url.split("[?&]").find(_.startsWith("t=")).map(_.drop(2)).getOrElse(""), "UTF-8")
          s"""{"Title":"$t","imdbID":"tt0133093","Response":"True"}"""
        } else if (url.contains("?i="))
          s"""{"tomatoURL":"$RtUrl","Response":"True"}"""
        else """{"Response":"False"}"""
    },
    apiKey = Some("test-key")
  )

  private def cacheWith(record: MovieRecord) =
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Film", Some(2024), record))))
  private def keyOf(cache: CaffeineMovieCache) = cache.keyOf("Film", Some(2024))

  // ── golden path: recover both identifiers ─────────────────────────────────────

  "refreshOneSync" should "recover imdbId (by title) AND rottenTomatoesUrl (by id) when both are missing" in {
    val cache = cacheWith(MovieRecord())
    new OmdbBackfill(cache, omdbStub).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId            shouldBe Some("tt0133093")
    e.rottenTomatoesUrl shouldBe Some(RtUrl)
    // It must NOT have written any rating value — those stay for the canonical writers.
    e.imdbRating     shouldBe None
    e.rottenTomatoes shouldBe None
  }

  // ── never override an existing identifier ─────────────────────────────────────

  it should "fill only the missing rottenTomatoesUrl and never touch an existing imdbId" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt7654321")))  // canonical id — must survive
    new OmdbBackfill(cache, omdbStub).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId            shouldBe Some("tt7654321") // untouched (no title search)
    e.rottenTomatoesUrl shouldBe Some(RtUrl)       // backfilled via the existing id
  }

  it should "recover imdbId via title search when only the rottenTomatoesUrl is already set" in {
    val cache = cacheWith(MovieRecord(rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/existing")))
    new OmdbBackfill(cache, omdbStub).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId            shouldBe Some("tt0133093")                                  // recovered
    e.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/existing")  // untouched
  }

  it should "make NO write when OMDb can supply neither identifier" in {
    val repository = new InMemoryMovieRepository(Seq(("Film", Some(2024), MovieRecord())))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    // ?t= returns no match, ?i= unreachable (no id) → nothing to write.
    val omdb = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = """{"Response":"False"}""" },
      apiKey = Some("test-key")
    )
    new OmdbBackfill(cache, omdb).refreshOneSync(keyOf(cache))
    repository.upserts shouldBe empty
  }

  // ── eligibility / gating ─────────────────────────────────────────────────────

  it should "be a no-op (no HTTP) when the row already has both imdbId and rottenTomatoesUrl" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt0133093"), rottenTomatoesUrl = Some(RtUrl)))
    val backfill = new OmdbBackfill(cache, new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("nothing missing — no call") },
      apiKey = Some("test-key")
    ))
    noException should be thrownBy backfill.refreshOneSync(keyOf(cache))
  }

  it should "be a no-op when the OMDb key is unset (feature off — no HTTP call)" in {
    val cache = cacheWith(MovieRecord())
    val keyless = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("feature off — no call") },
      apiKey = None
    )
    new OmdbBackfill(cache, keyless).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId shouldBe None
    e.rottenTomatoesUrl shouldBe None
  }

  it should "swallow an OMDb failure without throwing and leave the row untouched" in {
    val cache = cacheWith(MovieRecord())
    val failing = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("HTTP 503") },
      apiKey = Some("test-key")
    )
    noException should be thrownBy new OmdbBackfill(cache, failing).refreshOneSync(keyOf(cache))
    cache.get(keyOf(cache)).get.imdbId shouldBe None
  }

  // ── full-corpus walk ─────────────────────────────────────────────────────────

  "refreshAll" should "recover identifiers for every eligible row and skip fully-identified ones" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord()),                                                          // both missing
      ("B", None, MovieRecord(imdbId = Some("tt0002"))),                                   // only RT url missing
      ("C", None, MovieRecord(imdbId = Some("tt0003"), rottenTomatoesUrl = Some(RtUrl)))   // fully identified → skip
    ))
    val cache = new CaffeineMovieCache(repository)
    new OmdbBackfill(cache, omdbStub).refreshAll()

    cache.get(cache.keyOf("A", None)).get.imdbId            shouldBe Some("tt0133093") // recovered
    cache.get(cache.keyOf("A", None)).get.rottenTomatoesUrl shouldBe Some(RtUrl)
    cache.get(cache.keyOf("B", None)).get.imdbId            shouldBe Some("tt0002")    // untouched
    cache.get(cache.keyOf("B", None)).get.rottenTomatoesUrl shouldBe Some(RtUrl)       // backfilled
    cache.get(cache.keyOf("C", None)).get.imdbId            shouldBe Some("tt0003")    // unchanged
  }
}
