package services.enrichment

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import tools.GetOnlyHttpFetch

/**
 * Tests for `OmdbBackfill` — the OMDb fallback that fills the three IMDb-keyed
 * ratings (imdbRating / rottenTomatoes / metascore) a row is still MISSING,
 * never overriding a value a canonical source already supplied.
 *
 * OMDb HTTP is stubbed via a canned JSON body keyed off the `?i=<imdbId>` URL.
 */
class OmdbBackfillSpec extends AnyFlatSpec with Matchers {

  private val FullBody =
    """{"imdbRating":"7.5","Metascore":"72","Ratings":[
       |  {"Source":"Rotten Tomatoes","Value":"85%"},
       |  {"Source":"Metacritic","Value":"72/100"}
       |]}""".stripMargin

  /** OMDb client whose GET returns `body` for any id-bearing URL (key present). */
  private def omdbReturning(body: String): OMDbClient =
    new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = body },
      apiKey = Some("test-key")
    )

  private def cacheWith(record: MovieRecord) =
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Film", Some(2024), record))))

  // ── golden path ──────────────────────────────────────────────────────────────

  "refreshOneSync" should "fill all three ratings when the row has an imdbId and none yet" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt0133093")))
    val backfill = new OmdbBackfill(cache, omdbReturning(FullBody))

    backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))

    val e = cache.get(cache.keyOf("Film", Some(2024))).get
    e.imdbRating     shouldBe Some(7.5)
    e.rottenTomatoes shouldBe Some(85)
    e.metascore      shouldBe Some(72)
  }

  // ── never override (the load-bearing rule) ───────────────────────────────────

  it should "fill ONLY the missing field and never override an existing one" in {
    // imdbRating + metascore already set by canonical writers; only RT is missing.
    val cache = cacheWith(MovieRecord(
      imdbId     = Some("tt0133093"),
      imdbRating = Some(9.9),   // canonical IMDb value — must survive
      metascore  = Some(40)     // canonical MC value — must survive
    ))
    val backfill = new OmdbBackfill(cache, omdbReturning(FullBody))

    backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))

    val e = cache.get(cache.keyOf("Film", Some(2024))).get
    e.imdbRating     shouldBe Some(9.9)  // untouched
    e.metascore      shouldBe Some(40)   // untouched
    e.rottenTomatoes shouldBe Some(85)   // backfilled
  }

  it should "make NO write when every gap-fill the row needs is unavailable from OMDb" in {
    val repository = new InMemoryMovieRepository(Seq(("Film", Some(2024), MovieRecord(imdbId = Some("tt9999999")))))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    // OMDb returns all N/A → nothing to fill, so no write-back at all.
    val backfill = new OmdbBackfill(cache, omdbReturning("""{"imdbRating":"N/A","Metascore":"N/A","Ratings":[]}"""))

    backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))

    repository.upserts shouldBe empty
  }

  // ── eligibility / gating ─────────────────────────────────────────────────────

  it should "be a no-op (no HTTP call) when the row has no imdbId" in {
    val cache = cacheWith(MovieRecord(tmdbId = Some(42)))  // no imdbId
    val backfill = new OmdbBackfill(cache, new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("must not be called without imdbId") },
      apiKey = Some("test-key")
    ))

    noException should be thrownBy backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))
  }

  it should "be a no-op (no HTTP call) when all three ratings are already present" in {
    val cache = cacheWith(MovieRecord(
      imdbId = Some("tt0133093"), imdbRating = Some(7.0), rottenTomatoes = Some(80), metascore = Some(60)
    ))
    val backfill = new OmdbBackfill(cache, new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("must not be called when nothing missing") },
      apiKey = Some("test-key")
    ))

    noException should be thrownBy backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))
  }

  it should "be a no-op when the OMDb key is unset (feature off — no HTTP call)" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt0133093")))
    val keyless = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("feature off — no call") },
      apiKey = None
    )
    val backfill = new OmdbBackfill(cache, keyless)

    backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))

    val e = cache.get(cache.keyOf("Film", Some(2024))).get
    e.imdbRating shouldBe None
    e.rottenTomatoes shouldBe None
    e.metascore shouldBe None
  }

  it should "swallow an OMDb failure without throwing and leave the row untouched" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt0133093")))
    val failing = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("HTTP 503") },
      apiKey = Some("test-key")
    )
    val backfill = new OmdbBackfill(cache, failing)

    noException should be thrownBy backfill.refreshOneSync(cache.keyOf("Film", Some(2024)))
    cache.get(cache.keyOf("Film", Some(2024))).get.imdbRating shouldBe None
  }

  // ── full-corpus walk ─────────────────────────────────────────────────────────

  "refreshAll" should "backfill every eligible row and skip ineligible ones" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord(imdbId = Some("tt0001"))),                         // eligible
      ("B", None, MovieRecord(imdbId = Some("tt0002"), rottenTomatoes = Some(99))), // partial gap
      ("C", None, MovieRecord(tmdbId = Some(7)))                                 // no imdbId → skip
    ))
    val cache = new CaffeineMovieCache(repository)
    val backfill = new OmdbBackfill(cache, omdbReturning(FullBody))

    backfill.refreshAll()

    cache.get(cache.keyOf("A", None)).get.imdbRating     shouldBe Some(7.5)
    cache.get(cache.keyOf("B", None)).get.rottenTomatoes shouldBe Some(99) // untouched
    cache.get(cache.keyOf("B", None)).get.metascore      shouldBe Some(72) // backfilled
    cache.get(cache.keyOf("C", None)).get.imdbRating     shouldBe None     // skipped
  }
}
