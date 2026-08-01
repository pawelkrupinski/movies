package services.enrichment

import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch

/**
 * Tests for `ImdbRatings` — the extracted IMDb-stage class. Covers the
 * per-row refresh and the full-corpus walk.
 */
class ImdbRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Test scaffolding ────────────────────────────────────────────────────────

  // Stub IMDb GraphQL response. The real ImdbClient reads
  // `data.title.ratingsSummary.aggregateRating` out of the JSON.
  private def imdbStub(ratings: Map[String, Double]): ImdbClient = {
    new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("ImdbClient uses POST")
      override def post(url: String, body: String, contentType: String): String = {
        // body contains the imdbId in the GraphQL variables — find the matching stub.
        ratings.collectFirst {
          case (id, rating) if body.contains(id) =>
            s"""{"data":{"title":{"ratingsSummary":{"aggregateRating":$rating,"voteCount":1234}}}}"""
        }.getOrElse(throw new RuntimeException(s"no stubbed rating for body: $body"))
      }
    })
  }

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating, tmdbId = Some(42))

  // ── refreshOneSync ──────────────────────────────────────────────────────────

  "refreshOneSync" should "fetch the rating and write it back when it differs from the cached value" in {
    val repository  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4)))

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))

    cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.imdbRating) shouldBe Some(7.4)
  }

  it should "not write back when the fetched rating equals the cached value (idempotent)" in {
    val repository  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(7.4)))))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4)))

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))

    // No new upserts — the value hadn't changed.
    repository.upserts shouldBe empty
  }

  // The old "swallow IMDb client failures without throwing" test lived here. Its
  // no-throw half asserted the behaviour that hid the 2026-07-30 outage; its
  // rating-preserved half is now covered by "leave the previously stored rating
  // untouched when the source is blocked" below, alongside the propagation it
  // must have. See tools.EnrichmentRead.

  it should "be a no-op when the row has no imdbId (TMDB resolved without a cross-reference)" in {
    val tmdbOnly = MovieRecord(tmdbId = Some(42))
    val repository  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), tmdbOnly)))
    val cache = new CaffeineMovieCache(repository)
    // ImdbClient must never be invoked — the stub throws on any request.
    val ratings = new ImdbRatings(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
      override def post(url: String, body: String, contentType: String): String = get(url)
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository())
    val ratings = new ImdbRatings(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
      override def post(url: String, body: String, contentType: String): String = get(url)
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "walk every cached row and update each rating that changed" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, mkEnrichment("tt1", rating = Some(5.0))),
      ("B", None, mkEnrichment("tt2", rating = Some(6.0))),
      ("C", None, mkEnrichment("tt3", rating = Some(7.0)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new ImdbRatings(cache, imdbStub(Map(
      "tt1" -> 7.4,  // changed
      "tt2" -> 6.0,  // unchanged
      "tt3" -> 8.1   // changed
    )))

    ratings.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.imdbRating) shouldBe Some(7.4)
    cache.get(cache.keyOf("B", None)).flatMap(_.imdbRating) shouldBe Some(6.0)
    cache.get(cache.keyOf("C", None)).flatMap(_.imdbRating) shouldBe Some(8.1)
  }

  it should "return a summary of the walk (walked / changed / failed) matching what it logs" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, mkEnrichment("tt1", rating = Some(5.0))),  // changes → 7.4
      ("B", None, mkEnrichment("tt2", rating = Some(6.0))),  // unchanged
      ("C", None, mkEnrichment("tt3", rating = Some(7.0))),  // changes → 8.1
      ("D", None, MovieRecord(tmdbId = Some(1)))             // no imdbId → skipped (not walked)
    ))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4, "tt2" -> 6.0, "tt3" -> 8.1)))

    val summary = ratings.refreshAll()

    summary.walked  shouldBe Some(3)   // D has no imdbId, so it isn't walked
    summary.changed shouldBe Some(2)   // A and C moved
    summary.failed  shouldBe Some(0)
    summary.message should include ("2 changed")
    summary.message should include ("0 failed")
  }

  it should "record each bulk-observed rating change into the adaptive cadence (and nothing for unchanged rows)" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord(imdbId = Some("tt1"), imdbRating = Some(5.0), tmdbId = Some(101))),
      ("B", None, MovieRecord(imdbId = Some("tt2"), imdbRating = Some(6.0), tmdbId = Some(102)))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val cadence = new services.cadence.InMemoryRatingCadenceStore
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4, "tt2" -> 6.0)),  // A moves, B unchanged
      (key, tmdbId, v) => cadence.record(services.tasks.RatingTasks.dedupKey(services.freshness.FreshnessKind.ImdbRating, key, tmdbId), v))

    ratings.refreshAll()

    cadence.statsFor("imdb|tmdb:101").flatMap(_.lastChange).map(_.to) shouldBe Some("7.4")
    cadence.statsFor("imdb|tmdb:102")                                    shouldBe None  // unchanged → no bulk record
  }

  // ── A blocked source must reach RatingHandler as a failure ─────────────────
  // The per-row path feeds RatingHandler, which is written to treat a THROWN
  // refresh as a failure: it records the attempt for /debug, skips
  // freshness.markFresh, skips the cadence report, and lets the queue retry.
  // While ImdbRatings swallowed the block into None it got Success(None)
  // instead, and booked a healthy "checked, unchanged" refresh over a dead
  // source for ~47h on 2026-07-30.

  private def blockedImdb(code: Int): ImdbClient = new ImdbClient(http = new HttpFetch {
    def get(url: String): String = throw new tools.HttpStatusException(code, "GET", "https://x", None)
    override def post(url: String, body: String, contentType: String): String =
      throw new tools.HttpStatusException(code, "POST", "https://caching.graphql.imdb.com/", None)
  })

  "refreshOneSync" should "propagate a blocked source instead of reporting 'no rating'" in {
    val repository = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new ImdbRatings(cache, blockedImdb(403))

    a[tools.HttpStatusException] should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
  }

  it should "leave the previously stored rating untouched when the source is blocked" in {
    // The freeze-don't-blank guarantee, asserted rather than assumed: a failed
    // refresh must never degrade a rating we already had.
    val repository = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new ImdbRatings(cache, blockedImdb(403))

    an[Exception] should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
    cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.imdbRating) shouldBe Some(5.0)
  }

  it should "still report None (no throw) when IMDb genuinely has no such title" in {
    val repository = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1"))))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new ImdbRatings(cache, blockedImdb(404))

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024))) shouldBe None
  }
}
