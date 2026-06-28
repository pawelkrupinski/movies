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

  it should "swallow IMDb client failures (network blip, HTML challenge) without throwing" in {
    val repository  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new CaffeineMovieCache(repository)
    val failingImdb = new ImdbClient(http = new HttpFetch {
      def get(url: String): String                                              = throw new RuntimeException("boom")
      override def post(url: String, body: String, contentType: String): String = throw new RuntimeException("boom")
    })
    val ratings = new ImdbRatings(cache, failingImdb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
    // Cached rating is preserved on failure.
    cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.imdbRating) shouldBe Some(5.0)
  }

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

}
