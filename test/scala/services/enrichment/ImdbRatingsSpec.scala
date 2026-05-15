package services.enrichment

import services.movies.{MovieCache, MovieRepo, MovieService}

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, ImdbIdResolved, MovieRecordCreated, TmdbResolved}
import tools.HttpFetch

import scala.collection.mutable

/**
 * Tests for `ImdbRatings` — the extracted IMDb-stage class. Covers the
 * per-row refresh, the bus listener, and the periodic walk.
 */
class ImdbRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Test scaffolding ────────────────────────────────────────────────────────

  private class FakeRepo(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty)
      extends MovieRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), MovieRecord]
    val upserts = mutable.ListBuffer.empty[(String, Option[Int], MovieRecord)]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], MovieRecord)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = {
      store.put((t, y), e); upserts.append((t, y, e))
    }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove((t, y)); () }
  }

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
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating, metascore = None,
               originalTitle = None, tmdbId = Some(42))

  // ── refreshOneSync ──────────────────────────────────────────────────────────

  "refreshOneSync" should "fetch the rating and write it back when it differs from the cached value" in {
    val repo  = new FakeRepo(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new MovieCache(repo)
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4)))

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))

    cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.imdbRating) shouldBe Some(7.4)
  }

  it should "not write back when the fetched rating equals the cached value (idempotent)" in {
    val repo  = new FakeRepo(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(7.4)))))
    val cache = new MovieCache(repo)
    repo.upserts.clear()
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4)))

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))

    // No new upserts — the value hadn't changed.
    repo.upserts shouldBe empty
  }

  it should "swallow IMDb client failures (network blip, HTML challenge) without throwing" in {
    val repo  = new FakeRepo(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new MovieCache(repo)
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
    val tmdbOnly = MovieRecord(imdbId = None, imdbRating = None, metascore = None,
                               originalTitle = None, tmdbId = Some(42))
    val repo  = new FakeRepo(Seq(("Foo", Some(2024), tmdbOnly)))
    val cache = new MovieCache(repo)
    // ImdbClient must never be invoked — the stub throws on any request.
    val ratings = new ImdbRatings(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
      override def post(url: String, body: String, contentType: String): String = get(url)
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache   = new MovieCache(new FakeRepo())
    val ratings = new ImdbRatings(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
      override def post(url: String, body: String, contentType: String): String = get(url)
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "walk every cached row and update each rating that changed" in {
    val repo = new FakeRepo(Seq(
      ("A", None, mkEnrichment("tt1", rating = Some(5.0))),
      ("B", None, mkEnrichment("tt2", rating = Some(6.0))),
      ("C", None, mkEnrichment("tt3", rating = Some(7.0)))
    ))
    val cache = new MovieCache(repo)
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

  // ── Event listener ──────────────────────────────────────────────────────────

  "onTmdbResolved" should "trigger an IMDb refresh for the resolved row when subscribed on the bus" in {
    val bus   = new EventBus()
    val repo  = new FakeRepo(Seq(("Foo", Some(2024), mkEnrichment("tt1", rating = Some(5.0)))))
    val cache = new MovieCache(repo)
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt1" -> 7.4)))
    bus.subscribe(ratings.onTmdbResolved)

    bus.publish(TmdbResolved("Foo", Some(2024), "tt1"))

    // Schedule is async — give the worker pool a beat.
    eventually(cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.imdbRating) shouldBe Some(7.4))
  }

  it should "ignore events of other types (PartialFunction.applyOrElse)" in {
    val bus   = new EventBus()
    val cache = new MovieCache(new FakeRepo())
    // Stub throws if hit — the test asserts it never is.
    val ratings = new ImdbRatings(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
      override def post(url: String, body: String, contentType: String): String = get(url)
    }))
    bus.subscribe(ratings.onTmdbResolved)

    noException should be thrownBy bus.publish(MovieRecordCreated("Anything", None))
  }

  // ── onImdbIdResolved — rating refresh once ImdbIdResolver finds the id ────

  "onImdbIdResolved" should "refresh the rating for the resolved row" in {
    val bus = new EventBus()
    val resolved = mkEnrichment("tt12345", rating = None)
    val cache = new MovieCache(new FakeRepo(Seq(("Resolved", Some(2025), resolved))))
    val ratings = new ImdbRatings(cache, imdbStub(Map("tt12345" -> 8.4)))
    bus.subscribe(ratings.onImdbIdResolved)

    bus.publish(ImdbIdResolved("Resolved", Some(2025), "tt12345"))

    eventually(cache.get(cache.keyOf("Resolved", Some(2025))).flatMap(_.imdbRating) shouldBe Some(8.4))
  }

  // Tiny polling helper — refreshOneSync runs sync but `onTmdbResolved`
  // dispatches via `schedule` to ImdbRatings's worker pool.
  private def eventually(check: => org.scalatest.Assertion): org.scalatest.Assertion = {
    val deadline = System.currentTimeMillis() + 2000
    var last: Throwable = null
    while (System.currentTimeMillis() < deadline) {
      try return check
      catch { case t: Throwable => last = t; Thread.sleep(20) }
    }
    throw last
  }
}
