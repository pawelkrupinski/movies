package services.enrichment

import services.movies.{InMemoryMovieRepo, MovieCache}

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, MovieRecordCreated, TmdbResolved}
import tools.HttpFetch
import tools.Eventually.eventually

/**
 * Tests for `RottenTomatoesRatings` — the RT-score equivalent of `ImdbRatings`.
 * Covers per-row refresh, the bus listener, and the periodic walk.
 *
 * Score-page HTML is the trimmed real-RT fixture we use for the client tests
 * (JSON-LD aggregateRating block). We inline a tiny variant per scenario to
 * keep each test self-contained.
 */
class RottenTomatoesRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Test scaffolding ────────────────────────────────────────────────────────

  /** Build a movie-page HTML carrying the given Tomatometer percentage in the
   *  JSON-LD aggregateRating block — same shape the real RT page emits. */
  private def pageWithScore(score: Int): String =
    s"""<!doctype html><html><head>
       |<script type="application/ld+json">{"@type":"Movie","aggregateRating":{"@type":"AggregateRating","ratingValue":"$score"}}</script>
       |</head><body></body></html>""".stripMargin

  /** Stub HttpFetch that returns canned HTML keyed by URL. Throws for unknown
   *  URLs so a misrouted fetch surfaces loudly in tests. */
  private def httpStub(pages: Map[String, String]): HttpFetch = new HttpFetch {
    def get(url: String): String =
      pages.getOrElse(url, throw new RuntimeException(s"unstubbed URL: $url"))
  }

  private def rtClient(pages: Map[String, String]): RottenTomatoesClient =
    new RottenTomatoesClient(http = httpStub(pages))

  private def mkEnrichment(rtUrl: Option[String], score: Option[Int] = None): MovieRecord =
    MovieRecord(
      imdbId            = Some("tt0001"),
      imdbRating        = None,
      metascore         = None,
      originalTitle     = None,
      tmdbId            = Some(42),
      rottenTomatoes    = score,
      rottenTomatoesUrl = rtUrl
    )

  // ── refreshOneSync ──────────────────────────────────────────────────────────

  "refreshOneSync" should "fetch the score and write it back when it differs from the cached value" in {
    val url = "https://www.rottentomatoes.com/m/the_dark_knight"
    val repo  = new InMemoryMovieRepo(Seq(("Dark Knight", Some(2008), mkEnrichment(Some(url), score = Some(50)))))
    val cache = new MovieCache(repo)
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), rtClient(Map(url -> pageWithScore(94))))

    ratings.refreshOneSync(cache.keyOf("Dark Knight", Some(2008)))

    cache.get(cache.keyOf("Dark Knight", Some(2008))).flatMap(_.rottenTomatoes) shouldBe Some(94)
  }

  it should "not write back when the fetched score equals the cached value (idempotent)" in {
    val url = "https://www.rottentomatoes.com/m/the_dark_knight"
    val repo  = new InMemoryMovieRepo(Seq(("Dark Knight", Some(2008), mkEnrichment(Some(url), score = Some(94)))))
    val cache = new MovieCache(repo)
    repo.upserts.clear()
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), rtClient(Map(url -> pageWithScore(94))))

    ratings.refreshOneSync(cache.keyOf("Dark Knight", Some(2008)))

    repo.upserts shouldBe empty
  }

  it should "swallow RT client failures (network blip, 503, Cloudflare challenge) without throwing" in {
    val url = "https://www.rottentomatoes.com/m/foo"
    val repo  = new InMemoryMovieRepo(Seq(("Foo", Some(2024), mkEnrichment(Some(url), score = Some(50)))))
    val cache = new MovieCache(repo)
    val failing = new RottenTomatoesClient(http = new HttpFetch {
      def get(u: String): String = throw new RuntimeException("HTTP 503")
    })
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), failing)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
    // Cached score is preserved on failure — better stale than wrong.
    cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.rottenTomatoes) shouldBe Some(50)
  }

  it should "be a no-op when the row has no rottenTomatoesUrl (RT didn't know the film)" in {
    val repo  = new InMemoryMovieRepo(Seq(("Foo", Some(2024), mkEnrichment(None))))
    val cache = new MovieCache(repo)
    // RottenTomatoesClient stub throws on any fetch — the test asserts it never is.
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), new RottenTomatoesClient(http = new HttpFetch {
      def get(u: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache   = new MovieCache(new InMemoryMovieRepo())
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), new RottenTomatoesClient(http = new HttpFetch {
      def get(u: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "walk every cached row with an RT URL and update each score that changed" in {
    val urls = Map(
      "A" -> "https://www.rottentomatoes.com/m/a",
      "B" -> "https://www.rottentomatoes.com/m/b",
      "C" -> "https://www.rottentomatoes.com/m/c"
    )
    val repo = new InMemoryMovieRepo(Seq(
      ("A", None, mkEnrichment(Some(urls("A")), score = Some(50))),
      ("B", None, mkEnrichment(Some(urls("B")), score = Some(60))),
      ("C", None, mkEnrichment(Some(urls("C")), score = Some(70)))
    ))
    val cache = new MovieCache(repo)
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), rtClient(Map(
      urls("A") -> pageWithScore(74),  // changed
      urls("B") -> pageWithScore(60),  // unchanged
      urls("C") -> pageWithScore(81)   // changed
    )))

    ratings.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.rottenTomatoes) shouldBe Some(74)
    cache.get(cache.keyOf("B", None)).flatMap(_.rottenTomatoes) shouldBe Some(60)
    cache.get(cache.keyOf("C", None)).flatMap(_.rottenTomatoes) shouldBe Some(81)
  }

  it should "skip rows without an RT URL (no GET issued, no exception)" in {
    val urlA = "https://www.rottentomatoes.com/m/a"
    val repo = new InMemoryMovieRepo(Seq(
      ("A", None, mkEnrichment(Some(urlA))),
      // No RT URL — must not be fetched.
      ("B", None, mkEnrichment(None))
    ))
    val cache = new MovieCache(repo)
    // Stub only knows A; if the walk tries to fetch B-related anything, it throws.
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), rtClient(Map(urlA -> pageWithScore(85))))

    noException should be thrownBy ratings.refreshAll()
    cache.get(cache.keyOf("A", None)).flatMap(_.rottenTomatoes) shouldBe Some(85)
    cache.get(cache.keyOf("B", None)).flatMap(_.rottenTomatoes) shouldBe None
  }

  // ── Event listener ──────────────────────────────────────────────────────────

  "onTmdbResolved" should "trigger an RT refresh for the resolved row when subscribed on the bus" in {
    val bus = new EventBus()
    val url = "https://www.rottentomatoes.com/m/foo"
    val repo  = new InMemoryMovieRepo(Seq(("Foo", Some(2024), mkEnrichment(Some(url), score = Some(50)))))
    val cache = new MovieCache(repo)
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), rtClient(Map(url -> pageWithScore(74))))
    bus.subscribe(ratings.onTmdbResolved)

    bus.publish(TmdbResolved("Foo", Some(2024), "tt1"))

    eventually(cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.rottenTomatoes) shouldBe Some(74))
  }

  it should "ignore events of other types (PartialFunction.applyOrElse)" in {
    val bus   = new EventBus()
    val cache = new MovieCache(new InMemoryMovieRepo())
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(apiKey = None), new RottenTomatoesClient(http = new HttpFetch {
      def get(u: String): String = throw new RuntimeException("should not be called")
    }))
    bus.subscribe(ratings.onTmdbResolved)

    noException should be thrownBy bus.publish(MovieRecordCreated("Anything", None))
  }

}
