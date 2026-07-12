package services.enrichment

import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{GetOnlyHttpFetch, HttpFetch, RealHttpFetch}

/**
 * Tests for `RottenTomatoesRatings` — the RT-score equivalent of `ImdbRatings`.
 * Covers per-row refresh and the full-corpus walk.
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
  private def httpStub(pages: Map[String, String]): HttpFetch = new GetOnlyHttpFetch {
    def get(url: String): String =
      pages.getOrElse(url, throw new RuntimeException(s"unstubbed URL: $url"))
  }

  private def rtClient(pages: Map[String, String]): RottenTomatoesClient =
    new RottenTomatoesClient(http = httpStub(pages))

  private def mkEnrichment(rtUrl: Option[String], score: Option[Int] = None): MovieRecord =
    MovieRecord(
      imdbId            = Some("tt0001"),
      tmdbId            = Some(42),
      rottenTomatoes    = score,
      rottenTomatoesUrl = rtUrl
    )

  // ── refreshOneSync ──────────────────────────────────────────────────────────

  "refreshOneSync" should "fetch the score and write it back when it differs from the cached value" in {
    val url = "https://www.rottentomatoes.com/m/the_dark_knight"
    val repository  = new InMemoryMovieRepository(Seq(("Dark Knight", Some(2008), mkEnrichment(Some(url), score = Some(50)))))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), rtClient(Map(url -> pageWithScore(94))))

    ratings.refreshOneSync(cache.keyOf("Dark Knight", Some(2008)))

    cache.get(cache.keyOf("Dark Knight", Some(2008))).flatMap(_.rottenTomatoes) shouldBe Some(94)
  }

  it should "not write back when the fetched score equals the cached value (idempotent)" in {
    val url = "https://www.rottentomatoes.com/m/the_dark_knight"
    val repository  = new InMemoryMovieRepository(Seq(("Dark Knight", Some(2008), mkEnrichment(Some(url), score = Some(94)))))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), rtClient(Map(url -> pageWithScore(94))))

    ratings.refreshOneSync(cache.keyOf("Dark Knight", Some(2008)))

    repository.upserts shouldBe empty
  }

  it should "swallow RT client failures (network blip, 503, Cloudflare challenge) without throwing" in {
    val url = "https://www.rottentomatoes.com/m/foo"
    val repository  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment(Some(url), score = Some(50)))))
    val cache = new CaffeineMovieCache(repository)
    val failing = new RottenTomatoesClient(http = new GetOnlyHttpFetch {
      def get(u: String): String = throw new RuntimeException("HTTP 503")
    })
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), failing)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
    // Cached score is preserved on failure — better stale than wrong.
    cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.rottenTomatoes) shouldBe Some(50)
  }

  it should "be a no-op when the row has no rottenTomatoesUrl (RT didn't know the film)" in {
    val repository  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment(None))))
    val cache = new CaffeineMovieCache(repository)
    // RottenTomatoesClient stub throws on any fetch — the test asserts it never is.
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), new RottenTomatoesClient(http = new GetOnlyHttpFetch {
      def get(u: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository())
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), new RottenTomatoesClient(http = new GetOnlyHttpFetch {
      def get(u: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  // ── single-fetch persistence (no confirmation deadband) ──────────────────────

  // The confirmation deadband is gone: any successfully-fetched score is written
  // on the FIRST fetch. Previously a film on the base cadence needed the same
  // value confirmed twice before it committed, and since the pending-confirmation
  // map was in-memory (reset on every worker restart) those films never committed
  // a score at all. This is the regression that fix.
  "refreshOneSync" should "persist a changed Tomatometer on the very first fetch (no confirmation gate)" in {
    val url        = "https://www.rottentomatoes.com/m/x"
    val repository = new InMemoryMovieRepository(Seq(("X", Some(2026), mkEnrichment(Some(url), score = Some(67)))))
    val cache      = new CaffeineMovieCache(repository)
    val ratings    = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None),
      rtClient(Map(url -> pageWithScore(66))))
    val key = cache.keyOf("X", Some(2026))

    val reported = ratings.refreshOneSync(key)                  // one fetch — 66

    reported shouldBe Some("66%")
    cache.get(key).flatMap(_.rottenTomatoes) shouldBe Some(66)  // written immediately
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "walk every cached row with an RT URL and update each score that changed" in {
    val urls = Map(
      "A" -> "https://www.rottentomatoes.com/m/a",
      "B" -> "https://www.rottentomatoes.com/m/b",
      "C" -> "https://www.rottentomatoes.com/m/c"
    )
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, mkEnrichment(Some(urls("A")), score = Some(50))),
      ("B", None, mkEnrichment(Some(urls("B")), score = Some(60))),
      ("C", None, mkEnrichment(Some(urls("C")), score = Some(70)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), rtClient(Map(
      urls("A") -> pageWithScore(74),  // changed
      urls("B") -> pageWithScore(60),  // unchanged
      urls("C") -> pageWithScore(81)   // changed
    )))

    ratings.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.rottenTomatoes) shouldBe Some(74)
    cache.get(cache.keyOf("B", None)).flatMap(_.rottenTomatoes) shouldBe Some(60)
    cache.get(cache.keyOf("C", None)).flatMap(_.rottenTomatoes) shouldBe Some(81)
  }

  it should "record each bulk-observed Tomatometer change into the adaptive cadence (and nothing for unchanged rows)" in {
    val urlA = "https://www.rottentomatoes.com/m/a"
    val urlB = "https://www.rottentomatoes.com/m/b"
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord(tmdbId = Some(201), rottenTomatoes = Some(50), rottenTomatoesUrl = Some(urlA))),
      ("B", None, MovieRecord(tmdbId = Some(202), rottenTomatoes = Some(60), rottenTomatoesUrl = Some(urlB)))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val cadence = new services.cadence.InMemoryRatingCadenceStore
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None),
      rtClient(Map(urlA -> pageWithScore(74), urlB -> pageWithScore(60))),  // A moves, B unchanged
      cadenceRecorder = (key, tmdbId, v) => cadence.record(services.tasks.RatingTasks.dedupKey(services.freshness.FreshnessKind.RtRating, key, tmdbId), v))

    ratings.refreshAll()

    cadence.statsFor("rt|tmdb:201").flatMap(_.lastChange).map(_.to) shouldBe Some("74%")
    cadence.statsFor("rt|tmdb:202")                                    shouldBe None
  }

  it should "skip rows without an RT URL (no GET issued, no exception)" in {
    val urlA = "https://www.rottentomatoes.com/m/a"
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, mkEnrichment(Some(urlA))),
      // No RT URL — must not be fetched.
      ("B", None, mkEnrichment(None))
    ))
    val cache = new CaffeineMovieCache(repository)
    // Stub only knows A; if the walk tries to fetch B-related anything, it throws.
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), rtClient(Map(urlA -> pageWithScore(85))))

    noException should be thrownBy ratings.refreshAll()
    cache.get(cache.keyOf("A", None)).flatMap(_.rottenTomatoes) shouldBe Some(85)
    cache.get(cache.keyOf("B", None)).flatMap(_.rottenTomatoes) shouldBe None
  }

  // ── hint-keyed url cache ─────────────────────────────────────────────────────

  // Two rows that share the RT hint key (same cleanTitle → same title/fallback,
  // year None via the apiKey-less TMDB) but stay DISTINCT cache rows — distinct
  // tmdbIds keep them as separate same-title "remakes" rather than collapsing.
  // Each fetch of /m/foo is counted; discovery probes it once, the score scrape
  // once. With the cache the SECOND row's probe is skipped (one fewer GET).
  private def twoFooRows() = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
    ("Foo", Some(2024), MovieRecord(tmdbId = Some(42))),
    ("Foo", Some(2025), MovieRecord(tmdbId = Some(99)))
  )))
  private def countingRt(gets: java.util.concurrent.atomic.AtomicInteger): RottenTomatoesClient =
    new RottenTomatoesClient(http = new GetOnlyHttpFetch {
      def get(url: String): String = { gets.incrementAndGet(); pageWithScore(80) }
    })

  "the RT url cache" should "probe the slug once across two rows sharing the hints" in {
    val gets = new java.util.concurrent.atomic.AtomicInteger(0)
    val cache = twoFooRows()
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), countingRt(gets),
      new services.resolution.WriteThroughResolutionCache(new services.resolution.InMemoryResolutionStore()))

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
    ratings.refreshOneSync(cache.keyOf("Foo", Some(2025)))
    // row1: probe + score = 2; row2: score only (probe cached) = 1.
    gets.get() shouldBe 3
  }

  it should "probe again for each row without the cache (control)" in {
    val gets = new java.util.concurrent.atomic.AtomicInteger(0)
    val cache = twoFooRows()
    val ratings = new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), countingRt(gets),
      services.resolution.ResolutionCache.passthrough)

    ratings.refreshOneSync(cache.keyOf("Foo", Some(2024)))
    ratings.refreshOneSync(cache.keyOf("Foo", Some(2025)))
    gets.get() shouldBe 4
  }

}
