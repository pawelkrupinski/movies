package services.enrichment

import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, InProcessEventBus, MovieRecordCreated, TmdbResolved}
import tools.HttpFetch
import tools.Eventually.eventually

/**
 * Tests for `MetascoreRatings` — mirrors `ImdbRatingsSpec` but for the
 * Metacritic critic-aggregate score.
 */
class MetascoreRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Scaffolding ─────────────────────────────────────────────────────────────

  // Stub MC client that maps URL → JSON-LD HTML and lets the real parser run.
  private def mcStub(scores: Map[String, Option[Int]]): MetacriticClient = {
    new MetacriticClient(new HttpFetch {
      def get(url: String): String =
        scores.get(url) match {
          case Some(Some(s)) =>
            s"""<html><head><script type="application/ld+json">
               |{"@type":"Movie","aggregateRating":{"@type":"AggregateRating","ratingValue":$s,"bestRating":100,"worstRating":0,"reviewCount":10}}
               |</script></head><body></body></html>""".stripMargin
          case Some(None) =>
            // Page exists but no aggregated score yet — JSON-LD omits aggregateRating.
            """<html><head><script type="application/ld+json">{"@type":"Movie"}</script></head><body></body></html>"""
          case None => throw new RuntimeException(s"unstubbed URL: $url")
        }
    })
  }

  private def mkEnrichment(
    imdbId:        String,
    mcUrl:         Option[String] = None,
    metascore:     Option[Int]    = None
  ): MovieRecord =
    MovieRecord(
      imdbId        = Some(imdbId), imdbRating = None, metascore = metascore,
      originalTitle = None, tmdbId = Some(42),
      metacriticUrl = mcUrl
    )

  private val Url = "https://www.metacritic.com/movie/the-dark-knight"

  // ── refreshOneSync ──────────────────────────────────────────────────────────

  "refreshOneSync" should "scrape the score and write it back when it differs from the cached value" in {
    val repo  = new InMemoryMovieRepo(Seq(
      ("The Dark Knight", Some(2008), mkEnrichment("tt0468569", mcUrl = Some(Url), metascore = Some(70)))
    ))
    val cache  = new CaffeineMovieCache(repo)
    val mc     = mcStub(Map(Url -> Some(85)))
    val rates  = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mc)

    rates.refreshOneSync(cache.keyOf("The Dark Knight", Some(2008)))

    cache.get(cache.keyOf("The Dark Knight", Some(2008))).flatMap(_.metascore) shouldBe Some(85)
  }

  it should "not write back when the score is unchanged (idempotent)" in {
    val repo  = new InMemoryMovieRepo(Seq(
      ("The Dark Knight", Some(2008), mkEnrichment("tt0468569", mcUrl = Some(Url), metascore = Some(85)))
    ))
    val cache = new CaffeineMovieCache(repo)
    repo.upserts.clear()
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(Url -> Some(85))))

    rates.refreshOneSync(cache.keyOf("The Dark Knight", Some(2008)))

    repo.upserts shouldBe empty
  }

  it should "not change the cached score when MC has no aggregated score yet (None from parser)" in {
    val repo  = new InMemoryMovieRepo(Seq(
      ("Indie Film", Some(2025), mkEnrichment("tt9", mcUrl = Some(Url), metascore = Some(70)))
    ))
    val cache = new CaffeineMovieCache(repo)
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(Url -> None)))

    rates.refreshOneSync(cache.keyOf("Indie Film", Some(2025)))

    // Stays Some(70). We never clear an existing score just because MC's
    // current scrape didn't return one — that's almost always a transient
    // gap rather than MC actively removing a published Metascore.
    cache.get(cache.keyOf("Indie Film", Some(2025))).flatMap(_.metascore) shouldBe Some(70)
  }

  it should "swallow MC fetch failures (network blip, Cloudflare challenge) without throwing" in {
    val repo  = new InMemoryMovieRepo(Seq(("X", None, mkEnrichment("tt9", mcUrl = Some(Url), metascore = Some(70)))))
    val cache = new CaffeineMovieCache(repo)
    val brokenMc = new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("boom")
    })
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), brokenMc)

    noException should be thrownBy rates.refreshOneSync(cache.keyOf("X", None))
    cache.get(cache.keyOf("X", None)).flatMap(_.metascore) shouldBe Some(70)
  }

  it should "be a no-op when the row has no metacriticUrl" in {
    val repo  = new InMemoryMovieRepo(Seq(("X", None, mkEnrichment("tt9", mcUrl = None, metascore = None))))
    val cache = new CaffeineMovieCache(repo)
    // MC stub throws on any call — proving we never tried to fetch.
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy rates.refreshOneSync(cache.keyOf("X", None))
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy rates.refreshOneSync(cache.keyOf("Missing", None))
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "walk every cached row with an MC URL and update each score that changed" in {
    val url2 = "https://www.metacritic.com/movie/inception"
    val url3 = "https://www.metacritic.com/movie/the-godfather"
    val repo = new InMemoryMovieRepo(Seq(
      ("A", None, mkEnrichment("tt1", mcUrl = Some(Url),  metascore = Some(70))),
      ("B", None, mkEnrichment("tt2", mcUrl = Some(url2), metascore = Some(74))),
      ("C", None, mkEnrichment("tt3", mcUrl = Some(url3), metascore = Some(100))),
      ("D", None, mkEnrichment("tt4", mcUrl = None,       metascore = None))   // skipped: no URL
    ))
    val cache = new CaffeineMovieCache(repo)
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(
      Url  -> Some(85),    // changed
      url2 -> Some(74),    // unchanged
      url3 -> Some(100)    // unchanged
    )))

    rates.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.metascore) shouldBe Some(85)
    cache.get(cache.keyOf("B", None)).flatMap(_.metascore) shouldBe Some(74)
    cache.get(cache.keyOf("C", None)).flatMap(_.metascore) shouldBe Some(100)
    cache.get(cache.keyOf("D", None)).flatMap(_.metascore) shouldBe None
  }

  // ── URL discovery via TMDB usTitle fallback ─────────────────────────────────

  // Regression: HP1's UK/US title divergence. TMDB returns "Philosopher's
  // Stone" for both original_title and the en-US `title`; the slug derived
  // from either 404s. MC indexes the film at
  // `/movie/harry-potter-and-the-sorcerers-stone`, which slugifies from
  // TMDB's US alternative title ("Harry Potter and the Sorcerer's Stone").
  // The TMDB `details` call fetches alternative_titles via append_to_response.
  "resolveAndPersistUrl via TMDB usTitle fallback" should
    "use the US alternative title when the linkTitle + englishTitle slugs 404" in {
    val sorcerers    = "https://www.metacritic.com/movie/harry-potter-and-the-sorcerers-stone"
    val philosophers = "https://www.metacritic.com/movie/harry-potter-and-the-philosophers-stone"
    val tmdbBody = """{
      "id":671,"title":"Harry Potter and the Philosopher's Stone","release_date":"2001-11-16",
      "alternative_titles":{"titles":[
        {"iso_3166_1":"US","title":"Harry Potter and the Sorcerer's Stone","type":""}
      ]}
    }"""
    val tmdb = new TmdbClient(http = new HttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/671?")) tmdbBody
        else throw new RuntimeException(s"unstubbed TMDB url: $url")
    }, apiKey = Some("stub"))

    val repo = new InMemoryMovieRepo(Seq(
      ("Harry Potter i Kamień filozoficzny", Some(2001), MovieRecord(
        imdbId        = Some("tt0241527"),
        imdbRating    = None,
        metascore     = None,
        originalTitle = Some("Harry Potter and the Philosopher's Stone"),
        tmdbId        = Some(671),
        metacriticUrl = None
      ))
    ))
    val cache = new CaffeineMovieCache(repo)
    // MC stub: 404 for both philosophers slug variants, 200 + JSON-LD for sorcerers.
    val mc = new MetacriticClient(new HttpFetch {
      def get(url: String): String =
        if (url == sorcerers)
          """<html><head><script type="application/ld+json">
            |{"@type":"Movie","aggregateRating":{"@type":"AggregateRating","ratingValue":64,"bestRating":100,"worstRating":0,"reviewCount":10}}
            |</script></head><body></body></html>""".stripMargin
        else if (url == philosophers || url.contains("/search/") || url.contains("/movie/")) throw new RuntimeException("HTTP 404")
        else throw new RuntimeException(s"unstubbed MC url: $url")
    })
    val rates = new MetascoreRatings(cache, tmdb, mc)

    rates.refreshOneSync(cache.keyOf("Harry Potter i Kamień filozoficzny", Some(2001)))

    val row = cache.get(cache.keyOf("Harry Potter i Kamień filozoficzny", Some(2001))).get
    row.metacriticUrl shouldBe Some(sorcerers)
    row.metascore     shouldBe Some(64)
  }

  // ── Event listener ──────────────────────────────────────────────────────────

  "onTmdbResolved" should "trigger a metascore refresh for the resolved row when subscribed on the bus" in {
    val bus   = new InProcessEventBus()
    val repo  = new InMemoryMovieRepo(Seq(
      ("Foo", Some(2024), mkEnrichment("tt1", mcUrl = Some(Url), metascore = None))
    ))
    val cache = new CaffeineMovieCache(repo)
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(Url -> Some(85))))
    bus.subscribe(rates.onTmdbResolved)

    bus.publish(TmdbResolved("Foo", Some(2024), "tt1"))

    eventually(cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.metascore) shouldBe Some(85))
  }

  it should "ignore events of other types (PartialFunction.applyOrElse)" in {
    val bus   = new InProcessEventBus()
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))
    bus.subscribe(rates.onTmdbResolved)

    noException should be thrownBy bus.publish(MovieRecordCreated("Anything", None))
  }

}
