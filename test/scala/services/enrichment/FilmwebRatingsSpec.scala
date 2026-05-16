package services.enrichment

import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import clients.TmdbClient
import models.{CinemaShowings, MovieRecord, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, InProcessEventBus, MovieRecordCreated, TmdbResolved}
import tools.HttpFetch
import tools.Eventually.eventually

/**
 * Tests for `FilmwebRatings` — the extracted Filmweb stage. Mirrors the
 * other ratings specs (`ImdbRatingsSpec`, `RottenTomatoesRatingsSpec`).
 */
class FilmwebRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Scaffolding ─────────────────────────────────────────────────────────────

  /** Stub fetch that routes by URL substring. Filmweb's API endpoints:
   *    /live/search?query=...  → search hits
   *    /film/{id}/info         → title + year
   *    /film/{id}/rating       → rate
   */
  private class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  private val deadFetch = new HttpFetch {
    override def get(url: String): String = throw new RuntimeException(s"unused: $url")
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  /** TmdbClient with no API key — every method short-circuits to None / empty
   *  without making a network call. Used by tests whose path doesn't exercise
   *  URL discovery (i.e. row already has a filmwebUrl). */
  private val disabledTmdb = new TmdbClient(apiKey = None)

  private def mkEnrichment(
    imdbId:        String,
    filmwebUrl:    Option[String] = None,
    filmwebRating: Option[Double] = None
  ): MovieRecord =
    MovieRecord(
      imdbId        = Some(imdbId), imdbRating = None, metascore = None,
      originalTitle = None, tmdbId = Some(42),
      filmwebUrl    = filmwebUrl,
      filmwebRating = filmwebRating
    )

  // ── refreshOneSync: existing URL → rating-only refresh ─────────────────────

  "refreshOneSync" should "fetch the rating via the stored URL's id and write it back when it changes" in {
    val url = "https://www.filmweb.pl/film/Mortal+Kombat+II-2026-10007434"
    val repo = new InMemoryMovieRepo(Seq(
      ("Mortal Kombat II", Some(2026), mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(6.0)))
    ))
    val cache   = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/film/10007434/rating" -> """{"rate":6.72,"count":1000}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Mortal Kombat II", Some(2026)))

    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).flatMap(_.filmwebRating) shouldBe Some(6.72)
  }

  it should "skip search + info when the URL is present (cheap rating-only path)" in {
    // Verify by stubbing ONLY the rating endpoint; any other call would throw
    // "unstubbed URL".
    val url = "https://www.filmweb.pl/film/Title-9999"
    val repo = new InMemoryMovieRepo(Seq(("X", None, mkEnrichment("tt1", filmwebUrl = Some(url)))))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/film/9999/rating" -> """{"rate":7.5,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("X", None))
    cache.get(cache.keyOf("X", None)).flatMap(_.filmwebRating) shouldBe Some(7.5)
  }

  // ── refreshOneSync: missing URL → full lookup ──────────────────────────────

  "refreshOneSync (no stored URL)" should "fall through to filmweb.lookup, populating both URL and rating" in {
    val repo  = new InMemoryMovieRepo(Seq(("Drama", Some(2024), mkEnrichment("tt1"))))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"          -> """{"searchHits":[{"id":555,"type":"film","matchedTitle":"Drama"}]}""",
      "/film/555/info"        -> """{"title":"Drama","year":2024}""",
      "/film/555/rating"      -> """{"rate":7.2,"count":500}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Drama", Some(2024)))

    val after = cache.get(cache.keyOf("Drama", Some(2024))).get
    after.filmwebUrl    should not be empty
    after.filmwebUrl.get should endWith ("-555")
    after.filmwebRating shouldBe Some(7.2)
  }

  // ── refreshOneSync: URL discovery uses TMDB-derived fallback + directors ───

  it should "pass TMDB's originalTitle as fallback and TMDB credits as directors to filmweb.lookup" in {
    // Cinema scrapes "Diuna: Część druga" (Polish title); the row already
    // carries an `originalTitle` (the cinema-reported English/original), a
    // tmdbId, and one cinema's director slot. The lookup should:
    //   - Search Filmweb with the Polish title (succeeds, one hit).
    //   - Verify the Filmweb /preview director matches TMDB's credits.
    //   - Persist the URL + rating.
    val tmdbBody    = """{"id":693134,"title":"Dune: Part Two","release_date":"2024-02-27","alternative_titles":{"titles":[]}}"""
    val creditsBody = """{"id":693134,"crew":[{"job":"Director","name":"Denis Villeneuve"}]}"""
    val tmdb = new TmdbClient(http = new HttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/693134?")) tmdbBody
        else if (url.contains("/movie/693134/credits")) creditsBody
        else throw new RuntimeException(s"unstubbed TMDB url: $url")
    }, apiKey = Some("stub"))

    val cinemaShowings = Map[models.Cinema, CinemaShowings](
      Multikino -> CinemaShowings(
        filmUrl = None, posterUrl = None, synopsis = None, cast = None,
        director = Some("Denis Villeneuve"), runtimeMinutes = None,
        releaseYear = None, showtimes = Seq.empty
      )
    )
    val repo = new InMemoryMovieRepo(Seq(
      ("Diuna: Część druga", Some(2024), MovieRecord(
        imdbId        = Some("tt15239678"),
        imdbRating    = None,
        metascore     = None,
        originalTitle = Some("Dune: Part Two"),
        tmdbId        = Some(693134),
        cinemaShowings = cinemaShowings
      ))
    ))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"          -> """{"searchHits":[{"id":779836,"type":"film","matchedTitle":"Diuna"}]}""",
      "/film/779836/info"     -> """{"title":"Diuna: Część druga","originalTitle":"Dune: Part Two","year":2024}""",
      "/film/779836/preview"  -> """{"directors":[{"id":1,"name":"Denis Villeneuve"}]}""",
      "/film/779836/rating"   -> """{"rate":8.2,"count":100}"""
    )))
    val ratings = new FilmwebRatings(cache, tmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Diuna: Część druga", Some(2024)))

    val after = cache.get(cache.keyOf("Diuna: Część druga", Some(2024))).get
    after.filmwebUrl    should not be empty
    after.filmwebUrl.get should include ("-779836")
    after.filmwebRating shouldBe Some(8.2)
  }

  it should "drop a row whose only Filmweb candidate has a director that doesn't match TMDB's" in {
    // Same shape, but Filmweb returns the wrong "Belle" (Asante, 2013); TMDB
    // knows the cinema is screening Hosoda's 2021 anime. Director mismatch
    // → tightened lookup returns None → row stays URL-less.
    val tmdbBody    = """{"id":682507,"title":"Belle","release_date":"2021-07-16","alternative_titles":{"titles":[]}}"""
    val creditsBody = """{"id":682507,"crew":[{"job":"Director","name":"Mamoru Hosoda"}]}"""
    val tmdb = new TmdbClient(http = new HttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/682507?")) tmdbBody
        else if (url.contains("/movie/682507/credits")) creditsBody
        else throw new RuntimeException(s"unstubbed TMDB url: $url")
    }, apiKey = Some("stub"))

    val repo = new InMemoryMovieRepo(Seq(
      ("Belle", Some(2021), MovieRecord(
        imdbId = None, imdbRating = None, metascore = None, originalTitle = Some("Belle"),
        tmdbId = Some(682507)
      ))
    ))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"      -> """{"searchHits":[{"id":1,"type":"film","matchedTitle":"Belle"}]}""",
      "/film/1/info"      -> """{"title":"Belle","year":2013}""",
      "/film/1/preview"   -> """{"directors":[{"id":10,"name":"Amma Asante"}]}"""
    )))
    val ratings = new FilmwebRatings(cache, tmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Belle", Some(2021)))

    val after = cache.get(cache.keyOf("Belle", Some(2021))).get
    after.filmwebUrl    shouldBe None
    after.filmwebRating shouldBe None
  }

  // ── Failure handling ───────────────────────────────────────────────────────

  it should "swallow Filmweb fetch failures without throwing" in {
    val url  = "https://www.filmweb.pl/film/Foo-7"
    val repo = new InMemoryMovieRepo(Seq(("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(6.0)))))
    val cache = new CaffeineMovieCache(repo)
    val brokenFilmweb = new FilmwebClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("boom")
    })
    val ratings = new FilmwebRatings(cache, disabledTmdb, brokenFilmweb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", None))
    cache.get(cache.keyOf("Foo", None)).flatMap(_.filmwebRating) shouldBe Some(6.0)
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val ratings = new FilmwebRatings(cache, disabledTmdb, new FilmwebClient(deadFetch))
    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  it should "not write back when the rating is unchanged (idempotent)" in {
    val url = "https://www.filmweb.pl/film/Foo-12"
    val repo = new InMemoryMovieRepo(Seq(("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(7.5)))))
    val cache = new CaffeineMovieCache(repo)
    repo.upserts.clear()
    val filmweb = new FilmwebClient(new StubFetch(Map("/film/12/rating" -> """{"rate":7.5,"count":1}""")))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Foo", None))

    repo.upserts shouldBe empty
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "do the cheap rating-only path for rows with a URL and full lookup for rows without" in {
    val urlA = "https://www.filmweb.pl/film/A-1"
    val urlB = "https://www.filmweb.pl/film/B-2"
    val repo = new InMemoryMovieRepo(Seq(
      ("A", None, mkEnrichment("tt1", filmwebUrl = Some(urlA), filmwebRating = Some(5.0))),  // changed
      ("B", None, mkEnrichment("tt2", filmwebUrl = Some(urlB), filmwebRating = Some(6.0))),  // unchanged
      ("C", None, mkEnrichment("tt3"))                                                       // full lookup
    ))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/film/1/rating"   -> """{"rate":7.4,"count":1}""",
      "/film/2/rating"   -> """{"rate":6.0,"count":1}""",
      "/live/search"     -> """{"searchHits":[{"id":33,"type":"film","matchedTitle":"C"}]}""",
      "/film/33/info"    -> """{"title":"C","year":2024}""",
      "/film/33/rating"  -> """{"rate":8.1,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.filmwebRating) shouldBe Some(7.4)
    cache.get(cache.keyOf("B", None)).flatMap(_.filmwebRating) shouldBe Some(6.0)
    val c = cache.get(cache.keyOf("C", None)).get
    c.filmwebUrl.get  should endWith ("-33")
    c.filmwebRating shouldBe Some(8.1)
  }

  // ── auditOneSync (backfill API) ────────────────────────────────────────────

  "auditOneSync" should "drop a stored URL whose canonical id no longer matches the row's identity" in {
    // Row carries a stale Filmweb URL pointing at a completely unrelated film
    // (the legacy buggy lookup picked it). The tightened re-resolve returns
    // None because no candidate clears the title bar → URL + rating cleared.
    val staleUrl = "https://www.filmweb.pl/film/Its+About+Time-2015-838929"
    val repo = new InMemoryMovieRepo(Seq(
      ("Wartość sentymentalna", Some(2025), mkEnrichment(
        "tt1", filmwebUrl = Some(staleUrl), filmwebRating = Some(7.5)
      ))
    ))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"     -> """{"searchHits":[{"id":838929,"type":"film","matchedTitle":"Wartość sentymentalna"}]}""",
      "/film/838929/info" -> """{"title":"It's About Time","year":2015}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    val outcome = ratings.auditOneSync("Wartość sentymentalna", Some(2025))

    outcome shouldBe FilmwebRatings.Dropped(staleUrl)
    val after = cache.get(cache.keyOf("Wartość sentymentalna", Some(2025))).get
    after.filmwebUrl    shouldBe None
    after.filmwebRating shouldBe None
  }

  it should "report Corrected when re-resolution picks a different canonical URL" in {
    val staleUrl  = "https://www.filmweb.pl/film/Wrong-2015-111"
    val rightId   = 222
    val repo = new InMemoryMovieRepo(Seq(
      ("Foo", Some(2024), mkEnrichment("tt1", filmwebUrl = Some(staleUrl), filmwebRating = Some(5.0)))
    ))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"          -> s"""{"searchHits":[{"id":$rightId,"type":"film","matchedTitle":"Foo"}]}""",
      s"/film/$rightId/info"  -> """{"title":"Foo","year":2024}""",
      s"/film/$rightId/rating"-> """{"rate":8.0,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    val outcome = ratings.auditOneSync("Foo", Some(2024))

    outcome shouldBe a [FilmwebRatings.Corrected]
    val after = cache.get(cache.keyOf("Foo", Some(2024))).get
    after.filmwebUrl.get should include (s"-$rightId")
    after.filmwebRating  shouldBe Some(8.0)
  }

  it should "report Kept when re-resolution returns the same canonical URL" in {
    val rightId = 333
    val rightUrl = s"https://www.filmweb.pl/film/Foo-2024-$rightId"
    val repo = new InMemoryMovieRepo(Seq(
      ("Foo", Some(2024), mkEnrichment("tt1", filmwebUrl = Some(rightUrl), filmwebRating = Some(7.0)))
    ))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"          -> s"""{"searchHits":[{"id":$rightId,"type":"film","matchedTitle":"Foo"}]}""",
      s"/film/$rightId/info"  -> """{"title":"Foo","year":2024}""",
      s"/film/$rightId/rating"-> """{"rate":7.0,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    val outcome = ratings.auditOneSync("Foo", Some(2024))

    outcome shouldBe FilmwebRatings.Kept(rightUrl)
    val after = cache.get(cache.keyOf("Foo", Some(2024))).get
    after.filmwebUrl    shouldBe Some(rightUrl)
    after.filmwebRating shouldBe Some(7.0)
  }

  // ── Event listener ──────────────────────────────────────────────────────────

  "onTmdbResolved" should "trigger a per-row refresh when subscribed on the bus" in {
    val bus   = new InProcessEventBus()
    val url   = "https://www.filmweb.pl/film/Foo-99"
    val repo  = new InMemoryMovieRepo(Seq(("Foo", Some(2024), mkEnrichment("tt1", filmwebUrl = Some(url)))))
    val cache = new CaffeineMovieCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map("/film/99/rating" -> """{"rate":7.4,"count":1}""")))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)
    bus.subscribe(ratings.onTmdbResolved)

    bus.publish(TmdbResolved("Foo", Some(2024), "tt1"))

    eventually(cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.filmwebRating) shouldBe Some(7.4))
  }

  it should "ignore events of other types (PartialFunction.applyOrElse)" in {
    val bus   = new InProcessEventBus()
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val ratings = new FilmwebRatings(cache, disabledTmdb, new FilmwebClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))
    bus.subscribe(ratings.onTmdbResolved)

    noException should be thrownBy bus.publish(MovieRecordCreated("Anything", None))
  }

}
