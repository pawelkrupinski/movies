package services.enrichment

import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import clients.TmdbClient
import models.{Filmweb, MovieRecord, Multikino, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{GetOnlyHttpFetch, RealHttpFetch, RoutingHttpFetch}

/**
 * Tests for `FilmwebRatings` — the extracted Filmweb stage. Mirrors the
 * other ratings specs (`ImdbRatingsSpec`, `RottenTomatoesRatingsSpec`).
 */
class FilmwebRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Scaffolding ─────────────────────────────────────────────────────────────

  // Filmweb's API endpoints we stub here:
  //   /live/search?query=...  → search hits
  //   /film/{id}/info         → title + year
  //   /film/{id}/rating       → rate
  // `RoutingHttpFetch` matches by URL substring.

  /** TmdbClient with no API key — every method short-circuits to None / empty
   *  without making a network call. Used by tests whose path doesn't exercise
   *  URL discovery (i.e. row already has a filmwebUrl). */
  private val disabledTmdb = new TmdbClient(new RealHttpFetch, apiKey = None)

  private def mkEnrichment(
    imdbId:        String,
    filmwebUrl:    Option[String] = None,
    filmwebRating: Option[Double] = None
  ): MovieRecord =
    MovieRecord(
      imdbId        = Some(imdbId), tmdbId = Some(42),
      filmwebUrl    = filmwebUrl,
      filmwebRating = filmwebRating
    )

  // ── refreshOneSync: existing URL → rating-only refresh ─────────────────────

  "refreshOneSync" should "fetch the rating via the stored URL's id and write it back when it changes" in {
    val url = "https://www.filmweb.pl/film/Mortal+Kombat+II-2026-10007434"
    val repository = new InMemoryMovieRepository(Seq(
      ("Mortal Kombat II", Some(2026), mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(6.0)))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/film/10007434/rating" -> """{"rate":6.72,"count":1000}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Mortal Kombat II", Some(2026)))

    // Stored at the badge's one-decimal display precision (6.72 → "6.7"), not the
    // raw vote average — see RatingDisplay.
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).flatMap(_.filmwebRating) shouldBe Some(6.7)
  }

  it should "skip search + info when the URL is present (cheap rating-only path)" in {
    // Verify by stubbing ONLY the rating endpoint; any other call would throw
    // "unstubbed URL".
    val url = "https://www.filmweb.pl/film/Title-9999"
    val repository = new InMemoryMovieRepository(Seq(("X", None, mkEnrichment("tt1", filmwebUrl = Some(url)))))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/film/9999/rating" -> """{"rate":7.5,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("X", None))
    cache.get(cache.keyOf("X", None)).flatMap(_.filmwebRating) shouldBe Some(7.5)
  }

  // ── refreshOneSync: missing URL → full lookup ──────────────────────────────

  "refreshOneSync (no stored URL)" should "fall through to filmweb.lookup, populating both URL and rating" in {
    val repository  = new InMemoryMovieRepository(Seq(("Drama", Some(2024), mkEnrichment("tt1"))))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
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
    val tmdb = new TmdbClient(http = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/693134?")) tmdbBody
        else if (url.contains("/movie/693134/credits")) creditsBody
        else throw new RuntimeException(s"unstubbed TMDB url: $url")
    }, apiKey = Some("stub"))

    val repository = new InMemoryMovieRepository(Seq(
      ("Diuna: Część druga", Some(2024), MovieRecord(
        imdbId        = Some("tt15239678"),
        tmdbId        = Some(693134),
        data = Map[Source, SourceData](
          Multikino -> SourceData(
            title    = Some("Diuna: Część druga"),
            director = Seq("Denis Villeneuve")
          ),
          Tmdb -> SourceData(originalTitle = Some("Dune: Part Two"))
        )
      ))
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
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
    val tmdb = new TmdbClient(http = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("/movie/682507?")) tmdbBody
        else if (url.contains("/movie/682507/credits")) creditsBody
        else throw new RuntimeException(s"unstubbed TMDB url: $url")
    }, apiKey = Some("stub"))

    val repository = new InMemoryMovieRepository(Seq(
      ("Belle", Some(2021), MovieRecord(
        tmdbId = Some(682507),
        data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Belle")))
      ))
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
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

  it should "read a parenthesised (YYYY) off the cinema title as the Filmweb lookup year for a year-less row" in {
    // "Konwicki: Lawa (1989)" — a yearless retrospective TMDB never resolved (no
    // tmdbId, no director). Filmweb's fuzzy search surfaces BOTH the unrelated
    // "Lawa"/orig "Lava" (2014, id 719437) and the real 1989 Konwicki film
    // (id 111). Without the embedded-year hint the lookup had no year to gate the
    // collision and took the first hit (2014); reading "(1989)" off the cinema
    // title lets the year gate drop the 2014 film and land the 1989 one.
    val repository = new InMemoryMovieRepository(Seq(
      ("Lawa", None, MovieRecord(
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Konwicki: Lawa (1989)")))
      ))
    ))
    val cache = new CaffeineMovieCache(repository)
    val fetch = new RoutingHttpFetch(Map(
      "/live/search"      -> """{"searchHits":[
        |  {"id":719437,"type":"film","matchedTitle":"Lawa"},
        |  {"id":111,"type":"film","matchedTitle":"Lawa"}
        |]}""".stripMargin,
      "/film/719437/info" -> """{"title":"Lawa","originalTitle":"Lava","year":2014}""",
      "/film/111/info"    -> """{"title":"Lawa","year":1989}""",
      "/film/111/rating"  -> """{"rate":7.1,"count":500}"""
    ))
    val filmweb = new FilmwebClient(fetch)
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    // The row keys on its derived display title (year-less scraped key); grab it
    // rather than reconstruct the sanitisation here.
    val key = cache.entries.head._1
    key.year shouldBe None
    ratings.refreshOneSync(key)

    val after = cache.get(key).get
    after.filmwebUrl.get should include ("-111")
    after.filmwebUrl.get should not include ("719437")
  }

  it should "store Filmweb's originalTitle, year, directors, genres and plot on the Filmweb slot" in {
    // Filmweb-only row (TMDB never resolved). On URL discovery it must persist a
    // FULL content slot — the Filmweb counterpart of an IMDb/TMDB slot — so the
    // Polish plot, directors, original title and year are available to back the
    // display fields when no other source has them.
    val repository = new InMemoryMovieRepository(Seq(
      ("Ostatni konsjerż", None, MovieRecord(
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Ostatni konsjerż")))
      ))
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/live/search"      -> """{"searchHits":[{"id":900,"type":"film","matchedTitle":"Ostatni konsjerż"}]}""",
      "/film/900/info"    -> """{"title":"Ostatni konsjerż","originalTitle":"Der letzte Concierge","year":2025}""",
      "/film/900/preview" -> """{"directors":[{"id":1,"name":"Gastón Solnicki"}],"genres":[{"id":2,"name":{"text":"Dramat"}}],"plot":{"synopsis":"Lucius Glantz walczy o hotel."}}""",
      "/film/900/rating"  -> """{"rate":4.7,"count":50}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    val key = cache.entries.head._1
    ratings.refreshOneSync(key)

    val slot = cache.get(key).get.data(Filmweb)
    slot.originalTitle shouldBe Some("Der letzte Concierge")
    slot.releaseYear   shouldBe Some(2025)
    slot.director      shouldBe Seq("Gastón Solnicki")
    slot.genres        shouldBe Seq("Dramat")
    slot.synopsis      shouldBe Some("Lucius Glantz walczy o hotel.")
  }

  it should "re-kick TMDB + IMDb resolution when URL discovery adds a director/originalTitle to a tmdbNoMatch row" in {
    // End-to-end: a film TMDB missed (tmdbNoMatch) gains a Filmweb slot with an
    // original title + director; the write must fire the enrichment retrigger with
    // ResolveTmdb + ResolveImdbId so Filmweb's data re-attempts the resolution.
    import services.movies.RetriggerKind
    val captured = scala.collection.mutable.ListBuffer.empty[Set[RetriggerKind]]
    val repository = new InMemoryMovieRepository(Seq(
      ("Ostatni konsjerż", None, MovieRecord(
        tmdbNoMatch = true,
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Ostatni konsjerż")))
      ))
    ))
    val cache = new CaffeineMovieCache(repository, retrigger = (_, _, kinds) => { captured += kinds; () })
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/live/search"      -> """{"searchHits":[{"id":900,"type":"film","matchedTitle":"Ostatni konsjerż"}]}""",
      "/film/900/info"    -> """{"title":"Ostatni konsjerż","originalTitle":"Der letzte Concierge","year":2025}""",
      "/film/900/preview" -> """{"directors":[{"id":1,"name":"Gastón Solnicki"}],"genres":[{"id":2,"name":{"text":"Dramat"}}]}""",
      "/film/900/rating"  -> """{"rate":4.7,"count":50}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.entries.head._1)

    val kinds = captured.flatten.toSet
    kinds should contain (RetriggerKind.ResolveTmdb)
    kinds should contain (RetriggerKind.ResolveImdbId)
  }

  // ── Failure handling ───────────────────────────────────────────────────────

  it should "swallow Filmweb fetch failures without throwing" in {
    val url  = "https://www.filmweb.pl/film/Foo-7"
    val repository = new InMemoryMovieRepository(Seq(("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(6.0)))))
    val cache = new CaffeineMovieCache(repository)
    val brokenFilmweb = new FilmwebClient(RoutingHttpFetch.dead("boom"))
    val ratings = new FilmwebRatings(cache, disabledTmdb, brokenFilmweb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", None))
    cache.get(cache.keyOf("Foo", None)).flatMap(_.filmwebRating) shouldBe Some(6.0)
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val ratings = new FilmwebRatings(cache, disabledTmdb, new FilmwebClient(RoutingHttpFetch.dead("unused")))
    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  it should "not write back when the rating is unchanged (idempotent)" in {
    val url = "https://www.filmweb.pl/film/Foo-12"
    val repository = new InMemoryMovieRepository(Seq(("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(7.5)))))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map("/film/12/rating" -> """{"rate":7.5,"count":1}""")))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Foo", None))

    repository.upserts shouldBe empty
  }

  it should "not write back when the rating drifts below display precision (vote-count noise)" in {
    // Filmweb's vote average creeps by ~1e-5 every fetch; the badge shows one
    // decimal, so a stored 7.5 seeing a fresh 7.53 is invisible. Rewriting the row
    // (and re-projecting the read model) for it is the steady per-refresh write
    // churn — the stored value already sits at display precision (RatingDisplay).
    val url = "https://www.filmweb.pl/film/Foo-12"
    val repository = new InMemoryMovieRepository(Seq(
      ("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(7.5)))))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map("/film/12/rating" -> """{"rate":7.53,"count":1001}""")))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    ratings.refreshOneSync(cache.keyOf("Foo", None))

    repository.upserts shouldBe empty
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "do the cheap rating-only path for rows with a URL and full lookup for rows without" in {
    val urlA = "https://www.filmweb.pl/film/A-1"
    val urlB = "https://www.filmweb.pl/film/B-2"
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, mkEnrichment("tt1", filmwebUrl = Some(urlA), filmwebRating = Some(5.0))),  // changed
      ("B", None, mkEnrichment("tt2", filmwebUrl = Some(urlB), filmwebRating = Some(6.0))),  // unchanged
      ("C", None, mkEnrichment("tt3"))                                                       // full lookup
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
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
    val repository = new InMemoryMovieRepository(Seq(
      ("Wartość sentymentalna", Some(2025), mkEnrichment(
        "tt1", filmwebUrl = Some(staleUrl), filmwebRating = Some(7.5)
      ))
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
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
    val repository = new InMemoryMovieRepository(Seq(
      ("Foo", Some(2024), mkEnrichment("tt1", filmwebUrl = Some(staleUrl), filmwebRating = Some(5.0)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
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
    val repository = new InMemoryMovieRepository(Seq(
      ("Foo", Some(2024), mkEnrichment("tt1", filmwebUrl = Some(rightUrl), filmwebRating = Some(7.0)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/live/search"           -> s"""{"searchHits":[{"id":$rightId,"type":"film","matchedTitle":"Foo"}]}""",
      s"/film/$rightId/info"   -> """{"title":"Foo","originalTitle":"Foo Original","year":2024}""",
      s"/film/$rightId/preview"-> """{"directors":[{"id":1,"name":"Jane Doe"}],"genres":[{"id":2,"name":{"text":"Dramat"}}],"plot":{"synopsis":"Blurb po polsku."}}""",
      s"/film/$rightId/rating" -> """{"rate":7.0,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb)

    val outcome = ratings.auditOneSync("Foo", Some(2024))

    outcome shouldBe FilmwebRatings.Kept(rightUrl)
    val after = cache.get(cache.keyOf("Foo", Some(2024))).get
    after.filmwebUrl    shouldBe Some(rightUrl)
    after.filmwebRating shouldBe Some(7.0)
    // A Kept (same-URL) audit still BACKFILLS the full Filmweb content slot, so a
    // row enriched before Filmweb became a full source gains its metadata.
    val slot = after.data(models.Filmweb)
    slot.originalTitle shouldBe Some("Foo Original")
    slot.director      shouldBe Seq("Jane Doe")
    slot.genres        shouldBe Seq("Dramat")
    slot.synopsis      shouldBe Some("Blurb po polsku.")
  }

  // ── hint-keyed url cache ─────────────────────────────────────────────────────

  // `auditOneSync` re-resolves the url on every call, so two audits of the same
  // row exercise url discovery twice. The cache should make the SECOND audit
  // skip the Filmweb `/live/search` (the expensive discovery), rebuilding the
  // rating/genres from the cached url instead.
  private def countingFilmweb(searches: java.util.concurrent.atomic.AtomicInteger): FilmwebClient = {
    val routes = Map(
      "/live/search"     -> """{"searchHits":[{"id":555,"type":"film","matchedTitle":"Foo"}]}""",
      "/film/555/info"   -> """{"title":"Foo","year":2024}""",
      "/film/555/rating" -> """{"rate":7.2,"count":500}"""
    )
    new FilmwebClient(new GetOnlyHttpFetch {
      def get(url: String): String = {
        if (url.contains("/live/search")) searches.incrementAndGet()
        routes.collectFirst { case (frag, body) if url.contains(frag) => body }
          .getOrElse(throw new RuntimeException(s"unstubbed Filmweb url: $url"))
      }
    })
  }

  // ── onImdbIdMissing callback ─────────────────────────────────────────────────

  "refreshOneSync (url discovered, no imdbId)" should "fire the callback so Wikidata resolution is triggered" in {
    val url        = "https://www.filmweb.pl/film/Popiół+i+diament-1958-1118"
    val repository = new InMemoryMovieRepository(Seq(
      ("Popiół i diament", Some(1958),
        MovieRecord(tmdbId = Some(1), data = Map(Tmdb -> SourceData(originalTitle = Some("Ashes and Diamonds")))))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/live/search"    -> s"""{"searchHits":[{"id":1118,"type":"film","matchedTitle":"Popiół i diament"}]}""",
      "/film/1118/info" -> """{"title":"Popiół i diament","year":1958}""",
      "/film/1118/rating" -> """{"rate":8.1,"count":2000}"""
    )))
    var fired: Option[(String, Option[Int], String)] = None
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb,
      onImdbIdMissing = (title, year, search) => fired = Some((title, year, search)))

    ratings.refreshOneSync(cache.keyOf("Popiół i diament", Some(1958)))

    fired shouldBe Some(("Popiół i diament", Some(1958), "Ashes and Diamonds"))
  }

  "refreshOneSync (url already set, no imdbId)" should "fire the callback on rating-only refresh to catch existing rows" in {
    val url        = "https://www.filmweb.pl/film/Popiół+i+diament-1958-1118"
    val repository = new InMemoryMovieRepository(Seq(
      ("Popiół i diament", Some(1958), MovieRecord(tmdbId = Some(1), filmwebUrl = Some(url)))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/film/1118/rating" -> """{"rate":8.1,"count":2000}"""
    )))
    var fired = false
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb,
      onImdbIdMissing = (_, _, _) => fired = true)

    ratings.refreshOneSync(cache.keyOf("Popiół i diament", Some(1958)))

    fired shouldBe true
  }

  it should "NOT fire the callback when imdbId is already resolved" in {
    val url        = "https://www.filmweb.pl/film/Popiół+i+diament-1958-1118"
    val repository = new InMemoryMovieRepository(Seq(
      ("Popiół i diament", Some(1958),
        MovieRecord(imdbId = Some("tt0052080"), tmdbId = Some(1), filmwebUrl = Some(url)))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val filmweb = new FilmwebClient(new RoutingHttpFetch(Map(
      "/film/1118/rating" -> """{"rate":8.1,"count":2000}"""
    )))
    var fired = false
    val ratings = new FilmwebRatings(cache, disabledTmdb, filmweb,
      onImdbIdMissing = (_, _, _) => fired = true)

    ratings.refreshOneSync(cache.keyOf("Popiół i diament", Some(1958)))

    fired shouldBe false
  }

  "the Filmweb url cache" should "search once across two audits of the same row" in {
    val searches = new java.util.concurrent.atomic.AtomicInteger(0)
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1")))))
    val ratings = new FilmwebRatings(cache, disabledTmdb, countingFilmweb(searches),
      new services.resolution.WriteThroughResolutionCache(new services.resolution.InMemoryResolutionStore()))

    ratings.auditOneSync("Foo", Some(2024))
    ratings.auditOneSync("Foo", Some(2024))
    searches.get() shouldBe 1
  }

  it should "search on every audit without the cache (control)" in {
    val searches = new java.util.concurrent.atomic.AtomicInteger(0)
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt1")))))
    val ratings = new FilmwebRatings(cache, disabledTmdb, countingFilmweb(searches),
      services.resolution.ResolutionCache.passthrough)

    ratings.auditOneSync("Foo", Some(2024))
    ratings.auditOneSync("Foo", Some(2024))
    searches.get() shouldBe 2
  }

}
