package services.enrichment

import clients.TmdbClient
import models.{Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{DomainEvent, InProcessEventBus, MovieDetailsComplete, TmdbResolved}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository, MovieService}
import tools.Eventually.eventually
import tools.{HttpFetch, RoutingHttpFetch}

import scala.collection.mutable

/**
 * Tests for the two-stage enrichment pipeline:
 *
 *   1. **TMDB stage** (owned by `MovieService`): resolves `(title, year)`
 *      → tmdbId + imdbId + originalTitle. Publishes `TmdbResolved` on success.
 *   2. **IMDb stage** (owned by `ImdbRatings`, decoupled): fetches the IMDb
 *      rating for one cache row. Triggered by `TmdbResolved` via the EventBus,
 *      or by the hourly refresh loop. Also chainable synchronously by callers
 *      that just ran a sync TMDB pass and want a single-shot answer.
 *
 * Plus the daily TMDB-retry tick — clears the negative cache and re-schedules
 * the TMDB stage for any cached row with `tmdbId.isEmpty`.
 *
 * `reEnrichSync` is the sync test surface. It runs the TMDB stage on the
 * calling thread and does NOT publish events — callers that need the IMDb
 * rating filled on the same pass chain `imdbRatings.refreshOneSync(...)`.
 */
class EnrichmentPipelineStagesSpec extends AnyFlatSpec with Matchers {

  // ── Stubs ──────────────────────────────────────────────────────────────────

  private val deadFetch        = RoutingHttpFetch.dead("unused")
  private def deadFilmweb()    = new FilmwebClient(http = deadFetch)
  private def deadMetacritic() = new MetacriticClient(http = deadFetch)
  private def deadRt()         = new RottenTomatoesClient(http = deadFetch)

  // TMDB returns "Mortal Kombat II" → tt17490712 / id 931285.
  private val Mk2Search =
    """{"results":[
      |  {"id":931285, "title":"Mortal Kombat II", "original_title":"Mortal Kombat II",
      |   "release_date":"2026-05-06", "popularity":223.66}
      |]}""".stripMargin
  private val Mk2ExternalIds = """{"id":931285, "imdb_id":"tt17490712"}"""
  private val Mk2ImdbGraphql =
    """{"data":{"title":{"ratingsSummary":{"aggregateRating":7.0,"voteCount":17000}}}}"""

  private def tmdbStub() = new TmdbClient(
    http = new RoutingHttpFetch(Map("/search/movie" -> Mk2Search, "/external_ids" -> Mk2ExternalIds)),
    apiKey = Some("stub")
  )

  // ── TmdbResolved is published by the async TMDB stage (event-driven path) ──

  "the async TMDB stage" should "publish TmdbResolved after a successful resolution" in {
    val bus  = new InProcessEventBus()
    val seen = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seen.append(e) }

    val cache       = new CaffeineMovieCache(new InMemoryMovieRepository())
    val svc   = new MovieService(cache, bus, tmdbStub())

    // The async path goes through `MovieDetailsComplete` → needsTmdbResolution →
    // dispatchResolve → ec pool → resolveTmdbOnce, which publishes the event on
    // success (this spec has no enqueue wired, so dispatch runs inline).
    svc.onMovieDetailsComplete(MovieDetailsComplete("Mortal Kombat II", Some(2026)))

    eventually(seen.toSeq shouldBe Seq(TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712")))
  }

  it should "NOT publish TmdbResolved when TMDB has no match" in {
    val bus  = new InProcessEventBus()
    val seen = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seen.append(e) }

    val emptyTmdb = new TmdbClient(
      http = new RoutingHttpFetch(Map("/search/movie" -> """{"results":[]}""")),
      apiKey = Some("stub")
    )
    val cache       = new CaffeineMovieCache(new InMemoryMovieRepository())
    val imdbRatings = new ImdbRatings(cache, new ImdbClient(http = deadFetch))
    val svc   = new MovieService(cache, bus, emptyTmdb)

    svc.onMovieDetailsComplete(MovieDetailsComplete("Unknown Title", None))

    // Wait a beat — no event should fire because there's no hit.
    Thread.sleep(200)
    seen shouldBe empty
    // And the row should be negative-cached.
    cache.isNegative(cache.keyOf("Unknown Title", None)) shouldBe true
  }

  it should "persist tmdbNoMatch on a definitive no-match so the held-back row becomes ready to project" in {
    val emptyTmdb = new TmdbClient(
      http = new RoutingHttpFetch(Map("/search/movie" -> """{"results":[]}""")),
      apiKey = Some("stub")
    )
    // An already-scraped (cinema-side) row TMDB will fail to match — the real
    // flow always has the row present before the stage runs.
    val repository = new InMemoryMovieRepository(Seq(
      ("Some Local Premiere", Some(2026),
        MovieRecord(data = Map[Source, SourceData](
          Multikino -> SourceData(title = Some("Some Local Premiere"), releaseYear = Some(2026)))))
    ))
    val cache = new CaffeineMovieCache(repository)
    val svc   = new MovieService(cache, new InProcessEventBus(), emptyTmdb)
    val key   = cache.keyOf("Some Local Premiere", Some(2026))
    // Not yet concluded (no tmdbId, no no-match) → held back from the read model.
    cache.get(key).map(_.readyToProject) shouldBe Some(false)

    svc.onMovieDetailsComplete(MovieDetailsComplete("Some Local Premiere", Some(2026)))

    // A definitive no-match persists `tmdbNoMatch`, concluding the row so it is
    // released to the read model as a standalone card.
    eventually {
      val row = cache.get(key)
      row.map(_.tmdbNoMatch)    shouldBe Some(true)
      row.map(_.readyToProject) shouldBe Some(true)
    }
  }

  it should "NOT publish TmdbResolved when reEnrichSync runs (sync path is event-free so listeners don't race)" in {
    // Sync callers (scripts via reEnrichSync) drive each stage explicitly on
    // the calling thread; if reEnrichSync also fired bus events, every
    // subscribed *Ratings listener would async-fetch in parallel with the
    // caller's explicit refresh, racing on writes back to the cache.
    val bus  = new InProcessEventBus()
    val seen = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seen.append(e) }

    val cache       = new CaffeineMovieCache(new InMemoryMovieRepository())
    val imdb        = new ImdbClient(http = new RoutingHttpFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val imdbRatings = new ImdbRatings(cache, imdb)
    val svc   = new MovieService(cache, bus, tmdbStub())

    svc.reEnrichSync("Mortal Kombat II", Some(2026))
    seen shouldBe empty
  }

  // ── reEnrichSync chains both stages on the calling thread ─────────────────

  // `reEnrichSync` is TMDB-only by design (the sync path doesn't publish bus
  // events, so chaining IMDb internally would couple this service to
  // ImdbRatings). Callers that also want the IMDb rating filled on the same
  // pass compose the two services themselves — the script in
  // `scripts/EnrichmentBackfill` follows this pattern.
  "reEnrichSync" should "fill the row's IMDb rating when chained with imdbRatings.refreshOneSync" in {
    val bus      = new InProcessEventBus()
    val imdbHttp = new RoutingHttpFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql))
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository())
    val ratings  = new ImdbRatings(cache, new ImdbClient(http = imdbHttp))
    val svc      = new MovieService(cache, bus, tmdbStub())

    svc.reEnrichSync("Mortal Kombat II", Some(2026))
    ratings.refreshOneSync("Mortal Kombat II", Some(2026))
    val e = cache.get(cache.keyOf("Mortal Kombat II", Some(2026)))

    e.flatMap(_.imdbRating) shouldBe Some(7.0)
    e.flatMap(_.imdbId)     shouldBe Some("tt17490712")
    e.flatMap(_.tmdbId)     shouldBe Some(931285)
  }

  // Regression: TMDB sometimes resolves a film and reports the same tmdbId as
  // before but `external_ids.imdb_id = null` — happens when TMDB momentarily
  // drops a cross-reference (data hiccup) or hasn't replicated it yet for a
  // very recent release. The TMDB stage must NOT null an existing imdbId in
  // that case; we trust the previously-resolved one until tmdbId itself shifts
  // (which would mean it's a different film).
  it should "preserve an existing imdbId when TMDB resolves the same tmdbId but with no cross-reference" in {
    val seed = MovieRecord(
      imdbId = Some("tt17490712"), imdbRating = Some(7.0),
      tmdbId = Some(931285),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Mortal Kombat II")))
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Mortal Kombat II", Some(2026), seed)
    )))
    // TMDB search returns the same tmdbId 931285 but external_ids has no imdb_id.
    val tmdbHttp = new RoutingHttpFetch(Map(
      "/search/movie" -> Mk2Search,
      "/external_ids" -> """{"id":931285, "imdb_id":""}"""   // ← cross-reference dropped
    ))
    val tmdb = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val svc = new MovieService(
      cache, new InProcessEventBus(),
      tmdb
    )

    val e = svc.reEnrichSync("Mortal Kombat II", Some(2026))

    // The stale tmdbId is unchanged, so we keep the previously-known imdbId.
    e.flatMap(_.tmdbId) shouldBe Some(931285)
    e.flatMap(_.imdbId) shouldBe Some("tt17490712")  // preserved, NOT nulled
  }

  // The opposite case: when tmdbId changes, the row is now about a different
  // film. Preserving the old imdbId across that boundary would be wrong — the
  // new resolution wins, even if its imdbId is None.
  it should "discard the existing imdbId when TMDB resolves to a DIFFERENT tmdbId (correction path)" in {
    val seed = MovieRecord(
      imdbId = Some("tt-old-wrong-id"), imdbRating = Some(7.0),
      tmdbId = Some(99999),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Old Film")))
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Mortal Kombat II", Some(2026), seed)
    )))
    // TMDB now resolves to a DIFFERENT tmdbId (931285) AND that one has no
    // imdb cross-reference (yet). The old imdbId is about the wrong film and
    // must not leak into the new row.
    val tmdbHttp = new RoutingHttpFetch(Map(
      "/search/movie" -> Mk2Search,
      "/external_ids" -> """{"id":931285, "imdb_id":""}"""
    ))
    val tmdb = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val svc = new MovieService(
      cache, new InProcessEventBus(),
      tmdb
    )

    val e = svc.reEnrichSync("Mortal Kombat II", Some(2026))

    e.flatMap(_.tmdbId) shouldBe Some(931285)        // new film
    e.flatMap(_.imdbId) shouldBe None                // old imdbId discarded
  }

  it should "preserve an existing imdbRating when the IMDb GraphQL fetch fails" in {
    // Seed: row already has a rating. Re-enrich runs TMDB stage (succeeds)
    // and IMDb stage (network blip → no update). The cached rating must be
    // preserved (TMDB stage carries it forward).
    val seed = MovieRecord(
      imdbId = Some("tt17490712"), imdbRating = Some(7.2),
      tmdbId = Some(931285)
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Mortal Kombat II", Some(2026), seed)
    )))
    val brokenImdb = new ImdbClient(http = new HttpFetch {
      override def get(url: String): String = throw new RuntimeException("network blip")
      override def post(url: String, body: String, contentType: String): String = get(url)
    })
    val svc = new MovieService(cache, new InProcessEventBus(), tmdbStub())

    val e = svc.reEnrichSync("Mortal Kombat II", Some(2026))

    e.flatMap(_.imdbRating) shouldBe Some(7.2)   // unchanged
  }

  // ── Director-match verification + director-page filmography walk ──────────
  //
  // The Niedźwiedzica case: TMDB's pl-PL search for "Niedźwiedzica" returns
  // the 1999 American film "Grizzly Falls" (tmdb 50416) — same Polish title
  // is registered there, and TMDB hasn't been given a Polish title for the
  // 2026 Helgestad documentary (tmdb 1648927). The cinema reports the
  // director name "Asgeir Helgestad"; verifying that against the candidate's
  // credits should reject Grizzly Falls (directed by Stewart Raffill) and
  // fall back to walking Helgestad's TMDB filmography to find a 2026 hit.

  it should "accept the title-search candidate when its director matches the cinema's reported director" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val bus   = new InProcessEventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case r: TmdbResolved => resolved.append(r) }

    val tmdbHttp = new RoutingHttpFetch(Map(
      "/search/movie" -> Mk2Search,
      "/movie/931285/credits" -> """{"id":931285,"crew":[{"id":1,"name":"Simon McQuoid","job":"Director","department":"Directing"}]}""",
      "/external_ids" -> Mk2ExternalIds
    ))
    val tmdb = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val svc  = new MovieService(cache, bus, tmdb)
    bus.subscribe(svc.onMovieDetailsComplete)

    bus.publish(MovieDetailsComplete("Mortal Kombat II", Some(2026), None, Some("Simon McQuoid")))

    eventually(resolved.size shouldBe 1)
    val e = cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).get
    e.tmdbId shouldBe Some(931285)
    e.imdbId shouldBe Some("tt17490712")
  }

  it should "walk the director's TMDB filmography when the title candidate has a different director (Niedźwiedzica regression)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val bus   = new InProcessEventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    val missing  = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case r: TmdbResolved => resolved.append(r) }
    bus.subscribe { case m: services.events.ImdbIdMissing => missing.append(m) }

    // TMDB title search for "Niedźwiedzica" returns Grizzly Falls (1999, dir
    // Stewart Raffill) — wrong film. Director walk for "Asgeir Helgestad"
    // surfaces his actual 2026 doc; the resolver should pick that one.
    val wrongTitleHit =
      """{"results":[{"id":50416,"title":"Niedźwiedzica","original_title":"Grizzly Falls","release_date":"1999-12-31","popularity":2.5}]}"""
    val wrongCredits  = """{"id":50416,"crew":[{"id":99,"name":"Stewart Raffill","job":"Director","department":"Directing"}]}"""
    val personSearch  =
      """{"results":[{"id":2200772,"name":"Asgeir Helgestad","known_for_department":"Directing","popularity":0.014}]}"""
    val helgestadCredits =
      """{"crew":[
         {"id":1648927,"title":"Frost Without Snow and Ice","original_title":"Frost uten Snø og Is","release_date":"2026-04-09","department":"Directing","job":"Director","popularity":0.5},
         {"id":682310, "title":"Queen Without Land","original_title":"Queen Without Land","release_date":"2018-04-21","department":"Directing","job":"Director","popularity":0.1}
       ]}"""
    // Helgestad's 2026 film has no IMDb cross-reference on TMDB yet — we
    // should still resolve and publish ImdbIdMissing so ImdbRatings can
    // recover the id via the suggestion endpoint.
    val frostExternalIds = """{"id":1648927,"imdb_id":""}"""

    val tmdbHttp = new RoutingHttpFetch(Map(
      "/search/movie"                 -> wrongTitleHit,
      "/movie/50416/credits"          -> wrongCredits,
      "/search/person"                -> personSearch,
      "/person/2200772/movie_credits" -> helgestadCredits,
      "/movie/1648927/external_ids"   -> frostExternalIds
    ))
    val tmdb = new TmdbClient(http = tmdbHttp, apiKey = Some("stub"))
    val svc  = new MovieService(cache, bus, tmdb)
    bus.subscribe(svc.onMovieDetailsComplete)

    bus.publish(MovieDetailsComplete("Niedźwiedzica", Some(2026), None, Some("Asgeir Helgestad")))

    // Wait on the event, not just the cache row: runTmdbStageSync writes the
    // resolved row (cache.put) BEFORE resolveTmdbOnce publishes ImdbIdMissing, so
    // asserting missing.size outside `eventually` races that publish.
    eventually {
      val e = cache.get(cache.keyOf("Niedźwiedzica", Some(2026)))
      e.flatMap(_.tmdbId) shouldBe Some(1648927)
      missing.size shouldBe 1
    }
    // Resolved without an imdb cross-reference → ImdbIdMissing fires, no
    // TmdbResolved.
    resolved shouldBe empty
  }

  it should "leave behaviour unchanged when the cinema doesn't report a director" in {
    // Mortal Kombat II without a director hint: the existing TMDB title-search
    // path runs and stores the canonical row. Director verification is opt-in.
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val bus   = new InProcessEventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case r: TmdbResolved => resolved.append(r) }

    val svc = new MovieService(cache, bus, tmdbStub())
    bus.subscribe(svc.onMovieDetailsComplete)

    bus.publish(MovieDetailsComplete("Mortal Kombat II", Some(2026)))   // 3-arg form, director=None by default

    eventually(resolved.size shouldBe 1)
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).flatMap(_.tmdbId) shouldBe Some(931285)
  }

  // ── Daily TMDB retry ──────────────────────────────────────────────────────

  "retryUnresolvedTmdb" should "clear the negative cache so previously-failed lookups get another shot" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("Some Film", Some(2026))
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true

    val svc = new MovieService(
      cache, new InProcessEventBus(), new TmdbClient(http = new RoutingHttpFetch(Map("/search/movie" -> """{"results":[]}""")), apiKey = Some("stub"))
    )

    svc.retryUnresolvedTmdb()
    cache.isNegative(key) shouldBe false
  }

  it should "NOT re-run the TMDB stage for rows missing MC or RT URLs (those are recovered by *Ratings.refreshAll)" in {
    // Once a row has a tmdbId, we trust it. Re-running the TMDB stage purely
    // to discover missing MC/RT URLs would risk flipping the row to a
    // different film (TMDB's title search lands on whichever same-title hit
    // is most popular at the moment — that's how Belle / Bez końca / On
    // drive / Wspinaczka got mis-resolved). URL discovery is the rating
    // services' job: each *Ratings.refreshAll walk probes for missing URLs
    // without ever touching tmdbId / imdbId.
    val incomplete = MovieRecord(
      imdbId            = Some("tt17490712"),
      imdbRating        = Some(7.0),
      tmdbId            = Some(931285),
      data              = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Mortal Kombat II")))
    )
    val repository  = new InMemoryMovieRepository(Seq(("Mortal Kombat II", Some(2026), incomplete)))
    val cache = new CaffeineMovieCache(repository)
    val bus   = new InProcessEventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => resolved.append(e) }

    // TMDB stub throws on any call — we assert the retry never reaches it.
    val deadTmdb = new TmdbClient(http = deadFetch, apiKey = Some("stub"))
    val svc      = new MovieService(cache, bus, deadTmdb)

    noException should be thrownBy svc.retryUnresolvedTmdb()

    // Give the worker pool a beat — nothing should fire.
    Thread.sleep(200)
    resolved shouldBe empty
  }

  it should "schedule the TMDB stage for cached rows whose tmdbId is empty (legacy data)" in {
    // Seed a row that has imdbId but no tmdbId (legacy from before TMDB was
    // always set). It carries its title in a cinema slot, as a scraped row
    // does — the repository re-derives the display title from that on read (like
    // Mongo), so the resolved event reports "Mortal Kombat II", not the bare id.
    val seed = MovieRecord(
      imdbId = Some("tt-legacy"),
      data   = Map[Source, SourceData](Multikino -> SourceData(title = Some("Mortal Kombat II")))
    )
    val repository  = new InMemoryMovieRepository(Seq(("Mortal Kombat II", Some(2026), seed)))
    val cache = new CaffeineMovieCache(repository)
    val bus   = new InProcessEventBus()
    val seenResolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seenResolved.append(e) }

    val imdb = new ImdbClient(http = new RoutingHttpFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val svc  = new MovieService(cache, bus, tmdbStub())

    svc.retryUnresolvedTmdb()

    // The retry dispatches onto the worker pool — give it a moment.
    eventually(seenResolved.size shouldBe 1)
    seenResolved.head shouldBe TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712")

    // The cache row now carries the resolved tmdbId.
    val key = cache.keyOf("Mortal Kombat II", Some(2026))
    cache.get(key).flatMap(_.tmdbId) shouldBe Some(931285)
  }

  // ── MovieDetailsComplete triggers TMDB stage; TMDB skip-when-cached short-circuit ───

  "onMovieDetailsComplete" should "schedule TMDB stage when the row isn't cached" in {
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository())
    val bus      = new InProcessEventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => resolved.append(e) }

    val imdb = new ImdbClient(http = new RoutingHttpFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val svc  = new MovieService(cache, bus, tmdbStub())
    bus.subscribe(svc.onMovieDetailsComplete)

    bus.publish(MovieDetailsComplete("Mortal Kombat II", Some(2026)))

    eventually(resolved.size shouldBe 1)
    resolved.head shouldBe TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712")
  }

  it should "skip rows that already have a tmdbId (no redundant TMDB call)" in {
    val seed  = MovieRecord(
      imdbId = Some("tt17490712"), imdbRating = Some(7.0),
      tmdbId = Some(931285),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Mortal Kombat II")))
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Mortal Kombat II", Some(2026), seed))))
    val bus   = new InProcessEventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => resolved.append(e) }

    // dead TMDB / dead Filmweb / dead IMDb — assert no call lands.
    val svc = new MovieService(
      cache, bus, new TmdbClient(http = deadFetch, apiKey = Some("stub"))
    )
    bus.subscribe(svc.onMovieDetailsComplete)

    bus.publish(MovieDetailsComplete("Mortal Kombat II", Some(2026)))

    // Give the worker a beat — but no event should fire.
    Thread.sleep(100)
    resolved shouldBe empty
  }

  // ── IMDb stage fills the rating for a resolved row ──────────────────────────

  "imdbRatings.refreshOneSync" should "fetch the IMDb rating for a resolved row" in {
    // Seed a row that already has TMDB data but no IMDb rating yet — simulate
    // the state right after the TMDB stage writes. In production the queue's
    // RatingHandler runs this per row once TmdbResolved enqueues the task.
    val seed = MovieRecord(
      imdbId = Some("tt17490712"),
      tmdbId = Some(931285),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Mortal Kombat II")))
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Mortal Kombat II", Some(2026), seed))))

    val imdb        = new ImdbClient(http = new RoutingHttpFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val imdbRatings = new ImdbRatings(cache, imdb)

    imdbRatings.refreshOneSync("Mortal Kombat II", Some(2026))

    val key = cache.keyOf("Mortal Kombat II", Some(2026))
    cache.get(key).flatMap(_.imdbRating) shouldBe Some(7.0)
  }

}
