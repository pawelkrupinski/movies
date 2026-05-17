package modules

import clients.TmdbClient
import controllers.{HealthController, MovieController, MovieControllerService}
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
import play.api.{Environment, Mode}
import play.api.mvc.ControllerComponents
import services.{ShowtimeCache, Stoppable}
import services.cinemas._
import services.enrichment._
import services.events.{EventBus, InProcessEventBus}
import services.movies._
import tools.{HttpFetch, RealHttpFetch}

trait Wiring {
  lazy val httoFetch: HttpFetch = new RealHttpFetch()

  // ── External API clients ──────────────────────────────────────────────────
  lazy val tmdbClient = new TmdbClient(httoFetch)
  lazy val filmwebClient = new FilmwebClient(httoFetch)
  lazy val imdbClient = new ImdbClient(httoFetch)
  lazy val metacriticClient = new MetacriticClient(httoFetch)
  lazy val rottenTomatoesClient = new RottenTomatoesClient(httoFetch)

  // ── Cinema scrapers ───────────────────────────────────────────────────────
  // The only place that names a specific cinema. ShowtimeCache iterates the
  // list — adding a new cinema is a new entry here plus its scraper class.
  lazy val cinemaCityClient = new CinemaCityClient(httoFetch)
  // Multikino's films API responds 401 to any direct datacenter request
  // without a session cookie, so it gets its own session-handling fetch
  // (`DefaultFetch` does the homepage warm-up + optional ScrapingAnt
  // routing). Exposed as an overridable member so fixture tests can route
  // it through their `FakeHttpFetch`; production must keep the default —
  // swapping it back to the shared `httoFetch` is a silent prod-only
  // regression (the FakeHttpFetch path stays green in unit tests).
  // `ClientIntegrationSpec` sources every cinema scraper from this Wiring
  // and hits the live cinema APIs, so a regression here surfaces as a 401
  // there.
  lazy val multikinoFetch: HttpFetch = MultikinoClient.DefaultFetch

  lazy val cinemaScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(multikinoFetch),
    new CharlieMonroeClient(httoFetch),
    new KinoPalacoweClient(httoFetch),
    new HeliosClient(httoFetch),
    new CinemaCityScraper(cinemaCityClient, "1078", CinemaCityPoznanPlaza),
    new CinemaCityScraper(cinemaCityClient, "1081", CinemaCityKinepolis),
    new KinoMuzaClient(httoFetch),
    new KinoBulgarskaClient(httoFetch),
    new KinoApolloClient(httoFetch),
    new RialtoClient(httoFetch)
  )

  // ── Events ────────────────────────────────────────────────────────────────
  lazy val eventBus: EventBus = new InProcessEventBus()

  // ── MovieRecord ────────────────────────────────────────────────────────────
  lazy val movieRepo: MovieRepo = new MongoMovieRepo()
  lazy val movieCache: MovieCache = new CaffeineMovieCache(movieRepo)
  // ImdbRatings / RottenTomatoesRatings own the hourly rating refresh + the
  // per-row event listener for their respective services. Pulled out of
  // MovieService so each external service has its own tempo and the TMDB
  // stage doesn't block on IMDb's GraphQL CDN or RT's HTML render.
  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient)
  // Split out of ImdbRatings: handles `ImdbIdMissing` events by hitting IMDb's
  // suggestion endpoint, writes the resolved id back, then publishes
  // `ImdbIdResolved` so the rating fetchers chain on.
  lazy val imdbIdResolver = new ImdbIdResolver(movieCache, imdbClient, eventBus)
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient)
  lazy val metascoreRatings = new MetascoreRatings(movieCache, tmdbClient, metacriticClient)
  lazy val filmwebRatings = new FilmwebRatings(movieCache, tmdbClient, filmwebClient)
  lazy val movieService = new MovieService(movieCache, eventBus, tmdbClient)
  lazy val movieControllerService = new MovieControllerService(movieService)
  // Daily tick that drops rows whose `cinemaData` is empty — i.e. films
  // that no cinema is currently showing. Without it the cache + Mongo grow
  // unbounded (every festival / anniversary / one-off screening leaves a
  // permanent row when its single cinema drops the listing).
  lazy val unscreenedCleanup = new UnscreenedCleanup(movieCache)

  // ── Showtime aggregation ──────────────────────────────────────────────────
  lazy val showtimeCache = new ShowtimeCache(cinemaScrapers, eventBus, movieCache)

  def controllerComponents: ControllerComponents
  def environmentMode: Mode

  // ── Controllers ───────────────────────────────────────────────────────────
  lazy val movieController = new MovieController(controllerComponents, movieControllerService, environmentMode)
  lazy val healthController = new HealthController(controllerComponents)

  // Subscribe BEFORE ShowtimeCache.start() so the bus's first MovieRecordCreated
  // events reach the enrichment handlers. Bus uses PartialFunction.applyOrElse,
  // so each listener only sees events it pattern-matches.
  //
  // Non-IMDb listeners (Filmweb / MC / RT) subscribe to BOTH `TmdbResolved`
  // and `ImdbIdMissing` — neither service needs an IMDb id, and the latter
  // event is the only signal we get for TMDB hits without an IMDb cross-
  // reference (very recent Polish indies, e.g. "Chłopiec na krańcach świata"
  // tmdbId=1277047, imdb_id=null). Without subscribing to `ImdbIdMissing`,
  // those rows had to wait an hour for the next periodic walk to pick them up.
  //
  //   MovieRecordCreated → movieService.onMovieRecordCreated         (runs TMDB stage)
  //   TmdbResolved       → imdbRatings.onTmdbResolved                (runs IMDb stage)
  //   TmdbResolved       → rottenTomatoesRatings.onTmdbResolved      (runs RT stage)
  //   TmdbResolved       → metascoreRatings.onTmdbResolved           (runs Metascore stage)
  //   TmdbResolved       → filmwebRatings.onTmdbResolved             (runs Filmweb stage)
  //   ImdbIdMissing      → imdbIdResolver.onImdbIdMissing            (IMDb-search fallback → publishes ImdbIdResolved)
  //   ImdbIdMissing      → rottenTomatoesRatings.onImdbIdMissing     (RT stage on TMDB-only hits)
  //   ImdbIdMissing      → metascoreRatings.onImdbIdMissing          (Metascore stage on TMDB-only hits)
  //   ImdbIdMissing      → filmwebRatings.onImdbIdMissing            (Filmweb stage on TMDB-only hits)
  //   ImdbIdResolved     → imdbRatings.onImdbIdResolved              (rating fetch once id is known)
  eventBus.subscribe(movieService.onMovieRecordCreated)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  eventBus.subscribe(imdbRatings.onTmdbResolved)
  eventBus.subscribe(imdbRatings.onImdbIdResolved)
  eventBus.subscribe(rottenTomatoesRatings.onTmdbResolved)
  eventBus.subscribe(rottenTomatoesRatings.onImdbIdMissing)
  eventBus.subscribe(metascoreRatings.onTmdbResolved)
  eventBus.subscribe(metascoreRatings.onImdbIdMissing)
  eventBus.subscribe(filmwebRatings.onTmdbResolved)
  eventBus.subscribe(filmwebRatings.onImdbIdMissing)

  // Start background work and register shutdown hooks. Order matters on stop:
  // every ratings service's stop() must drain its worker pool before the
  // repo's close() runs so in-flight upserts land.
  protected def start(): Unit = {
    movieService.start()
    imdbRatings.start()
    rottenTomatoesRatings.start()
    metascoreRatings.start()
    filmwebRatings.start()
    unscreenedCleanup.start()
    showtimeCache.start()
  }

  /** The full event-cascade drain order, producer→consumer. Each `stop()`
   *  in this sequence awaits its worker pool's termination, so by the time
   *  the next entry runs every event the previous one was going to publish
   *  has fired and the next entry's queue is fully populated:
   *
   *    movieService     → publishes TmdbResolved / ImdbIdMissing
   *    imdbIdResolver   → consumes ImdbIdMissing, publishes ImdbIdResolved
   *    imdbRatings      → consumes TmdbResolved + ImdbIdResolved
   *    *Ratings (RT, MC, FW) → consume TmdbResolved + ImdbIdMissing
   *
   *  Exposed as a `Seq[Stoppable]` so tests / recording scripts can drain
   *  the same set in the same order without duplicating the list. */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(
    movieService,
    imdbIdResolver,
    imdbRatings,
    rottenTomatoesRatings,
    metascoreRatings,
    filmwebRatings
  )

  protected def stop(): Unit = {
    // showtimeCache first so no more scrape submissions land while we drain.
    showtimeCache.stop()
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    movieRepo.close()
  }

  /** Run the entire enrichment pipeline for one row, synchronously, on the
   *  calling thread. No worker pools, no scheduled retries — every HTTP
   *  call goes out and back before the next stage starts.
   *
   *  Used by recording scripts (`RecordAllDataToFixture`) as a safety net
   *  *after* the bus-driven path has drained. The async path's retry
   *  scheduler can drop tasks if their backoff window outlives
   *  `movieService.worker`'s shutdown — a transient TMDB rate-limit
   *  blip during the first scrape burst, for example, would cost every
   *  affected row its enrichment. This pass picks those rows up and
   *  records every fixture they're entitled to.
   *
   *  Stages:
   *    1. `reEnrichSync` runs the TMDB stage — tmdb.search → /external_ids
   *       (or sister-row donation, if a cached sibling already resolved).
   *    2. If TMDB resolved a tmdbId without an imdbId cross-reference,
   *       `imdbIdResolver.resolveSync` hits IMDb's suggestion endpoint.
   *    3. Each `*Ratings.refreshOneSync` does its URL discovery + rating
   *       scrape — Filmweb (search + info + preview + rating), Metacritic
   *       (slug probe + canonical-page scrape), Rotten Tomatoes (slug
   *       probe + canonical-page scrape), IMDb (GraphQL CDN POST).
   *
   *  Idempotent — safe to call after the bus-driven path. Already-resolved
   *  rows re-hit the same URLs; `RecordingHttpFetch` overwrites the
   *  fixture with byte-identical content. */
  def fullySyncOne(title: String, year: Option[Int]): Unit = {
    if (movieService.get(title, year).flatMap(_.tmdbId).isEmpty) movieService.reEnrichSync(title, year)
    for {
      row <- movieService.get(title, year)
      _   <- row.tmdbId if row.imdbId.isEmpty
    } imdbIdResolver.resolveSync(title, year, row.originalTitle.getOrElse(title))
    imdbRatings.refreshOneSync(title, year)
    rottenTomatoesRatings.refreshOneSync(title, year)
    metascoreRatings.refreshOneSync(title, year)
    filmwebRatings.refreshOneSync(title, year)
  }
}
