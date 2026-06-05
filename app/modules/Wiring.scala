package modules

import clients.TmdbClient
import controllers.{AuthController, FacebookDataDeletionController, HealthController, LandingController, LegalController, MovieController, MovieControllerService, PlanController, UptimeController, UserStateController}
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
// Wrocław/Warszawa cinema objects are imported inside the commented-out
// scraper block below; re-add them here when those scrapers go live.
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.{MongoConnection, ShowtimeCache, Stoppable, UptimeMonitor}
import services.auth.{AppleTokenValidator, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, OauthProvider}
import services.cinemas._
import services.enrichment._
import services.events.{EventBus, InProcessEventBus}
import services.movies._
import services.users.{AccountDeletion, CachingUserRepo, CachingUserStateRepo, MongoUserRepo, MongoUserStateRepo, UserRepo, UserStateRepo}
import tools.{Env, HttpFetch, MonitoringHttpFetch, RealHttpFetch}

trait Wiring {
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database)
  lazy val httoFetch: HttpFetch = new MonitoringHttpFetch(new RealHttpFetch(), uptimeMonitor)

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

  // Single override point for Multikino's transport. `fetchFor` builds a
  // Zyte → direct fallback chain. Tests override this field with
  // `FakeHttpFetch` and the routing decision drops out entirely.
  lazy val multikinoFetch: HttpFetch = MultikinoClient.fetchFor(httoFetch)

  // Named so `KinoMuzaSynopsisRefresher` can share the same instance — the
  // refresher calls `client.parseSynopsis` to keep one source of truth for
  // the detail-page parse.
  lazy val kinoMuzaClient: KinoMuzaClient = new KinoMuzaClient(httoFetch)

  // Every cinema scraper is wrapped in `RetryingCinemaScraper` so a single
  // transient upstream blip (5xx, dropped connection, momentarily broken
  // HTML block) doesn't leave the cinema's slot empty for an entire
  // refresh cycle. Multikino retains its own internal retry layers (Zyte
  // → direct + homepage warm-up); the outer wrapper only adds extra
  // coverage when every inner layer is exhausted.
  lazy val cinemaScrapers: Seq[CinemaScraper] = Seq(
    // ── Poznań ────────────────────────────────────────────────────────────
    new MultikinoClient(multikinoFetch),
    new CharlieMonroeClient(httoFetch),
    new KinoPalacoweClient(httoFetch),
    new HeliosClient(httoFetch),
    new CinemaCityScraper(cinemaCityClient, "1078", CinemaCityPoznanPlaza),
    new CinemaCityScraper(cinemaCityClient, "1081", CinemaCityKinepolis),
    kinoMuzaClient,
    new KinoBulgarskaClient(httoFetch),
    new KinoApolloClient(httoFetch),
    new RialtoClient(httoFetch),
    // ── Wrocław + Warszawa (verified IDs; enable when the multi-city rollout
    //    goes live — uncomment, then re-add the cinema imports up top) ───────
    //   import models.{CinemaCityWroclavia, CinemaCityKorona, MultikinoPasazGrunwaldzki,
    //     CinemaCityArkadia, CinemaCityBemowo, CinemaCityGaleriaPolnocna, CinemaCityJanki,
    //     CinemaCityMokotow, CinemaCityPromenada, CinemaCitySadyba, MultikinoZloteTarasy,
    //     MultikinoMlociny, MultikinoReduta, MultikinoTargowek, MultikinoWolaPark}
    // new CinemaCityScraper(cinemaCityClient, "1097", CinemaCityWroclavia),
    // new CinemaCityScraper(cinemaCityClient, "1067", CinemaCityKorona),
    // new MultikinoClient(multikinoFetch, "0010", MultikinoPasazGrunwaldzki),
    // new HeliosClient(httoFetch, HeliosNuxt.Magnolia),
    // new HeliosClient(httoFetch, HeliosNuxt.AlejaBielany),
    // new CinemaCityScraper(cinemaCityClient, "1074", CinemaCityArkadia),
    // new CinemaCityScraper(cinemaCityClient, "1061", CinemaCityBemowo),
    // new CinemaCityScraper(cinemaCityClient, "1096", CinemaCityGaleriaPolnocna),
    // new CinemaCityScraper(cinemaCityClient, "1069", CinemaCityJanki),
    // new CinemaCityScraper(cinemaCityClient, "1070", CinemaCityMokotow),
    // new CinemaCityScraper(cinemaCityClient, "1068", CinemaCityPromenada),
    // new CinemaCityScraper(cinemaCityClient, "1060", CinemaCitySadyba),
    // new MultikinoClient(multikinoFetch, "0013", MultikinoZloteTarasy),
    // new MultikinoClient(multikinoFetch, "0040", MultikinoMlociny),
    // new MultikinoClient(multikinoFetch, "0052", MultikinoReduta),
    // new MultikinoClient(multikinoFetch, "0024", MultikinoTargowek),
    // new MultikinoClient(multikinoFetch, "0025", MultikinoWolaPark),
    // new HeliosClient(httoFetch, HeliosNuxt.BlueCity),
    // ── Independents (bespoke clients; enable with the rollout) ──────────────
    // new NoweHoryzontyClient(httoFetch),  // Wrocław — Kino Nowe Horyzonty
    // new DcfClient(httoFetch),            // Wrocław — Dolnośląskie Centrum Filmowe
    // new MuranowClient(httoFetch),        // Warszawa — Kino Muranów
  ).map(s => new RetryingCinemaScraper(s, uptimeMonitor))

  // ── Events ────────────────────────────────────────────────────────────────
  lazy val eventBus: EventBus = new InProcessEventBus()

  // ── Mongo ─────────────────────────────────────────────────────────────────
  // Single shared MongoClient + database for every repo in the app. Prior
  // shape was three independent MongoClients (movies + users + userStates),
  // each with its own Netty event loop / connection pool / replica-set
  // monitor thread — together they tipped the JVM RSS past Fly's 512 MB
  // cgroup ceiling, causing the kernel OOM cycle. Sharing one connection
  // is the standard Mongo driver usage pattern.
  // A missing/unreachable Mongo is a hard boot failure (throws out of
  // construction, aborts startup) everywhere except tests, rather than
  // silently serving a film-less site. A local dev who intentionally runs
  // without Mongo can opt back into silent-degrade with MONGODB_OPTIONAL=true
  // (or =1) — see `MongoConnection`.
  lazy val mongoConnection: MongoConnection = {
    val optedOut = Env.get("MONGODB_OPTIONAL").exists(v => v == "true" || v == "1")
    MongoConnection.fromEnv(required = MongoConnection.isRequired(environmentMode == Mode.Test, optedOut))
  }

  // ── Users ─────────────────────────────────────────────────────────────────
  // Wrap the Mongo-backed user repos in caching decorators — every
  // logged-in page render goes through `userRepo.findById`, and
  // `bootMergeFromServer` hits `userStateRepo.find` right after.
  // Caching trims the Frankfurt-Atlas RTT off the critical path
  // once the row's in memory.
  lazy val userRepo:      UserRepo      = new CachingUserRepo(new MongoUserRepo(mongoConnection.database, fallbackToOwnInit = false))
  lazy val userStateRepo: UserStateRepo = new CachingUserStateRepo(new MongoUserStateRepo(mongoConnection.database, fallbackToOwnInit = false))

  // ── MovieRecord ────────────────────────────────────────────────────────────
  lazy val movieRepo: MovieRepo = new MongoMovieRepo(mongoConnection.database, fallbackToOwnInit = false)
  // Cache publishes `CinemaMovieAdded` directly from `recordCinemaScrape`
  // so the event fires AFTER the slot is persisted — a downstream handler
  // that reads the cache for the just-added (cinema, title, year) tuple
  // always sees the new slot. The bus passes through the same instance
  // every other service shares for `MovieRecordCreated` / `TmdbResolved`
  // / `ImdbIdMissing` / `ImdbIdResolved`.
  lazy val movieCache: CaffeineMovieCache = new CaffeineMovieCache(movieRepo, eventBus)
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

  // Detail-page enricher for Muza — the listing scrape no longer hits
  // per-film detail pages (their burst limiter doesn't tolerate 80+ every
  // 5 min) or carries the portrait poster, synopsis, or trailer. Two
  // triggers: the `CinemaMovieAdded` event (subscribed below; fires
  // immediately when a new Muza film is persisted) and a periodic 1-min
  // safety-net scan (`refreshOne`) that catches rows whose first event
  // was lost across a restart. `Some("")` is the "tried, nothing" sentinel
  // so each row stays out of the rescan once processed.
  lazy val kinoMuzaSynopsisRefresher = new KinoMuzaSynopsisRefresher(movieCache, kinoMuzaClient, httoFetch)

  // ── Showtime aggregation ──────────────────────────────────────────────────
  lazy val showtimeCache = new ShowtimeCache(cinemaScrapers, eventBus, movieCache)

  def controllerComponents: ControllerComponents
  def environmentMode: Mode
  implicit def materializer: org.apache.pekko.stream.Materializer

  // ── OAuth providers ──────────────────────────────────────────────────────
  // Each provider is wired only when its env vars are present. The result
  // is a Map keyed by `OauthProvider.name` — the `/auth/:provider/start`
  // route indexes directly. Missing keys → provider absent → start route
  // returns 404 and the navbar UI hides the login button. Local dev with
  // no OAuth secrets keeps working: pages render, login pill just doesn't.
  lazy val oauthProviders: Map[String, OauthProvider] = {
    val google = for {
      id     <- Env.get("GOOGLE_CLIENT_ID")
      secret <- Env.get("GOOGLE_CLIENT_SECRET")
    } yield new GoogleOauthProvider(httoFetch, id, secret)
    val facebook = for {
      id     <- Env.get("FACEBOOK_APP_ID")
      secret <- Env.get("FACEBOOK_APP_SECRET")
    } yield new FacebookOauthProvider(httoFetch, id, secret)
    Seq(google, facebook).flatten.map(p => p.name -> (p: OauthProvider)).toMap
  }

  lazy val googleTokenValidator: Option[GoogleTokenValidator] =
    Env.get("GOOGLE_CLIENT_ID").map(id => new GoogleTokenValidator(httoFetch, id))

  lazy val facebookTokenValidator: Option[FacebookTokenValidator] =
    for {
      id     <- Env.get("FACEBOOK_APP_ID")
      secret <- Env.get("FACEBOOK_APP_SECRET")
    } yield new FacebookTokenValidator(httoFetch, id, secret)

  lazy val appleTokenValidator: Option[AppleTokenValidator] =
    Env.get("APPLE_BUNDLE_ID").orElse(Some("dev.kinowo.Kinowo"))
      .map(bundleId => new AppleTokenValidator(httoFetch, bundleId))

  // ── Controllers ───────────────────────────────────────────────────────────
  lazy val landingController = new LandingController(controllerComponents)
  lazy val movieController  = new MovieController(controllerComponents, movieControllerService, movieCache, userRepo, oauthProviders.keySet, environmentMode)
  lazy val planController   = new PlanController(controllerComponents, movieControllerService, userRepo, oauthProviders.keySet, environmentMode)
  lazy val healthController = new HealthController(controllerComponents)
  lazy val uptimeController = new UptimeController(controllerComponents, uptimeMonitor)(using materializer)
  lazy val authController   = new AuthController(controllerComponents, oauthProviders, userRepo, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  lazy val accountDeletion   = new AccountDeletion(userRepo, userStateRepo)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepo, accountDeletion)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, Env.get("FACEBOOK_APP_SECRET"), userRepo, accountDeletion)

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
  eventBus.subscribe(kinoMuzaSynopsisRefresher.onCinemaMovieAdded)

  // Start background work and register shutdown hooks. Order matters on stop:
  // every ratings service's stop() must drain its worker pool before the
  // repo's close() runs so in-flight upserts land.
  protected def start(): Unit = {
    // Force Mongo connection at boot rather than waiting for the first
    // lazy val that needs it. Connection errors / hydrate-from-Mongo
    // delays surface here in the boot timeline instead of slipping into
    // the middle of a user request.
    mongoConnection.database
    movieCache.start()
    movieService.start()
    imdbRatings.start()
    rottenTomatoesRatings.start()
    metascoreRatings.start()
    filmwebRatings.start()
    unscreenedCleanup.start()
    kinoMuzaSynopsisRefresher.start()
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
    kinoMuzaSynopsisRefresher.stop()
    movieCache.stop()
    // Each repo's close() is a no-op when it borrowed its database from
    // `mongoConnection` — closing the shared MongoClient is owned here.
    movieRepo.close()
    userRepo.close()
    userStateRepo.close()
    mongoConnection.close()
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
