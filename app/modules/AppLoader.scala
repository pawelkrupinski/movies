package modules

import clients.TmdbClient
import controllers.{AssetsComponents, HealthController, MovieController}
import play.api.ApplicationLoader.Context
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.{Application, ApplicationLoader, BuiltInComponentsFromContext, LoggerConfigurator, Mode}

import scala.concurrent.Future
import play.filters.HttpFiltersComponents
import play.filters.cors.CORSComponents
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
import services.cinemas.{CharlieMonroeClient, CinemaCityClient, CinemaCityScraper, CinemaScraper, HeliosClient, KinoApolloClient, KinoBulgarskaClient, KinoMuzaClient, KinoPalacoweClient, MultikinoClient, RialtoClient}
import services.enrichment.{FilmwebClient, FilmwebRatings, ImdbClient, ImdbIdResolver, ImdbRatings, MetacriticClient, MetascoreRatings, RottenTomatoesClient, RottenTomatoesRatings}
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieCache, MovieRepo, MovieService, UnscreenedCleanup}
import services.events.{EventBus, InProcessEventBus}
import services.ShowtimeCache

/**
 * Compile-time DI entry point. Replaces Guice + the three `play.modules.enabled`
 * modules — every component is wired explicitly in `AppComponents` below.
 *
 * Selected via `play.application.loader` in `application.conf`.
 */
class AppLoader extends ApplicationLoader {
  override def load(context: Context): Application = {
    // APP_MODE is an *override*; when unset we trust the mode Play already
    // baked into the Context. That works out to:
    //   - `sbt run`                          → Mode.Dev  (debug routes on)
    //   - production launcher (Docker/fly.io)→ Mode.Prod (debug routes 404)
    //   - tests                              → Mode.Test
    // Forcing Dev when APP_MODE is unset was leaking debug pages on fly because
    // we had no APP_MODE configured there — Play's own Prod was being overridden.
    val mode = sys.env.get("APP_MODE").map(_.toLowerCase) match {
      case Some("prod" | "production") => Mode.Prod
      case Some("test")                => Mode.Test
      case Some("dev" | "development") => Mode.Dev
      case None                        => context.environment.mode
      case Some(other)                 =>
        throw new IllegalArgumentException(s"Unknown APP_MODE: $other (expected dev|test|prod)")
    }
    val adjusted = context.copy(environment = context.environment.copy(mode = mode))
    LoggerConfigurator(adjusted.environment.classLoader)
      .foreach(_.configure(adjusted.environment))
    new AppComponents(adjusted).application
  }
}

/**
 * Single wiring class. Every dependency the app needs is constructed here as a
 * `lazy val` (so the order in the file doesn't matter — references resolve on
 * first use) and side-effecting components (`ShowtimeCache`, event
 * subscriptions) are forced at the bottom in the order they need to fire.
 */
class AppComponents(context: Context)
    extends BuiltInComponentsFromContext(context)
    with HttpFiltersComponents
    with CORSComponents
    with AssetsComponents {

  // ── External API clients ──────────────────────────────────────────────────
  lazy val tmdbClient           = new TmdbClient()
  lazy val filmwebClient        = new FilmwebClient()
  lazy val imdbClient           = new ImdbClient()
  lazy val metacriticClient     = new MetacriticClient()
  lazy val rottenTomatoesClient = new RottenTomatoesClient()

  // ── Cinema scrapers ───────────────────────────────────────────────────────
  // The only place that names a specific cinema. ShowtimeCache iterates the
  // list — adding a new cinema is a new entry here plus its scraper class.
  lazy val cinemaCityClient = new CinemaCityClient()
  lazy val cinemaScrapers: Seq[CinemaScraper] = Seq(
    new MultikinoClient(),
    new CharlieMonroeClient(),
    new KinoPalacoweClient(),
    new HeliosClient(),
    new CinemaCityScraper(cinemaCityClient, "1078", CinemaCityPoznanPlaza),
    new CinemaCityScraper(cinemaCityClient, "1081", CinemaCityKinepolis),
    new KinoMuzaClient(),
    new KinoBulgarskaClient(),
    new KinoApolloClient(),
    new RialtoClient()
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
  lazy val imdbRatings           = new ImdbRatings(movieCache, imdbClient)
  // Split out of ImdbRatings: handles `ImdbIdMissing` events by hitting IMDb's
  // suggestion endpoint, writes the resolved id back, then publishes
  // `ImdbIdResolved` so the rating fetchers chain on.
  lazy val imdbIdResolver        = new ImdbIdResolver(movieCache, imdbClient, eventBus)
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient)
  lazy val metascoreRatings      = new MetascoreRatings(movieCache, tmdbClient, metacriticClient)
  lazy val filmwebRatings        = new FilmwebRatings(movieCache, tmdbClient, filmwebClient)
  lazy val movieService     = new MovieService(movieCache, eventBus, tmdbClient)
  // Daily tick that drops rows whose `cinemaShowings` is empty — i.e. films
  // that no cinema is currently showing. Without it the cache + Mongo grow
  // unbounded (every festival / anniversary / one-off screening leaves a
  // permanent row when its single cinema drops the listing).
  lazy val unscreenedCleanup = new UnscreenedCleanup(movieCache)

  // ── Showtime aggregation ──────────────────────────────────────────────────
  lazy val showtimeCache = new ShowtimeCache(cinemaScrapers, eventBus, movieCache)

  // ── Controllers ───────────────────────────────────────────────────────────
  lazy val movieController  = new MovieController(controllerComponents, movieService, environment)
  lazy val healthController = new HealthController(controllerComponents)

  // ── Router + filters ──────────────────────────────────────────────────────
  override def httpFilters: Seq[EssentialFilter] = super.httpFilters :+ corsFilter
  lazy val router: Router = new _root_.router.Routes(httpErrorHandler, movieController, healthController, assets)

  // ── Boot-time wiring ──────────────────────────────────────────────────────
  // Fail fast in production if the scraping key is missing — moved out of
  // ShowtimeCache so the class stays construction-pure.
  if (environment.mode == Mode.Prod && Option(System.getenv("SCRAPINGANT_KEY")).forall(_.isEmpty))
    throw new RuntimeException("SCRAPINGANT_KEY must be set in production")

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
  movieService.start()
  imdbRatings.start()
  rottenTomatoesRatings.start()
  metascoreRatings.start()
  filmwebRatings.start()
  unscreenedCleanup.start()
  showtimeCache.start()

  applicationLifecycle.addStopHook(() => Future.successful {
    showtimeCache.stop()
    movieService.stop()
    imdbIdResolver.stop()
    imdbRatings.stop()
    rottenTomatoesRatings.stop()
    metascoreRatings.stop()
    filmwebRatings.stop()
    unscreenedCleanup.stop()
    movieRepo.close()
  })
}
