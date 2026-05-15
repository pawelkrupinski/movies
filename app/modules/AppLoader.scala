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
import services.cinemas.HeliosClient
import services.enrichment.{FilmwebClient, FilmwebRatings, ImdbClient, ImdbIdResolver, ImdbRatings, MetacriticClient, MetascoreRatings, RottenTomatoesClient, RottenTomatoesRatings}
import services.movies.{IdentityMerger, MovieCache, MovieRepo, MovieService}
import services.events.EventBus
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

  // ── Cinema scrapers + external API clients ────────────────────────────────
  lazy val heliosClient         = new HeliosClient()
  lazy val tmdbClient           = new TmdbClient()
  lazy val filmwebClient        = new FilmwebClient()
  lazy val imdbClient           = new ImdbClient()
  lazy val metacriticClient     = new MetacriticClient()
  lazy val rottenTomatoesClient = new RottenTomatoesClient()

  // ── Events ────────────────────────────────────────────────────────────────
  lazy val eventBus = new EventBus()

  // ── MovieRecord ────────────────────────────────────────────────────────────
  lazy val movieRepo    = new MovieRepo()
  lazy val movieCache   = new MovieCache(movieRepo)
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
  lazy val filmwebRatings        = new FilmwebRatings(movieCache, filmwebClient)
  lazy val movieService     = new MovieService(movieCache, eventBus, tmdbClient)
  // Folds rows that resolve to the same TMDB or IMDb id into one — handles
  // the year=None vs year=Some(year) duplicates cinemas inconsistently
  // produce, and bridges cross-script translations of the same film.
  lazy val identityMerger        = new IdentityMerger(movieCache)

  // ── Showtime aggregation ──────────────────────────────────────────────────
  lazy val showtimeCache = new ShowtimeCache(heliosClient, eventBus, movieCache)

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
  // IdentityMerger runs async on its own worker pool — safe to subscribe
  // in any order vs the rating listeners because they use `cache.putIfPresent`,
  // so a rating write to a key the merger just deleted can't resurrect it.
  eventBus.subscribe(identityMerger.onTmdbResolved)
  eventBus.subscribe(identityMerger.onImdbIdMissing)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  eventBus.subscribe(imdbRatings.onTmdbResolved)
  eventBus.subscribe(imdbRatings.onImdbIdResolved)
  eventBus.subscribe(rottenTomatoesRatings.onTmdbResolved)
  eventBus.subscribe(rottenTomatoesRatings.onImdbIdMissing)
  eventBus.subscribe(metascoreRatings.onTmdbResolved)
  eventBus.subscribe(metascoreRatings.onImdbIdMissing)
  eventBus.subscribe(filmwebRatings.onTmdbResolved)
  eventBus.subscribe(filmwebRatings.onImdbIdMissing)

  // Collapse any legacy duplicates the event-driven merger can't see —
  // already-resolved rows don't re-emit `TmdbResolved`, so two rows that
  // pre-date this code path (or were created across separate restarts
  // before the merger landed) would otherwise survive forever.
  identityMerger.mergeAll()

  // Start background work and register shutdown hooks. Order matters on stop:
  // every ratings service's stop() must drain its worker pool before the
  // repo's close() runs so in-flight upserts land.
  movieService.start()
  imdbRatings.start()
  rottenTomatoesRatings.start()
  metascoreRatings.start()
  filmwebRatings.start()
  showtimeCache.start()

  applicationLifecycle.addStopHook(() => Future.successful {
    showtimeCache.stop()
    movieService.stop()
    imdbIdResolver.stop()
    imdbRatings.stop()
    rottenTomatoesRatings.stop()
    metascoreRatings.stop()
    filmwebRatings.stop()
    identityMerger.stop()
    movieRepo.close()
  })
}
