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
import services.enrichment.{EnrichmentCache, EnrichmentRepo, EnrichmentService, FilmwebClient, ImdbClient, ImdbRatings, MetacriticClient, MetascoreRatings, RottenTomatoesClient, RottenTomatoesRatings}
import services.events.EventBus
import services.{Keepalive, ShowtimeCache}

/**
 * Compile-time DI entry point. Replaces Guice + the three `play.modules.enabled`
 * modules — every component is wired explicitly in `AppComponents` below.
 *
 * Selected via `play.application.loader` in `application.conf`.
 */
class AppLoader extends ApplicationLoader {
  override def load(context: Context): Application = {
    // Default to Mode.Dev so a forgotten env var doesn't silently expose debug
    // routes / disable the Keepalive in production. The deployment is expected
    // to set APP_MODE=prod explicitly.
    val mode = sys.env.get("APP_MODE").map(_.toLowerCase) match {
      case Some("prod" | "production") => Mode.Prod
      case Some("test")                => Mode.Test
      case Some("dev" | "development") => Mode.Dev
      case None                        => Mode.Dev
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
 * first use) and side-effecting components (`ShowtimeCache`, `Keepalive`,
 * event subscriptions) are forced at the bottom in the order they need to
 * fire.
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

  // ── Enrichment ────────────────────────────────────────────────────────────
  lazy val enrichmentRepo    = new EnrichmentRepo()
  lazy val enrichmentCache   = new EnrichmentCache(enrichmentRepo)
  // ImdbRatings / RottenTomatoesRatings own the hourly rating refresh + the
  // per-row event listener for their respective services. Pulled out of
  // EnrichmentService so each external service has its own tempo and the TMDB
  // stage doesn't block on IMDb's GraphQL CDN or RT's HTML render.
  lazy val imdbRatings           = new ImdbRatings(enrichmentCache, imdbClient)
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(enrichmentCache, rottenTomatoesClient)
  lazy val metascoreRatings      = new MetascoreRatings(enrichmentCache, metacriticClient)
  lazy val enrichmentService = new EnrichmentService(
    enrichmentCache, eventBus, imdbRatings,
    tmdbClient, filmwebClient, metacriticClient, rottenTomatoesClient
  )

  // ── Showtime aggregation + Fly keepalive ──────────────────────────────────
  lazy val showtimeCache = new ShowtimeCache(heliosClient, eventBus)
  lazy val keepalive     = new Keepalive()

  // ── Controllers ───────────────────────────────────────────────────────────
  lazy val movieController  = new MovieController(controllerComponents, showtimeCache, enrichmentService, environment)
  lazy val healthController = new HealthController(controllerComponents)

  // ── Router + filters ──────────────────────────────────────────────────────
  override def httpFilters: Seq[EssentialFilter] = super.httpFilters :+ corsFilter
  lazy val router: Router = new _root_.router.Routes(httpErrorHandler, movieController, healthController, assets)

  // ── Boot-time wiring ──────────────────────────────────────────────────────
  // Fail fast in production if the scraping key is missing — moved out of
  // ShowtimeCache so the class stays construction-pure.
  if (environment.mode == Mode.Prod && Option(System.getenv("SCRAPINGANT_KEY")).forall(_.isEmpty))
    throw new RuntimeException("SCRAPINGANT_KEY must be set in production")

  // Subscribe BEFORE ShowtimeCache.start() so the bus's first MovieAdded
  // events reach the enrichment handlers. Bus uses PartialFunction.applyOrElse,
  // so each listener only sees events it pattern-matches.
  //   MovieAdded   → enrichmentService.onMovieAdded         (runs TMDB stage)
  //   TmdbResolved → imdbRatings.onTmdbResolved             (runs IMDb stage)
  //   TmdbResolved → rottenTomatoesRatings.onTmdbResolved   (runs RT stage)
  //   TmdbResolved → metascoreRatings.onTmdbResolved        (runs Metascore stage)
  eventBus.subscribe(enrichmentService.onMovieAdded)
  eventBus.subscribe(imdbRatings.onTmdbResolved)
  eventBus.subscribe(rottenTomatoesRatings.onTmdbResolved)
  eventBus.subscribe(metascoreRatings.onTmdbResolved)

  // Start background work and register shutdown hooks. Order matters on stop:
  // every ratings service's stop() must drain its worker pool before the
  // repo's close() runs so in-flight upserts land.
  enrichmentService.start()
  imdbRatings.start()
  rottenTomatoesRatings.start()
  metascoreRatings.start()
  showtimeCache.start()
  if (environment.mode == Mode.Prod) keepalive.start()

  applicationLifecycle.addStopHook(() => Future.successful {
    if (environment.mode == Mode.Prod) keepalive.stop()
    showtimeCache.stop()
    enrichmentService.stop()
    imdbRatings.stop()
    rottenTomatoesRatings.stop()
    metascoreRatings.stop()
    enrichmentRepo.close()
  })
}
