package modules

import controllers.AssetsComponents
import play.api.ApplicationLoader.Context
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api._
import play.filters.HttpFiltersComponents
import play.filters.cors.CORSComponents
import router.Routes
import tools.Env

import scala.concurrent.Future

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
    with AssetsComponents with Wiring {
  def environmentMode: Mode = environment.mode

  // Fail fast if the scraping key is missing — gated on the production
  // composition root so tests and recording scripts that extend `Wiring`
  // directly (and don't need ScrapingAnt — they read fixtures) aren't
  // blocked by the missing env var on a clean CI runner.
  if (Env.get("SCRAPINGANT_KEY").isEmpty)
    throw new RuntimeException("SCRAPINGANT_KEY must be set")

  // ── Router + filters ──────────────────────────────────────────────────────
  lazy val cspFilter: CspFilter = new CspFilter()(materializer, executionContext)
  override def httpFilters: Seq[EssentialFilter] =
    super.httpFilters :+ corsFilter :+ cspFilter
  lazy val router: Router = new Routes(httpErrorHandler, movieController, authController, userStateController, healthController, assets)

  start()

  applicationLifecycle.addStopHook(() => Future.successful(stop()))
}
