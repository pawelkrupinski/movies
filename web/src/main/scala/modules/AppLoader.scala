package modules

import controllers.{AssetsComponents, TruncationTolerantHttpErrorHandler}
import play.api.ApplicationLoader.Context
import play.api.http.{HttpErrorConfig, HttpErrorHandler}
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api._
import play.filters.HttpFiltersComponents
import play.filters.cors.CORSComponents
import play.filters.gzip.GzipFilterComponents
import router.Routes

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
    with GzipFilterComponents
    with AssetsComponents with Wiring {
  def environmentMode: Mode = environment.mode

  // ── Router + filters ──────────────────────────────────────────────────────
  lazy val cspFilter: CspFilter = new CspFilter()(using materializer, executionContext)
  // Gzip last so it compresses the final rendered body. The pages are large
  // (the `/` listing is ~4.2 MB of uncompressed HTML — 200+ server-rendered
  // cards); gzip takes that to ~300 KB on the wire, the single biggest
  // mobile-load win. The filter is a no-op for
  // clients that don't send `Accept-Encoding: gzip` and skips already-
  // compressed payloads (images), so it only ever helps.
  override def httpFilters: Seq[EssentialFilter] =
    super.httpFilters :+ corsFilter :+ cspFilter :+ gzipFilter

  // Replace Play's default error handler with the truncation-tolerant
  // variant so `EntityStreamException` from client-side body cutoffs
  // (most of /uptime/img-event's beacon noise) lands at WARN rather
  // than ERROR — see the class comment for the full rationale.
  override lazy val httpErrorHandler: HttpErrorHandler = new TruncationTolerantHttpErrorHandler(
    environment,
    HttpErrorConfig(showDevErrors = environment.mode != Mode.Prod, playEditor = configuration.getOptional[String]("play.editor")),
    devContext.map(_.sourceMapper),
    Some(router)
  )

  lazy val router: Router = new Routes(httpErrorHandler, landingController, movieController, planController, authController, userStateController, healthController, uptimeController, legalController, facebookDataDeletionController, assets)

  start()

  applicationLifecycle.addStopHook(() => Future.successful(stop()))
}
