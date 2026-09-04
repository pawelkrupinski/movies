package modules

import controllers.{AssetsComponents, RetiredSiteController, TruncationTolerantHttpErrorHandler, WellKnownController}
import play.api.ApplicationLoader.Context
import play.api.http.{HttpErrorConfig, HttpErrorHandler}
import play.api.mvc.{EssentialFilter, Handler}
import play.api.routing.Router
import play.api.routing.sird._
import play.api._
import models.Country
import tools.Env
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
    val country = Country.fromEnv
    val mounted = AppLoader.mountedAt(adjusted, country)
    // KINOWO_RETIRED picks a DIFFERENT composition root, not a different code
    // path inside the usual one — see `RetiredComponents` for why a retired host
    // must not be able to reach the database at all. Where it moved TO is not
    // configured alongside it: the country already knows its live address
    // (`Country.webOrigin`), and a second spelling of it is a second thing to
    // get wrong.
    if (Env.flag("KINOWO_RETIRED")) new RetiredComponents(mounted, country).application
    else                            new AppComponents(mounted).application
  }
}

object AppLoader {

  /** Mount the whole application at its country's [[models.Country.mountPath]].
   *
   *  WHY THIS EXISTS. Every deployment but Poland's now shares one domain and
   *  are told apart by a path segment — `showtimes.cc/uk/kent/`, not
   *  `uk.showtimes.cc/kent/` — while Poland keeps `kinowo.net/poznan/` at the
   *  root. One process still serves exactly one country against one database;
   *  only the address moved. `play.http.context` is Play's own name for that
   *  address, and setting it does BOTH halves of the job: the router matches
   *  incoming paths under the prefix, and every reverse route emits it (the
   *  generated `withPrefix` publishes it to `router.RoutesPrefix`, which every
   *  `controllers.routes.*` reverse controller reads).
   *
   *  It is written into the configuration here, from the country, rather than
   *  spelled out per Kubernetes overlay: `KINOWO_COUNTRY` already selects the
   *  database, the language and the city list, and the mount point is one more
   *  thing that follows from it. A literal per overlay is a fourth place to get
   *  the same fact wrong.
   *
   *  The cookie PATHS have to be set alongside it either way: Play defaults both
   *  to `${play.http.context}` in `reference.conf`, but that substitution
   *  resolves when the file is parsed, so overriding the context alone would
   *  leave them behind at `/`. They then go to DIFFERENT places, and the split is
   *  the point.
   *
   *  The SESSION cookie stays at the host root, deliberately, so that the
   *  countries sharing a domain share a sign-in: `/uk`, `/de` and `/us` are one
   *  origin, and a visitor who signs in on one of them is the same person on the
   *  next. What that cookie carries is a `userId`, and it resolves against the
   *  SHARED users database (`models.Country.usersDbName`), so it means the same
   *  account whichever mount reads it — scoping it per country would not protect
   *  anything, it would just sign the visitor out for crossing a path segment.
   *  (The remembered CITY, which this comment used to worry about in the same
   *  breath, was never riding on this: `MovieController` sets the `city` cookie
   *  with `path = country.mountPath` of its own accord, and still does.)
   *
   *  The FLASH cookie stays scoped to the mount point. It is a one-shot message
   *  attached to a single redirect within one deployment; there is no such thing
   *  as a flash that means anything one country over, so letting `/uk`'s pop on
   *  `/de` would be a bug with nothing on the other side of the trade. */
  /** The two endpoints that must answer at the HOST ROOT no matter where the
   *  application is mounted, layered IN FRONT of the mounted router.
   *
   *  Everything else about this deployment moved one segment down, and that is
   *  the point — but these two are not fetched by a browser following a link.
   *  `/health` is hit by the kubelet on the POD's own address (startup,
   *  readiness and liveness probes in `movies-gitops/web/base/all.yaml`), and
   *  `/metrics` by a Prometheus that runs outside the cluster and scrapes
   *  `10.20.0.12:<nodePort>/metrics` directly. Neither goes through Caddy, so
   *  neither ever sees the country prefix, and mounting them under it would
   *  crashloop every non-Polish pod and blank its metrics — the two failures
   *  that look like an outage rather than a routing change.
   *
   *  Layered unconditionally rather than only for a prefixed country, so there
   *  is ONE routing shape to reason about: at the root the mounted router serves
   *  the same two paths through the same actions, and the overlay is a no-op. */
  private[modules] def rootOperationalRoutes(health: => Handler, metrics: => Handler): Router =
    Router.from {
      case GET(p"/health")  => health
      case GET(p"/metrics") => metrics
    }

  /** Everything a RETIRED deployment routes, below the two operational
   *  endpoints above (see [[controllers.RetiredSiteController]] for what each
   *  one answers, and why only two of them render a page).
   *
   *  A TOTAL function, ending in a catch-all: a retired host has no 404s to
   *  give. Every path it does not recognise is a path the live site might, so
   *  the client is sent there to find out rather than told it does not exist —
   *  which also means this router never needs updating when the live site grows
   *  a page.
   *
   *  `asset` is passed in rather than an `Assets` controller taken, so the whole
   *  table can be exercised in a spec without an asset pipeline behind it. */
  private[modules] def retiredRoutes(
      site:      RetiredSiteController,
      wellKnown: WellKnownController,
      asset:     String => Handler): Router =
    Router.from {
      // An app installed before the move still resolves its Universal Links /
      // App Links against this host.
      case GET(p"/.well-known/apple-app-site-association") => wellKnown.appleAppSiteAssociation
      case GET(p"/.well-known/assetlinks.json")            => wellKnown.assetLinks
      case GET(p"/assets/$file*")                          => asset(file)
      case GET(p"/")                                       => site.landing
      case GET(p"/$slug/")                                 => site.city(slug)
      case _                                               => site.elsewhere
    }

  private[modules] def mountedAt(context: Context, country: Country): Context = {
    val mountPath = country.mountPath
    context.copy(initialConfiguration = Configuration(
      "play.http.context"      -> mountPath,
      // Shared across the countries on this domain — see above.
      "play.http.session.path" -> "/",
      "play.http.flash.path"   -> mountPath,
    ).withFallback(context.initialConfiguration))
  }
}

/**
 * Single wiring class. Every dependency the app needs is constructed here as a
 * `lazy val` (so the order in the file doesn't matter — references resolve on
 * first use) and side-effecting components (cache hydrate, change-stream
 * watches, event subscriptions) are forced at the bottom in the order they
 * need to fire.
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
  lazy val renamedCityRedirectFilter: RenamedCityRedirectFilter =
    new RenamedCityRedirectFilter(models.Country.fromEnv.mountPath)(using materializer)
  lazy val httpMetricsFilter: HttpMetricsFilter = new HttpMetricsFilter(webHttpMetrics)(using executionContext)
  // Metrics FIRST (outermost) so the latency it records is the whole chain —
  // including gzip of a multi-MB body — and so a request rejected by Play's own
  // allowed-hosts/CSRF filters still lands on the error-rate panel. Any inner
  // position would silently exclude exactly the failures worth alerting on.
  // Gzip last so it compresses the final rendered body. The pages are large
  // (the `/` listing is ~4.2 MB of uncompressed HTML — 200+ server-rendered
  // cards); gzip takes that to ~300 KB on the wire, the single biggest
  // mobile-load win. The filter is a no-op for
  // clients that don't send `Accept-Encoding: gzip` and skips already-
  // compressed payloads (images), so it only ever helps.
  // The renamed-city 301 sits INSIDE the metrics filter (so the redirect is
  // counted like any other response) but ahead of the router, since its whole
  // job is to answer a path the router would 404.
  override def httpFilters: Seq[EssentialFilter] =
    (httpMetricsFilter +: super.httpFilters) :+ renamedCityRedirectFilter :+ corsFilter :+ cspFilter :+ gzipFilter

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

  // `.withPrefix` rather than the plain constructor: compile-time DI skips
  // Play's `RoutesProvider`, which is what applies `play.http.context` under
  // runtime DI. Calling it here is also what publishes the prefix to
  // `router.RoutesPrefix`, so every reverse route (`controllers.routes.*`, the
  // asset URLs in every template) emits the deployment's mount point too.
  lazy val router: Router =
    AppLoader.rootOperationalRoutes(healthController.check, metricsController.metrics)
      .orElse(new Routes(httpErrorHandler, landingController, wellKnownController, movieController, catalogController, clientSupportController, debugStreamController, authController, userStateController, healthController, metricsController, uptimeController, tasksController, legalController, supportController, facebookDataDeletionController, envConfigController, assets)
        .withPrefix(httpConfiguration.context))

  start()

  applicationLifecycle.addStopHook(() => Future.successful(stop()))
}
