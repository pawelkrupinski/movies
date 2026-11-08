package modules.webwiring

import controllers.{CatalogController, CityRequests, ClientSupportController, CrawlerController, CrawlerFiles, EncodedResponseCache, HealthController, LandingController, LegacyRedirectController, LegalController, MovieApiController, MovieController, PickerController, PickerFragments, SupportController, WellKnownController}
import modules.Wiring

/** ── The public site ───────────────────────────────────────────────────────
 *  The controllers a visitor (or the mobile apps) reach without signing in:
 *  the landing page, the per-city listing + film pages, the app catalog, the
 *  legal/support pages and the operational `/health` + `.well-known` answers. */
trait ControllersWiring { self: Wiring =>

  // The deployment's DEFAULT, and ONLY, `Lang` — Poland → pl → default
  // `messages`; other countries → their own bundle. `LandingController` /
  // `MovieController` render every visitor in this; an explicit language
  // pick swaps the visible copy client-side instead (`shared.js`'s
  // `applyLanguage`), never by re-rendering server-side.
  lazy val deploymentLang: play.api.i18n.Lang = play.api.i18n.Lang(country.language)

  // The fixed deployment `Messages` — what every visitor-facing render uses,
  // plus the `DebugController`-adjacent, ops/crawler-facing ones.
  implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(deploymentLang))

  // Every country's `/` picker fragment, rendered once for the life of the process:
  // the landing embeds its own country's, `/api/picker/:country` serves the rest.
  lazy val pickerFragments = PickerFragments.rendered()
  lazy val landingController = new LandingController(controllerComponents, country, pickerFragments)
  lazy val pickerController  = new PickerController(controllerComponents, pickerFragments)
  // Where a cached page superseded by a read-model change is re-rendered while the
  // previous copy keeps being served (`EncodedResponseCache.gzippedBody`). Capped at
  // two: a render is ~1 s of CPU for the biggest cities, and an uncapped burst of
  // cities changing at once would compete with the request threads it exists to
  // spare. Single-flight per page bounds the queue to the pages actually held.
  lazy val pageRefreshExecutor: scala.concurrent.ExecutionContextExecutorService =
    managedResources.executor("page refresh")(tools.DaemonExecutors.boundedEC("page-refresh", maxConcurrent = 2))
  lazy val encodedResponseCache = new EncodedResponseCache(pageRefreshExecutor, () => clock.instant())
  lazy val minifier: tools.Minifier = tools.Minifier.forMode(environmentMode)
  lazy val filmCards = new controllers.CaffeineFilmCardFragments(controllers.CaffeineFilmCardFragments.DefaultMaxBytes)
  /** The film pages Facebook's crawler fetches, written to the fleet database for the worker's
   *  re-scrapes (`services.facebook.FacebookFetches`) — none without that database. */
  lazy val facebookFetchRecorder: Option[services.facebook.FacebookFetchRecorder] =
    fleetConnection.database.map(db => managedResources.stopping(new services.facebook.FacebookFetchRecorder(
      new services.facebook.MongoFacebookFetches(db.getCollection(services.facebook.FacebookFetches.Collection)), clock, stopwatch, ticks)))
  // What every `/{city}/…` controller shares: our cities, the city cookie, the
  // deployment's language and the conditional GET over the shared gzip cache.
  lazy val cityRequests = new CityRequests(controllerComponents.messagesApi, country, webReadModel, encodedResponseCache,
    now = () => movieControllerService.now(), build = processConfiguration.commit)
  lazy val movieController  = new MovieController(controllerComponents, cityRequests, movieControllerService, oauthProviders.keySet, environmentMode,
    minifier = minifier,
    // Read per render, so an `/admin/config` change reaches the page.
    pageTags = () => controllers.PageTags.from(processConfiguration),
    filmCards = filmCards,
    recordRender = webRenderMetrics.record,
    filmPageServed = (userAgent, url) => facebookFetchRecorder.foreach(_.served(userAgent, url)))
  // The per-city JSON the mobile apps read (`/{city}/api/*`).
  lazy val movieApiController = new MovieApiController(controllerComponents, cityRequests, movieControllerService)
  // robots.txt and the sitemaps, cut from the live schedules and each city's own stamp.
  lazy val crawlerController = new CrawlerController(controllerComponents, cityRequests,
    CrawlerFiles.live(country, movieControllerService, webReadModel))
  // The addresses no longer minted but still answered, each redirected onto its current one.
  lazy val legacyRedirectController = new LegacyRedirectController(controllerComponents, cityRequests)
  // Global country+city catalog for the mobile apps (`GET /api/catalog`), served
  // identically by every deployment — no per-country/read-model dependency.
  lazy val catalogController = new CatalogController(controllerComponents)
  lazy val clientSupportController = new ClientSupportController(controllerComponents)
  lazy val healthController = new HealthController(controllerComponents, readiness = () => webReadModel.hydrated,
    databases = Seq(mongoConnection, usersConnection))
  lazy val wellKnownController = new WellKnownController(controllerComponents)
  lazy val legalController   = new LegalController(controllerComponents, country)
  lazy val supportController = new SupportController(controllerComponents, country)
}
