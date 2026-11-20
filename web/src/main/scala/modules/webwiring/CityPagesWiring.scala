package modules.webwiring

import controllers.{CityRequests, CrawlerController, CrawlerFiles, EncodedResponseCache, FilmPageController, LegacyRedirectController, MovieApiController}

/** ── The city pages ────────────────────────────────────────────────────────
 *  Everything under `/{city}/`: the listing, the film pages, the JSON the mobile apps read, robots.txt and the
 *  sitemaps, and the legacy addresses still answered — over one shared [[CityRequests]] and the gzip cache it serves
 *  conditional GETs from. */
trait CityPagesWiring { self: Wiring =>

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
      new services.facebook.MongoFacebookFetches(db.getCollection(services.facebook.FacebookFetches.Collection)), clock, stopwatch, ticks, threadPools = threadPools)))
  // What every `/{city}/…` controller shares: our cities, the city cookie, the
  // deployment's language and the conditional GET over the shared gzip cache.
  lazy val cityRequests = new CityRequests(controllerComponents.messagesApi, country, webReadModel, encodedResponseCache,
    now = () => movieControllerService.now(), build = processConfiguration.commit)
  lazy val movieController: controllers.MovieController = MovieControllerWiring.movieController(controllerComponents, cityRequests, movieControllerService,
    oauthProviders.keySet, environmentMode,
    minifier = minifier,
    // Read per render, so an `/admin/config` change reaches the page.
    pageTags = () => controllers.PageTags.from(processConfiguration),
    filmCards = filmCards,
    recordRender = webRenderMetrics.record)
  // The film pages, read per render like the listing.
  lazy val filmPageController = new FilmPageController(controllerComponents, cityRequests,
    new controllers.FilmDeepLinks(webReadModel, movieControllerService),
    oauthProviders.keySet, minifier, pageTags = () => controllers.PageTags.from(processConfiguration),
    filmPageServed = (userAgent, url) => facebookFetchRecorder.foreach(_.served(userAgent, url)))
  // The per-city JSON the mobile apps read (`/{city}/api/*`).
  lazy val movieApiController = new MovieApiController(controllerComponents, cityRequests, movieControllerService)
  // robots.txt and the sitemaps, cut from the live schedules and each city's own stamp.
  lazy val crawlerController = new CrawlerController(controllerComponents, cityRequests,
    CrawlerFiles.live(country, movieControllerService, webReadModel))
  // The addresses no longer minted but still answered, each redirected onto its current one.
  lazy val legacyRedirectController = new LegacyRedirectController(controllerComponents, cityRequests)
  /** The controllers the router serves the city routes with ([[modules.AppComponents.router]]). */
  lazy val cityPagesRouted = (movieController, filmPageController, movieApiController, crawlerController, legacyRedirectController)
}
