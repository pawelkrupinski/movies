package modules

import controllers.{AdminAction, AuthController, CatalogController, DebugCountries, DebugStack, DebugStreamController, EnvConfigController, FacebookDataDeletionController, GzippedResponseCache, HealthController, LandingController, LegalController, MetricsController, MovieController, MovieControllerService, PlanController, TasksController, UptimeController, UserStateController, WebMovieMetrics, WellKnownController}
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.{MongoConnection, UptimeMonitor}
import services.auth.{AppleTokenValidator, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, OauthProvider}
import services.fallback.{FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.metrics.WebJvmMetrics
import services.movies.{MongoMovieRepository, MovieRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelReader, WebReadModel}
import services.tasks.{BulkTaskResultStore, MongoBulkTaskResultStore, MongoTaskQueue, TaskQueue}
import services.users.{AccountDeletion, CachingUserRepository, CachingUserStateRepository, MongoUserRepository, MongoUserStateRepository, UserRepository, UserStateRepository}
import tools.{Env, HttpFetch, MonitoringHttpFetch, RealHttpFetch}

/**
 * Read/serving composition root. Builds the content-serving half of the app: the
 * shared data layer (Mongo + MovieCache, kept warm purely from Mongo via the
 * change stream — this process never scrapes), the user/auth stack, and the
 * controllers. The scrape + enrichment half lives in the separate `worker` app
 * (`modules.WorkerWiring`); the two share only the Mongo database.
 */
trait Wiring {
  // surfaceExternalWrites: the worker records all scraper + enrichment metrics
  // and writes them (batched) to the shared uptimeBuckets collection. This
  // serving process POLLS that collection every ~10s so /uptime reflects the
  // worker's activity — a fixed, bounded cost rather than reacting to every
  // write (the per-write change stream pegged the serving vCPU at multi-city
  // scrape volume).
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database, surfaceExternalWrites = true)
  // OAuth providers + token validators make outbound HTTP; the monitoring
  // wrapper records their latency on the same /uptime surface the worker feeds.
  lazy val httoFetch: HttpFetch = new MonitoringHttpFetch(new RealHttpFetch(), uptimeMonitor)

  // ── Mongo ─────────────────────────────────────────────────────────────────
  // Single shared MongoClient + database. A missing/unreachable Mongo is a hard
  // boot failure everywhere except tests (opt back into silent-degrade with
  // MONGODB_OPTIONAL=true) — see `MongoConnection`.
  lazy val mongoConnection: MongoConnection = {
    val optedOut = Env.get("MONGODB_OPTIONAL").exists(v => v == "true" || v == "1")
    MongoConnection.fromEnv(required = MongoConnection.isRequired(environmentMode == Mode.Test, optedOut))
  }

  // ── Users ─────────────────────────────────────────────────────────────────
  // Caching decorators trim the Atlas RTT off the logged-in critical path.
  lazy val userRepository:      UserRepository      = new CachingUserRepository(new MongoUserRepository(mongoConnection.database, fallbackToOwnInit = false))
  lazy val userStateRepository: UserStateRepository = new CachingUserStateRepository(new MongoUserStateRepository(mongoConnection.database, fallbackToOwnInit = false))

  // ── Denormalised read model ──────────────────────────────────────────────────
  // The serving app reads from the worker-maintained `web_movies` /
  // `web_screenings` collections via `WebReadModel`, kept warm by their change
  // streams. It deliberately does NOT watch `movies` — a showtime edit there
  // now reaches the web as one small screening-document delta, not a full-record
  // re-push. `movieRepository` survives only for the on-demand /debug corpus dump
  // (a one-off `findAll`, no change stream).
  //
  // Local read-mirror: `/debug`'s `movieRepository.findAll()` is a full `movies`
  // scan. Run locally it goes over the prod `flyctl` tunnel, where 1200+ full
  // documents take 30–60s and intermittently hit findAll's 60s timeout (→ an empty
  // /debug table). When `MONGODB_MOVIES_MIRROR_URI` points at a local Mongo
  // kept synced from prod by `scripts/local-mirror/mirror.sh`, movieRepository reads
  // that LAN mirror (~100ms) instead. movieRepository is read-only in this process
  // (the worker owns `movies` writes), and the task queue stays on the prod
  // connection below, so /debug re-enrich still works end-to-end: ↻ → prod
  // worker → prod `movies` → tailer → local mirror → /debug SSE. Unset (prod +
  // default dev) → reuse the shared prod connection, behaviour identical. Set →
  // ALWAYS read that local mirror and never fall back to the prod tunnel: an
  // unreachable mirror just disables movieRepository (an empty /debug) instead
  // of silently dumping the prod corpus over the slow tunnel.
  // Short timeouts on the mirror connection (`LocalMirrorTimeout`): it's a
  // loopback Mongo that answers in ~ms when healthy, so a few seconds of silence
  // means it's down. Capping the boot probe and the driver's per-request
  // server-selection makes a down/unreachable mirror disable fast (→ empty
  // /debug, per `movieConnection`'s no-fallback rule) instead of wedging boot
  // and every /debug load on the driver's 30s default.
  lazy val movieMirrorConnection: MongoConnection =
    Wiring.movieConnection(
      Env.get("MONGODB_MOVIES_MIRROR_URI"),
      MongoConnection.fromUri(_, required = false,
        probeTimeout           = MongoConnection.LocalMirrorTimeout,
        serverSelectionTimeout = Some(MongoConnection.LocalMirrorTimeout)),
      mongoConnection)
  // Showtimes split: /debug's movieRepository is read-only, so it only needs the
  // read-stitch — re-inject showtimes from `screenings` on the same connection it
  // reads `movies` from. The worker owns the backfill; here we just read.
  lazy val screeningsRepository: services.movies.ScreeningsRepository =
    new services.movies.MongoScreeningsRepository(movieMirrorConnection.database)
  lazy val movieRepository: MovieRepository = new MongoMovieRepository(
    movieMirrorConnection.database, fallbackToOwnInit = false,
    screenings = Some(screeningsRepository))
  lazy val readModelRepository: ReadModelReader = new MongoReadModelRepository(mongoConnection.database)
  lazy val webReadModel: WebReadModel = new WebReadModel(readModelRepository)

  // Reads come straight from the read model; enrichment + projection happen in
  // the worker process.
  lazy val movieControllerService = new MovieControllerService(webReadModel)

  // ── Task queue (read-only here) ─────────────────────────────────────────────
  // The worker owns the queue; this process only reads it for the /tasks monitor
  // page. Same shared `tasks` collection, no writes originate here.
  lazy val taskQueue: TaskQueue = new MongoTaskQueue(mongoConnection.database)
  // Read-only here: the worker writes each bulk job's last outcome; the /tasks page
  // reads it to show what a Run button actually did (same shared Mongo as `tasks`).
  lazy val bulkTaskResultStore: BulkTaskResultStore = new MongoBulkTaskResultStore(mongoConnection.database)

  def controllerComponents: ControllerComponents
  def environmentMode: Mode
  implicit def materializer: org.apache.pekko.stream.Materializer

  // Play's i18n API, provided by `BuiltInComponentsFromContext` (I18nComponents)
  // in `AppComponents`. Loads `conf/messages` (Polish default) + `messages.en`.
  def messagesApi: play.api.i18n.MessagesApi

  // The single `Messages` this deployment renders with — fixed at boot from the
  // country's language (Poland → pl → default `messages`; other countries → en).
  // A web deployment serves ONE country, so the locale never varies per request;
  // controllers inject this into their Twirl views instead of deriving a Lang
  // from `Accept-Language`.
  implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(play.api.i18n.Lang(models.Country.fromEnv.language)))

  // ── OAuth providers ──────────────────────────────────────────────────────
  // Each provider is wired only when its env vars are present. Missing keys →
  // provider absent → start route 404s and the navbar hides the login button.
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
  // View-rendering controllers take the deployment's fixed `Messages`
  // (`deploymentMessages`, implicit above) so their Twirl views resolve
  // `@messages("…")` in the country's language.
  lazy val landingController = new LandingController(controllerComponents)
  lazy val gzippedResponseCache = new GzippedResponseCache
  // Fetches + composites the per-film Open Graph share card. Its own poster
  // fetch (not the scraper's httoFetch) so slow cinema origins get a generous
  // connect budget instead of the fan-out's tight 5s.
  lazy val ogCardService     = new tools.OgCardService(new tools.HttpPosterFetch)
  lazy val cityOgCardService = new tools.CityOgCardService(new tools.HttpPosterFetch)
  // Comma-separated allowlist of admin EMAILS permitted to reach the operational
  // pages (/uptime, /tasks) and the rehydrate trigger. Empty
  // (unset) → nobody is authorised, so those pages are closed by default. The
  // shared AdminAction gate resolves the session's user UUID and checks its email
  // against this set.
  lazy val adminAllowlist: Set[String] =
    Env.get("ADMIN_ALLOWLIST").map(_.split(",").map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty)
  lazy val adminAction = new AdminAction(controllerComponents.parsers.anyContent, userRepository, adminAllowlist)(using controllerComponents.executionContext)
  // The /debug "pending enrichment (staging)" table reads + live-watches this.
  lazy val stagingRepository: services.staging.StagingRepository = new services.staging.MongoStagingRepository(mongoConnection.database)
  // Read-only view of the worker-written `rating_cadence` collection for the
  // dev-only /debug/cadence page (reads the primary Mongo, like the read model).
  lazy val ratingCadenceReader: services.cadence.RatingCadenceReader =
    new services.cadence.MongoRatingCadenceReader(mongoConnection.database)

  // ── Dev-only per-country /debug data ─────────────────────────────────────────
  // The /debug pages read ONE country's Mongo db. In prod that's this
  // deployment's country (`bootDebugStack`). Locally in Dev the navbar's country
  // switch stays SAME-ORIGIN (`?country=xx`) and selects a per-country stack here,
  // instead of navigating to the other country's PROD host (which serves prod
  // mode and 404s every /debug route). Each extra country reads its OWN database
  // (`country.mongoDb`, NOT the MONGODB_DB override — that would pin every country
  // to one db) off ONE shared MongoClient, so N countries add N database views,
  // not N connection pools. Its `movies` comes from the MAIN Mongo, since the
  // /debug read-mirror only holds the boot country's db.
  private lazy val bootDebugStack: DebugStack = new DebugStack(
    models.Country.fromEnv, movieRepository, stagingRepository, taskQueue, ratingCadenceReader,
    readModelMovies       = () => webReadModel.allMovies(),
    readModelScreenings   = () => webReadModel.allScreenings(),
    readModelLastModified = () => webReadModel.lastModified)
  // One shared client for the extra countries: None in prod, when only one country
  // is deployed, or when MONGODB_URI is unset — then there are no extras and the
  // debug switch stays off.
  private lazy val debugExtraClient: Option[org.mongodb.scala.MongoClient] =
    if (environmentMode == Mode.Prod || models.Country.switchable.sizeIs <= 1) None
    else MongoConnection.sharedClientFromEnv()
  private lazy val debugExtraStacks: Seq[(models.Country, MongoConnection, DebugStack)] =
    debugExtraClient.toSeq.flatMap { client =>
      models.Country.switchable.filterNot(_ == models.Country.fromEnv).map { country =>
        val conn       = MongoConnection.fromEnvForDb(country.mongoDb, required = false, sharedClient = Some(client))
        val screenings = new services.movies.MongoScreeningsRepository(conn.database)
        val reader     = new MongoReadModelRepository(conn.database)
        val stack = new DebugStack(country,
          new MongoMovieRepository(conn.database, fallbackToOwnInit = false, screenings = Some(screenings)),
          new services.staging.MongoStagingRepository(conn.database),
          new MongoTaskQueue(conn.database),
          new services.cadence.MongoRatingCadenceReader(conn.database),
          readModelMovies       = () => reader.findAllMovies(),
          readModelScreenings   = () => reader.findAllScreenings(),
          readModelLastModified = () => java.time.Instant.now())
        (country, conn, stack)
      }
    }
  lazy val debugCountries: DebugCountries =
    DebugCountries.of(bootDebugStack,
      debugExtraStacks.map { case (country, _, stack) => country -> stack }.toMap,
      devMode = environmentMode != Mode.Prod)

  lazy val movieController  = new MovieController(controllerComponents, movieControllerService, webReadModel, debugCountries, userRepository, adminAction, oauthProviders.keySet, environmentMode, gzippedResponseCache, ogCardService, cityOgCardService,
    cinemaSourceUrls = () => UptimeMonitor.cinemaUrls(uptimeMonitor.serviceTagsSnapshot()))
  lazy val planController   = new PlanController(controllerComponents, movieControllerService, userRepository, oauthProviders.keySet, environmentMode)
  // Global country+city catalog for the mobile apps (`GET /api/catalog`), served
  // identically by every deployment — no per-country/read-model dependency.
  lazy val catalogController = new CatalogController(controllerComponents)
  lazy val healthController = new HealthController(controllerComponents)
  lazy val wellKnownController = new WellKnownController(controllerComponents)
  // Exposes the in-app /uptime health (Mongo `uptimeBuckets`) as Prometheus
  // gauges for the self-hosted Grafana — host metrics alone can't see a service
  // failing silently behind a fallback (the residential proxy → Zyte case).
  // Samples per-city served-film counts every minute (all future / showing
  // tomorrow), appended to /metrics for Grafana to graph + alert on swings.
  // A web deployment serves exactly one country; tag its /metrics with that
  // country so its series line up with the worker's per-country series in Grafana.
  private val metricsCountry = models.Country.fromEnv
  lazy val webMovieMetrics = new WebMovieMetrics(movieControllerService, cities = metricsCountry.cities, country = metricsCountry.code)
  lazy val webJvmMetrics = new WebJvmMetrics
  lazy val metricsController = new MetricsController(controllerComponents, uptimeMonitor, webMovieMetrics, webJvmMetrics, metricsCountry.code)
  // Read-only on the web side: the worker writes fallback state; the /uptime page's
  // Filmweb-fallback section reads it (hydrated from Mongo at boot).
  lazy val filmwebFallbackStore: FilmwebFallbackStore = new MongoFilmwebFallbackStore(mongoConnection.database)
  lazy val uptimeController = new UptimeController(controllerComponents, adminAction, uptimeMonitor, filmwebFallbackStore)(using materializer)
  lazy val tasksController  = new TasksController(controllerComponents, adminAction, taskQueue, bulkTaskResultStore)
  // Dev-only SSE feed for the /debug live view; watches the SELECTED country's
  // `movies` + `pending_movies` via the same per-country stacks the /debug page
  // renders from. The live row's details cell ships empty (lazily fetched on
  // expand), so no cinema-URL snapshot is needed.
  lazy val debugStreamController = new DebugStreamController(controllerComponents, debugCountries, environmentMode)(using materializer)
  lazy val authController   = new AuthController(controllerComponents, oauthProviders, userRepository, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  lazy val accountDeletion   = new AccountDeletion(userRepository, userStateRepository)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepository, accountDeletion)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, Env.get("FACEBOOK_APP_SECRET"), userRepository, accountDeletion)
  // Live config: install the override cache as Env's source + publish web's knobs
  // to the shared registry, and serve the /admin/config page (see EnvConfigService).
  lazy val envConfigService = new services.config.EnvConfigService(
    app          = "web",
    overrides    = new services.config.MongoEnvOverrideStore(mongoConnection.database),
    registry     = new services.config.MongoEnvRegistryStore(mongoConnection.database),
    tickInterval = scala.concurrent.duration.Duration(Env.positiveLong("KINOWO_CONFIG_REFRESH_SECONDS", 30L), "seconds"))
  lazy val envConfigController = new EnvConfigController(controllerComponents, adminAction, envConfigService)

  // Start the data layer. Force the Mongo connection at boot (so connection
  // errors surface in the boot timeline, not mid-request), then start the cache
  // — hydrate from Mongo + open the change stream that keeps it warm.
  protected def start(): Unit = {
    mongoConnection.database
    // Install the override source first so boot-time knob reads already see flips.
    envConfigService.start()
    // Hydrate the read model from the derived collections + open their change
    // streams. (No `movies` watch — see the read-model wiring above.)
    webReadModel.start()
    // Sample per-city served-film counts once a minute for /metrics. Started
    // after the read model so the first sample reads a warm corpus.
    webMovieMetrics.start()
    // Force the Dev-only per-country debug stacks so their extra database views'
    // boot probes surface now, not on the first /debug?country= switch. A no-op
    // in prod (no extras) and cheap in Dev (one shared client, N db views).
    debugCountries
  }

  protected def stop(): Unit = {
    envConfigService.stop()
    uptimeMonitor.close()
    webMovieMetrics.stop()
    webReadModel.stop()
    // Each repository's close() is a no-op when it borrowed its database from
    // `mongoConnection` — closing the shared MongoClient is owned here.
    readModelRepository.close()
    movieRepository.close()
    userRepository.close()
    userStateRepository.close()
    // The /debug read-mirror owns its own MongoClient when distinct from the
    // shared prod connection (i.e. MONGODB_MOVIES_MIRROR_URI was set).
    if (movieMirrorConnection ne mongoConnection) movieMirrorConnection.close()
    // Dev-only per-country debug stacks share ONE client (built here); their
    // connections' own close() is a no-op, so close the shared client once.
    debugExtraClient.foreach(_.close())
    mongoConnection.close()
  }
}

object Wiring {
  /** The connection `movieRepository` reads the `movies` corpus from for /debug.
   *  With `MONGODB_MOVIES_MIRROR_URI` set, always the local mirror `openMirror`
   *  builds — there is deliberately NO fall-back to the prod tunnel, even when
   *  the mirror is unreachable (then that connection is simply disabled and
   *  /debug renders empty). Unset → the shared `prod` connection. `prod` is
   *  by-name so a configured mirror never forces the primary connection here. */
  def movieConnection(mirrorUri: Option[String],
                      openMirror: String => MongoConnection,
                      prod: => MongoConnection): MongoConnection =
    mirrorUri.fold(prod)(openMirror)
}
