package modules

import controllers.{AdminAction, AuthController, CatalogController, ClientSupportController, DebugCountries, DebugStack, DebugStreamController, EnvConfigController, FacebookDataDeletionController, GzippedResponseCache, HealthController, LandingController, LegalController, MetricsController, MovieController, MovieControllerService, PlanController, SupportController, TasksController, UptimeController, UserStateController, WebMovieMetrics, WellKnownController}
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.{MongoConnection, UptimeMonitor}
import services.auth.{AppleTokenValidator, AuthExchangeCodeStore, AuthExchangeCodes, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, InMemoryAuthExchangeCodeStore, MongoAuthExchangeCodeStore, OauthProvider}
import services.fallback.{FallbackStore, MongoFallbackStore}
import services.metrics.{WebCacheMetrics, WebHostMetrics, WebHttpMetrics, WebJvmMetrics}
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
  // A missing/unreachable Mongo is a hard boot failure everywhere except tests
  // (opt back into silent-degrade with MONGODB_OPTIONAL=true) — see
  // `MongoConnection`.
  private lazy val mongoRequired: Boolean = {
    val optedOut = Env.flag("MONGODB_OPTIONAL")
    MongoConnection.isRequired(environmentMode == Mode.Test, optedOut)
  }

  // ONE MongoClient behind every database view this process opens — this
  // country's corpus, and the shared users database below when that is a
  // different one. Built here rather than left to `MongoConnection.fromEnv` so
  // the second view BORROWS this pool: a client per view is a second connection
  // pool, Netty event loop and replica-set monitor thread set, which is the RSS
  // blow-up `MongoConnection` was written to avoid. `None` when MONGODB_URI is
  // unset — then there is no pool to share and each connection degrades on its
  // own, exactly as before. Owned HERE: `stop()` closes it after the connections
  // that borrowed it, since their own close() deliberately leaves it alone.
  private lazy val mongoSharedClient: Option[org.mongodb.scala.MongoClient] =
    MongoConnection.sharedClientFromEnv()

  lazy val mongoConnection: MongoConnection =
    MongoConnection.fromEnvForDb(models.Country.resolvedDbName, mongoRequired, sharedClient = mongoSharedClient)

  // ── Users ─────────────────────────────────────────────────────────────────
  // `users` + `userStates` come off `Country.usersDbName` rather than this
  // deployment's own database, so ONE account follows a visitor across every
  // country instead of one unrelated account per country wearing the same email. That
  // matters most where the session cookie now DOES travel: the three Showtimes
  // countries share one origin, so a `userId` minted under /uk arrives at /de,
  // and against a per-country database it would resolve to nobody — a silent
  // sign-out with the visitor's hidden films and /plan picks apparently gone.
  // Unset (`MONGODB_USERS_DB`), this IS this deployment's database and one
  // connection object serves both — no second boot probe of a database we are
  // already talking to.
  lazy val usersConnection: MongoConnection = Wiring.usersConnection(
    ownDbName   = models.Country.resolvedDbName,
    usersDbName = models.Country.usersDbName,
    own         = mongoConnection,
    openUsers   = MongoConnection.fromEnvForDb(_, mongoRequired, sharedClient = mongoSharedClient))

  // Caching decorators trim the Atlas RTT off the logged-in critical path.
  lazy val userRepository:      UserRepository      = new CachingUserRepository(new MongoUserRepository(usersConnection.database, fallbackToOwnInit = false))
  lazy val userStateRepository: UserStateRepository = new CachingUserStateRepository(new MongoUserStateRepository(usersConnection.database, fallbackToOwnInit = false))

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
  // /debug, per `debugMirrorConnection`'s no-fallback rule) instead of wedging boot
  // and every /debug load on the driver's 30s default.
  lazy val movieMirrorConnection: MongoConnection =
    Wiring.debugMirrorConnection(
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
  // Same read-stitch seam for the slots split. The worker owns the writes; web only
  // needs the repository so a film whose slots have moved to `movie_slots` still reads
  // complete.
  lazy val slotsRepository: services.movies.SlotsRepository =
    new services.movies.MongoSlotsRepository(movieMirrorConnection.database)
  /** The SERVING country's title rules. Passed explicitly so the web tier keys
   *  the same way the worker that wrote the corpus did; the per-country debug
   *  stacks below each get their OWN, since they read another country's database. */
  lazy val titleNormalizer: services.movies.TitleNormalizer =
    services.movies.TitleNormalizer.forCountry(models.Country.fromEnv)

  lazy val movieRepository: MovieRepository = new MongoMovieRepository(
    movieMirrorConnection.database, fallbackToOwnInit = false,
    screenings = Some(screeningsRepository), slots = Some(slotsRepository),
    normalizer = titleNormalizer)
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

  // One-shot sign-in codes for the two handoffs a session cookie cannot make:
  // the native apps' `kinowo://` deep link, and the country switch across the
  // kinowo.net / showtimes.cc domain boundary. They live in the SHARED users
  // database because the cross-domain hop mints on one pod and redeems on
  // ANOTHER — an in-process cache is exactly as unreachable there as the cookie
  // it stands in for. With no Mongo at all (local dev) the in-process store
  // keeps the native-app handoff working, since that one does start and finish
  // on the same pod.
  lazy val authExchangeCodes: AuthExchangeCodes = new AuthExchangeCodes(
    usersConnection.database.fold[AuthExchangeCodeStore](new InMemoryAuthExchangeCodeStore)(
      database => new MongoAuthExchangeCodeStore(Some(database))))

  // ── Controllers ───────────────────────────────────────────────────────────
  // View-rendering controllers take the deployment's fixed `Messages`
  // (`deploymentMessages`, implicit above) so their Twirl views resolve
  // `@messages("…")` in the country's language.
  lazy val landingController = new LandingController(controllerComponents, models.Country.fromEnv)
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
  lazy val stagingRepository: services.staging.StagingRepository = new services.staging.MongoStagingRepository(mongoConnection.database, titleNormalizer)
  // Read-only view of the worker-written `rating_cadence` collection for the
  // dev-only /debug/cadence page. Read from the MIRROR alongside `movies`: both
  // this and the attempt log below are read per /debug row-expand, so leaving
  // them on the prod tunnel would keep two ~110ms round-trips on a page whose
  // corpus read is already a LAN hop. They're mirrored collections, so this is
  // the same data, locally.
  lazy val ratingCadenceReader: services.cadence.RatingCadenceReader =
    new services.cadence.MongoRatingCadenceReader(movieMirrorConnection.database)
  // Whether the /debug stacks below are reading a COPY. Gates the navbar's
  // mirror-age badge: with no mirror configured every page reads the source, so
  // there is nothing that could be behind and nothing to render.
  private lazy val readingThroughMirror: Boolean = Env.get("MONGODB_MOVIES_MIRROR_URI").isDefined
  // How far behind that copy is. A sync that stops serves a page which renders,
  // times itself `now`, and is silently hours old — so the pages say their own
  // age (services.MirrorFreshness).
  private def mirrorFreshnessOf(connection: MongoConnection): services.MirrorFreshness =
    if (readingThroughMirror) new services.MongoMirrorFreshness(connection.database)
    else services.MirrorFreshness.notMirrored
  // Read-only view of the worker-written `enrichment_attempts` collection — the
  // last fetch per (source, film) behind the /debug row's expand section.
  lazy val enrichmentAttemptReader: services.attempts.EnrichmentAttemptReader =
    new services.attempts.MongoEnrichmentAttemptReader(movieMirrorConnection.database)

  // ── Dev-only per-country /debug data ─────────────────────────────────────────
  // The /debug pages read ONE country's Mongo db. In prod that's this
  // deployment's country (`bootDebugStack`). Locally in Dev the navbar's country
  // switch stays SAME-ORIGIN (`?country=xx`) and selects a per-country stack here,
  // instead of navigating to the other country's PROD host (which serves prod
  // mode and 404s every /debug route). Each extra country reads its OWN database
  // (`country.mongoDb`, NOT the MONGODB_DB override — that would pin every country
  // to one db) off ONE shared MongoClient, so N countries add N database views,
  // not N connection pools. When the read-mirror is configured those views come
  // from the MIRROR (which holds every country's db, not just the boot one), so
  // `?country=uk` is as fast as the boot country instead of paying the tunnel's
  // ~110ms per round-trip; unset → the MAIN Mongo, as before.
  //
  // The mirror is read UNCONDITIONALLY once configured, so a collection its sync
  // doesn't carry reads as permanently EMPTY — a blank page, no error. POINTING A
  // NEW READER HERE MEANS ADDING ITS COLLECTION TO `services.DebugMirror`, which
  // `MongoConnectionSpec` diffs against the sync's own list.
  private lazy val bootDebugStack: DebugStack = new DebugStack(
    models.Country.fromEnv, movieRepository, stagingRepository, taskQueue, ratingCadenceReader, enrichmentAttemptReader,
    readModelMovies       = () => webReadModel.allMovies(),
    readModelScreenings   = () => webReadModel.allScreenings(),
    readModelLastModified = () => webReadModel.lastModified,
    mirrorFreshness       = mirrorFreshnessOf(movieMirrorConnection))
  // One shared client for the extra countries: None in prod, when only one country
  // is deployed, or when MONGODB_URI is unset — then there are no extras and the
  // debug switch stays off.
  private lazy val debugExtraClient: Option[org.mongodb.scala.MongoClient] =
    if (environmentMode == Mode.Prod || models.Country.switchable.sizeIs <= 1) None
    else Env.get("MONGODB_MOVIES_MIRROR_URI")
      .map(MongoConnection.sharedClientFor(_, Some(MongoConnection.LocalMirrorTimeout)))
      .orElse(MongoConnection.sharedClientFromEnv())
  private lazy val debugExtraStacks: Seq[(models.Country, MongoConnection, DebugStack)] =
    debugExtraClient.toSeq.flatMap { client =>
      models.Country.switchable.filterNot(_ == models.Country.fromEnv).map { country =>
        val conn       = Wiring.debugMirrorConnection(
          Env.get("MONGODB_MOVIES_MIRROR_URI"),
          MongoConnection.mirrorForDb(_, country.mongoDb, sharedClient = Some(client)),
          MongoConnection.fromEnvForDb(country.mongoDb, required = false, sharedClient = Some(client)))
        val screenings = new services.movies.MongoScreeningsRepository(conn.database)
        val slots      = new services.movies.MongoSlotsRepository(conn.database)
        val reader     = new MongoReadModelRepository(conn.database)
        val stack = new DebugStack(country,
          // THIS stack's country, not the serving one: /debug reads another
          // country's database, and folding its titles with the serving country's
          // rules would key rows the way no worker ever wrote them.
          new MongoMovieRepository(conn.database, fallbackToOwnInit = false,
            screenings = Some(screenings), slots = Some(slots),
            normalizer = services.movies.TitleNormalizer.forCountry(country)),
          new services.staging.MongoStagingRepository(conn.database,
            normalizer = services.movies.TitleNormalizer.forCountry(country)),
          new MongoTaskQueue(conn.database),
          new services.cadence.MongoRatingCadenceReader(conn.database),
          new services.attempts.MongoEnrichmentAttemptReader(conn.database),
          readModelMovies       = () => reader.findAllMovies(),
          readModelScreenings   = () => reader.findAllScreenings(),
          readModelLastModified = () => java.time.Instant.now(),
          mirrorFreshness       = mirrorFreshnessOf(conn))
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
  lazy val clientSupportController = new ClientSupportController(controllerComponents)
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
  // Request rate / error rate / latency, recorded by `HttpMetricsFilter` on the
  // SAME registry the JVM collectors use — so it surfaces on the existing
  // /metrics body with no new endpoint. Replaces the dead Fly-proxy panels
  // (`fly_app_http_*`); see WebHttpMetrics for the cardinality rules.
  lazy val webHttpMetrics = new WebHttpMetrics(webJvmMetrics.registry, metricsCountry.code)
  // The MACHINE's free RAM and free disk, read from the process's own kernel.
  // Same registry again, same reason — and same cause: Fly's host metrics
  // (`fly_instance_memory_*`, `fly_volume_*`) died with the managed-Prometheus
  // token, and nothing else scrapes the web tier's host.
  // NOT lazy: nothing reads this object again — registering its callback gauges
  // on the registry IS its whole job — so a `lazy val` would never be forced and
  // the panels would stay as blank as they were with Fly's metrics gone.
  private val webHostMetrics = new WebHostMetrics(webJvmMetrics.registry, metricsCountry.code)
  // How much heap the gzipped-response cache is holding, against its budget. NOT
  // lazy for the same reason as the line above: registering the gauges is the
  // whole job. It forces `gzippedResponseCache`, which is only a map — no I/O, no
  // ordering constraint.
  private val webCacheMetrics = new WebCacheMetrics(webJvmMetrics.registry, metricsCountry.code, gzippedResponseCache)
  lazy val metricsController = new MetricsController(controllerComponents, uptimeMonitor, webMovieMetrics, webJvmMetrics, metricsCountry.code)
  // Read-only on the web side: the worker writes fallback state; the /uptime page's
  // Filmweb-fallback section reads it (hydrated from Mongo at boot).
  lazy val filmwebFallbackStore: FallbackStore = new MongoFallbackStore(mongoConnection.database)
  lazy val uptimeController = new UptimeController(controllerComponents, adminAction, uptimeMonitor, filmwebFallbackStore, models.Country.fromEnv)(using materializer)
  lazy val tasksController  = new TasksController(controllerComponents, adminAction, taskQueue, bulkTaskResultStore)
  // Dev-only SSE feed for the /debug live view; watches the SELECTED country's
  // `movies` + `pending_movies` via the same per-country stacks the /debug page
  // renders from. The live row's details cell ships empty (lazily fetched on
  // expand), so no cinema-URL snapshot is needed.
  lazy val debugStreamController = new DebugStreamController(controllerComponents, debugCountries, environmentMode)(using materializer)
  lazy val authController   = new AuthController(controllerComponents, oauthProviders, userRepository, authExchangeCodes, models.Country.fromEnv, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  lazy val accountDeletion   = new AccountDeletion(userRepository, userStateRepository)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepository, accountDeletion)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val supportController = new SupportController(controllerComponents)
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
    // A users database of its own is a second view on the shared client. When it
    // is this deployment's own database `usersConnection` IS `mongoConnection`,
    // and closing it here would be closing the primary early.
    if (usersConnection ne mongoConnection) usersConnection.close()
    // The /debug read-mirror owns its own MongoClient when distinct from the
    // shared prod connection (i.e. MONGODB_MOVIES_MIRROR_URI was set).
    if (movieMirrorConnection ne mongoConnection) movieMirrorConnection.close()
    // Dev-only per-country debug stacks share ONE client (built here); their
    // connections' own close() is a no-op, so close the shared client once.
    debugExtraClient.foreach(_.close())
    mongoConnection.close()
    // Every connection above BORROWED this client, so none of their own close()
    // calls touched it — it is owned here, and closed once, last.
    mongoSharedClient.foreach(_.close())
  }
}

object Wiring {
  /** Where a /debug data source reads from — the boot country's `movies` corpus,
   *  and equally each extra country's per-database stack.
   *  With `MONGODB_MOVIES_MIRROR_URI` set, always the local mirror `openMirror`
   *  builds — there is deliberately NO fall-back to the prod tunnel, even when
   *  the mirror is unreachable (then that connection is simply disabled and
   *  /debug renders empty). Unset → the shared `prod` connection. `prod` is
   *  by-name so a configured mirror never forces the primary connection here. */
  def debugMirrorConnection(mirrorUri: Option[String],
                            openMirror: String => MongoConnection,
                            prod: => MongoConnection): MongoConnection =
    mirrorUri.fold(prod)(openMirror)

  /** Which connection the `users` + `userStates` collections bind to.
   *
   *  A second database view costs a boot probe and a `close()` to get right, and
   *  the common case — `MONGODB_USERS_DB` unset, so the users database IS this
   *  deployment's own — needs neither: reuse the connection already open on that
   *  exact database. Only a genuinely DIFFERENT name opens a second view, which
   *  is why `own` is by-name.
   *
   *  Split out here rather than inlined as an `if` because it is the whole of the
   *  shared-account decision: get it backwards and every country silently keeps
   *  its own private copy of every account again, and no page renders any
   *  differently until someone switches country. */
  def usersConnection(ownDbName: String,
                      usersDbName: String,
                      own: => MongoConnection,
                      openUsers: String => MongoConnection): MongoConnection =
    if (usersDbName == ownDbName) own else openUsers(usersDbName)
}
