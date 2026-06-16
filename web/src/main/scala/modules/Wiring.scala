package modules

import controllers.{AdminAction, AdminTitleRulesController, AuthController, DebugStreamController, FacebookDataDeletionController, GzippedResponseCache, HealthController, LandingController, LegalController, MetricsController, MovieController, MovieControllerService, PlanController, TasksController, UptimeController, UserStateController}
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.{MongoConnection, UptimeMonitor}
import services.auth.{AppleTokenValidator, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, OauthProvider}
import services.fallback.{FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.movies.{MongoMovieRepository, MongoNormalizationReportRepository, MovieRepository, NormalizationReportRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelReader, WebReadModel}
import services.tasks.{MongoTaskQueue, TaskQueue}
import services.titlerules.{MongoTitleRulesRepository, TitleRulesCache, TitleRulesRepository}
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
  // re-push. `movieRepository` survives only for the on-demand /debug corpus dump and
  // the admin rule-merge preview (a one-off `findAll`, no change stream).
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
  lazy val movieRepository: MovieRepository = new MongoMovieRepository(movieMirrorConnection.database, fallbackToOwnInit = false)
  lazy val readModelRepository: ReadModelReader = new MongoReadModelRepository(mongoConnection.database)
  lazy val webReadModel: WebReadModel = new WebReadModel(readModelRepository)

  // Title-stripping rules, shared with the worker via Mongo. The web app reads
  // and never seeds (the worker owns seeding); it watches the change stream so
  // an admin edit takes effect here without a redeploy.
  lazy val titleRulesRepository: TitleRulesRepository = new MongoTitleRulesRepository(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesCache: TitleRulesCache = new TitleRulesCache(titleRulesRepository, seedIfEmpty = false)
  // The worker writes the backfill outcome here; the editor reads it.
  lazy val normalizationReportRepository: NormalizationReportRepository =
    new MongoNormalizationReportRepository(mongoConnection.database, fallbackToOwnInit = false)

  // Reads come straight from the read model; enrichment + projection happen in
  // the worker process.
  lazy val movieControllerService = new MovieControllerService(webReadModel)

  // ── Task queue (read-only here) ─────────────────────────────────────────────
  // The worker owns the queue; this process only reads it for the /tasks monitor
  // page. Same shared `tasks` collection, no writes originate here.
  lazy val taskQueue: TaskQueue = new MongoTaskQueue(mongoConnection.database)

  def controllerComponents: ControllerComponents
  def environmentMode: Mode
  implicit def materializer: org.apache.pekko.stream.Materializer

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
  lazy val landingController = new LandingController(controllerComponents)
  lazy val gzippedResponseCache = new GzippedResponseCache
  // Fetches + composites the per-film Open Graph share card. Its own poster
  // fetch (not the scraper's httoFetch) so slow cinema origins get a generous
  // connect budget instead of the fan-out's tight 5s.
  lazy val ogCardService    = new tools.OgCardService(new tools.HttpPosterFetch)
  // Comma-separated allowlist of admin EMAILS permitted to reach the operational
  // pages (title-rules editor, /uptime, /tasks) and the rehydrate trigger. Empty
  // (unset) → nobody is authorised, so those pages are closed by default. The
  // shared AdminAction gate resolves the session's user UUID and checks its email
  // against this set.
  lazy val adminAllowlist: Set[String] =
    Env.get("ADMIN_ALLOWLIST").map(_.split(",").map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty)
  lazy val adminAction = new AdminAction(controllerComponents.parsers.anyContent, userRepository, adminAllowlist)(using controllerComponents.executionContext)
  // The /debug "pending enrichment (staging)" table reads + live-watches this.
  lazy val stagingRepository: services.staging.StagingRepository = new services.staging.MongoStagingRepository(mongoConnection.database)
  lazy val movieController  = new MovieController(controllerComponents, movieControllerService, webReadModel, movieRepository, taskQueue, userRepository, adminAction, oauthProviders.keySet, environmentMode, gzippedResponseCache, ogCardService,
    cinemaSourceUrls = () => UptimeMonitor.cinemaUrls(uptimeMonitor.serviceTagsSnapshot()),
    stagingRepository = stagingRepository)
  lazy val planController   = new PlanController(controllerComponents, movieControllerService, userRepository, oauthProviders.keySet, environmentMode)
  lazy val healthController = new HealthController(controllerComponents)
  // Exposes the in-app /uptime health (Mongo `uptimeBuckets`) as Prometheus
  // gauges for the self-hosted Grafana — host metrics alone can't see a service
  // failing silently behind a fallback (the residential proxy → Zyte case).
  lazy val metricsController = new MetricsController(controllerComponents, uptimeMonitor)
  // Read-only on the web side: the worker writes fallback state; the /uptime/fallback
  // page reads it (hydrated from Mongo at boot).
  lazy val filmwebFallbackStore: FilmwebFallbackStore = new MongoFilmwebFallbackStore(mongoConnection.database)
  lazy val uptimeController = new UptimeController(controllerComponents, adminAction, uptimeMonitor, filmwebFallbackStore)(using materializer)
  lazy val tasksController  = new TasksController(controllerComponents, adminAction, taskQueue)
  // Dev-only SSE feed for the /debug live view; reuses the same on-demand
  // movieRepository + cinema-source-URL snapshot the /debug page renders from.
  lazy val debugStreamController = new DebugStreamController(controllerComponents, movieRepository, stagingRepository, environmentMode,
    () => UptimeMonitor.cinemaUrls(uptimeMonitor.serviceTagsSnapshot()))(using materializer)
  lazy val authController   = new AuthController(controllerComponents, oauthProviders, userRepository, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  lazy val accountDeletion   = new AccountDeletion(userRepository, userStateRepository)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepository, accountDeletion)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, Env.get("FACEBOOK_APP_SECRET"), userRepository, accountDeletion)
  lazy val adminTitleRulesController =
    new AdminTitleRulesController(controllerComponents, adminAction, titleRulesRepository, movieRepository, normalizationReportRepository)

  // Start the data layer. Force the Mongo connection at boot (so connection
  // errors surface in the boot timeline, not mid-request), then start the cache
  // — hydrate from Mongo + open the change stream that keeps it warm.
  protected def start(): Unit = {
    mongoConnection.database
    // Install rules (used by the admin rule-merge preview's normalisation).
    titleRulesCache.start()
    // Hydrate the read model from the derived collections + open their change
    // streams. (No `movies` watch — see the read-model wiring above.)
    webReadModel.start()
  }

  protected def stop(): Unit = {
    uptimeMonitor.close()
    webReadModel.stop()
    titleRulesCache.stop()
    // Each repository's close() is a no-op when it borrowed its database from
    // `mongoConnection` — closing the shared MongoClient is owned here.
    readModelRepository.close()
    movieRepository.close()
    titleRulesRepository.close()
    userRepository.close()
    userStateRepository.close()
    // The /debug read-mirror owns its own MongoClient when distinct from the
    // shared prod connection (i.e. MONGODB_MOVIES_MIRROR_URI was set).
    if (movieMirrorConnection ne mongoConnection) movieMirrorConnection.close()
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
