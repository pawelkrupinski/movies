package modules

import controllers.{AdminTitleRulesController, AuthController, FacebookDataDeletionController, GzippedResponseCache, HealthController, LandingController, LegalController, MovieController, MovieControllerService, PlanController, TasksController, UptimeController, UserStateController}
import play.api.Mode
import play.api.mvc.ControllerComponents
import services.{MongoConnection, UptimeMonitor}
import services.auth.{AppleTokenValidator, FacebookOauthProvider, FacebookTokenValidator, GoogleOauthProvider, GoogleTokenValidator, OauthProvider}
import services.fallback.{FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.movies.{MongoMovieRepo, MongoNormalizationReportRepo, MovieRepo, NormalizationReportRepo}
import services.readmodel.{MongoReadModelRepo, ReadModelReader, WebReadModel}
import services.tasks.{MongoTaskQueue, TaskQueue}
import services.titlerules.{MongoTitleRulesRepo, TitleRulesCache, TitleRulesRepo}
import services.users.{AccountDeletion, CachingUserRepo, CachingUserStateRepo, MongoUserRepo, MongoUserStateRepo, UserRepo, UserStateRepo}
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
  lazy val userRepo:      UserRepo      = new CachingUserRepo(new MongoUserRepo(mongoConnection.database, fallbackToOwnInit = false))
  lazy val userStateRepo: UserStateRepo = new CachingUserStateRepo(new MongoUserStateRepo(mongoConnection.database, fallbackToOwnInit = false))

  // ── Denormalised read model ──────────────────────────────────────────────────
  // The serving app reads from the worker-maintained `web_movies` /
  // `web_screenings` collections via `WebReadModel`, kept warm by their change
  // streams. It deliberately does NOT watch `movies` — a showtime edit there
  // now reaches the web as one small screening-doc delta, not a full-record
  // re-push. `movieRepo` survives only for the on-demand /debug corpus dump and
  // the admin rule-merge preview (a one-off `findAll`, no change stream).
  lazy val movieRepo: MovieRepo = new MongoMovieRepo(mongoConnection.database, fallbackToOwnInit = false)
  lazy val readModelRepo: ReadModelReader = new MongoReadModelRepo(mongoConnection.database)
  lazy val webReadModel: WebReadModel = new WebReadModel(readModelRepo)

  // Title-stripping rules, shared with the worker via Mongo. The web app reads
  // and never seeds (the worker owns seeding); it watches the change stream so
  // an admin edit takes effect here without a redeploy.
  lazy val titleRulesRepo: TitleRulesRepo = new MongoTitleRulesRepo(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesCache: TitleRulesCache = new TitleRulesCache(titleRulesRepo, seedIfEmpty = false)
  // The worker writes the backfill outcome here; the editor reads it.
  lazy val normalizationReportRepo: NormalizationReportRepo =
    new MongoNormalizationReportRepo(mongoConnection.database, fallbackToOwnInit = false)

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
  lazy val movieController  = new MovieController(controllerComponents, movieControllerService, webReadModel, movieRepo, userRepo, oauthProviders.keySet, environmentMode, gzippedResponseCache, ogCardService)
  lazy val planController   = new PlanController(controllerComponents, movieControllerService, userRepo, oauthProviders.keySet, environmentMode)
  lazy val healthController = new HealthController(controllerComponents)
  // Read-only on the web side: the worker writes fallback state; the /uptime/fallback
  // page reads it (hydrated from Mongo at boot).
  lazy val filmwebFallbackStore: FilmwebFallbackStore = new MongoFilmwebFallbackStore(mongoConnection.database)
  lazy val uptimeController = new UptimeController(controllerComponents, uptimeMonitor, filmwebFallbackStore)(using materializer)
  lazy val tasksController  = new TasksController(controllerComponents, taskQueue)
  lazy val authController   = new AuthController(controllerComponents, oauthProviders, userRepo, googleTokenValidator, facebookTokenValidator, appleTokenValidator)
  lazy val accountDeletion   = new AccountDeletion(userRepo, userStateRepo)
  lazy val userStateController = new UserStateController(controllerComponents, userStateRepo, accountDeletion)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val facebookDataDeletionController =
    new FacebookDataDeletionController(controllerComponents, Env.get("FACEBOOK_APP_SECRET"), userRepo, accountDeletion)
  // Comma-separated allowlist of admin EMAILS permitted to edit title rules.
  // Empty (unset) → nobody is authorised, so the editor is closed by default.
  lazy val adminAllowlist: Set[String] =
    Env.get("ADMIN_ALLOWLIST").map(_.split(",").map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty)
  lazy val adminTitleRulesController =
    new AdminTitleRulesController(controllerComponents, titleRulesRepo, movieRepo, normalizationReportRepo, userRepo, adminAllowlist)

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
    // Each repo's close() is a no-op when it borrowed its database from
    // `mongoConnection` — closing the shared MongoClient is owned here.
    readModelRepo.close()
    movieRepo.close()
    titleRulesRepo.close()
    userRepo.close()
    userStateRepo.close()
    mongoConnection.close()
  }
}
