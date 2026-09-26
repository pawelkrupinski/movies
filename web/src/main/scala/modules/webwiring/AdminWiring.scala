package modules.webwiring

import controllers.{AdminAction, EnvConfigController, IdentityAdminController, TasksController, UptimeController}
import modules.Wiring
import services.UptimeMonitor
import services.fallback.{FallbackStore, MongoFallbackStore}
import services.tasks.{BulkTaskResultStore, MongoBulkTaskResultStore, MongoTaskQueue, TaskQueue}

/** ── Operator pages ────────────────────────────────────────────────────────
 *  What sits behind the `AdminAction` gate: /uptime, /tasks, /admin/config and /admin/identity,
 *  and the read-only views of worker-owned state they render. */
trait AdminWiring { self: Wiring =>

  // surfaceExternalWrites: the worker records all scraper + enrichment metrics
  // and writes them (batched) to the shared uptimeBuckets collection. This
  // serving process POLLS that collection every ~10s so /uptime reflects the
  // worker's activity — a fixed, bounded cost rather than reacting to every
  // write (the per-write change stream pegged the serving vCPU at multi-city
  // scrape volume).
  // `surfaceExternalWrites = true` also means this tier does NOT own the bucket TTL index and
  // will not rebuild it — see `UptimeMonitor.ownsIndexes`.
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database, surfaceExternalWrites = true, clock = clock)

  // Comma-separated allowlist of admin EMAILS permitted to reach the operational
  // pages (/uptime, /tasks) and the rehydrate trigger. Empty
  // (unset) → nobody is authorised, so those pages are closed by default. The
  // shared AdminAction gate resolves the session's user UUID and checks its email
  // against this set.
  lazy val adminAllowlist: settings.AdminAllowlist = processConfiguration.adminAllowlist
  lazy val adminAction = new AdminAction(controllerComponents.parsers.anyContent, userRepository, adminAllowlist)(using controllerComponents.executionContext)

  // ── Task queue (read-only here) ─────────────────────────────────────────────
  // The worker owns the queue; this process only reads it for the /tasks monitor
  // page. Same shared `tasks` collection, no writes originate here.
  lazy val taskQueue: TaskQueue = new MongoTaskQueue(mongoConnection.database)
  // Read-only here: the worker writes each bulk job's last outcome; the /tasks page
  // reads it to show what a Run button actually did (same shared Mongo as `tasks`).
  lazy val bulkTaskResultStore: BulkTaskResultStore = new MongoBulkTaskResultStore(mongoConnection.database)

  // Read-only on the web side: the worker writes fallback state; the /uptime page's
  // Filmweb-fallback section reads it (hydrated from Mongo at boot).
  lazy val filmwebFallbackStore: FallbackStore = new MongoFallbackStore(mongoConnection.database)
  lazy val uptimeController = new UptimeController(controllerComponents, adminAction, uptimeMonitor, filmwebFallbackStore, country, clock)(using materializer)
  lazy val tasksController  = new TasksController(controllerComponents, adminAction, taskQueue, bulkTaskResultStore, country, clock)
  // Live config: install the override cache as Env's source + publish web's knobs
  // to the shared registry, and serve the /admin/config page (see EnvConfigService).
  lazy val envConfigService = new services.config.EnvConfigService(
    app          = "web",
    overrides    = new services.config.MongoEnvOverrideStore(mongoConnection.database),
    registry     = new services.config.MongoEnvRegistryStore(mongoConnection.database),
    env          = env,
    tickInterval = processConfiguration.configRefreshInterval(
      settings.ConfigRefreshInterval(scala.concurrent.duration.Duration(30L, "seconds"))))
  lazy val envConfigController = new EnvConfigController(controllerComponents, adminAction, envConfigService)

  // Film identity (phase 3 of docs/design/identity-resolver.md): the admin diagnostic and the
  // emergency pins. The pins are written here; the worker's shadow run (`KINOWO_IDENTITY_SHADOW`)
  // reads them, and `shadowDecisions` reads back the latest run it persisted — nothing while the
  // country's shadow run is off.
  lazy val identityPins = new services.identity.Pins(new services.identity.MongoPinStore(mongoConnection.database), clock)
  lazy val shadowDecisions: services.identity.ShadowDecisions = new services.identity.ShadowRunStore(
    mongoConnection.database.fold[services.identity.ShadowRunBackend](new services.identity.InMemoryShadowRunBackend)(
      services.identity.MongoShadowRunBackend.reader), clock)
  lazy val identityAdminController =
    new IdentityAdminController(controllerComponents, adminAction, userRepository, identityPins, shadowDecisions)
}
