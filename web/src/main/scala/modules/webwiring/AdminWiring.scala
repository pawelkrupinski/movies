package modules.webwiring

import controllers.{AdminAction, EnvConfigController, ImageEventController, TaskRunController, TasksController, UptimeBars, UptimeController, UptimeStreamController}
import services.{UptimeMonitor, UptimeTier}
import services.fallback.{FallbackStore, MongoFallbackStore}
import services.tasks.{BulkTaskResultStore, MongoBulkTaskResultStore, MongoTaskQueue, TaskQueue}

/** ── Operator pages ────────────────────────────────────────────────────────
 *  What sits behind the `AdminAction` gate: /uptime, /tasks and /admin/config,
 *  and the read-only views of worker-owned state they render. */
trait AdminWiring { self: Wiring =>

  // UptimeTier.Surfacing: the worker records all scraper + enrichment metrics
  // and writes them (batched) to the shared uptimeBuckets collection. This
  // serving process POLLS that collection every ~10s so /uptime reflects the
  // worker's activity — a fixed, bounded cost rather than reacting to every
  // write (the per-write change stream pegged the serving vCPU at multi-city
  // scrape volume). It also means this tier does NOT own the bucket TTL index and
  // will not rebuild it — see `UptimeTier`.
  lazy val uptimeMonitor = managedResources.register("uptime monitor",
    new UptimeMonitor(mongoConnection.database, tier = UptimeTier.Surfacing(), clock = clock,
      bucketStore = new UptimeMonitor.BucketStore(), ticks = ticks, threadPools = threadPools))(_.close())

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
  lazy val filmwebFallbackStore: FallbackStore = new MongoFallbackStore(mongoConnection.database, stopwatch)
  /** The worker's closure sweep's verdicts (`venue_closures`), for /uptime's dropped-venue section. */
  lazy val closureLedger: services.closure.ClosureLedgerReader =
    mongoConnection.database.fold[services.closure.ClosureLedgerReader](new services.closure.InMemoryClosureLedger)(new services.closure.MongoClosureLedger(_))
  private lazy val uptimeBars = new UptimeBars(uptimeMonitor, country.zone, clock)
  lazy val uptimeController = new UptimeController(controllerComponents, adminAction, uptimeMonitor, uptimeBars, filmwebFallbackStore, closureLedger, country)
  lazy val uptimeStreamController = new UptimeStreamController(controllerComponents, adminAction, uptimeMonitor, uptimeBars)(using materializer)
  // Not behind the admin gate: every visitor's browser reports its poster loads (ImageEventController.imgEvent says why).
  lazy val imageEventController = new ImageEventController(controllerComponents, new controllers.ImageLoadBeacon(uptimeMonitor))
  lazy val tasksController  = new TasksController(controllerComponents, adminAction, taskQueue, bulkTaskResultStore, country, clock)
  lazy val taskRunController = new TaskRunController(controllerComponents, adminAction, taskQueue, clock)
  // Live config: install the override cache as Env's source + publish web's knobs
  // to the shared registry, and serve the /admin/config page (see EnvConfigService).
  lazy val envConfigService = managedResources.stopping(new services.config.EnvConfigService(
    app          = "web",
    overrides    = new services.config.MongoEnvOverrideStore(mongoConnection.database),
    registry     = new services.config.MongoEnvRegistryStore(mongoConnection.database),
    env          = env,
    tickInterval = processConfiguration.configRefreshInterval(
      settings.ConfigRefreshInterval(scala.concurrent.duration.Duration(30L, "seconds"))), threadPools = threadPools))
  lazy val envConfigController = new EnvConfigController(controllerComponents, adminAction, envConfigService)
}
