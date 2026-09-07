package modules.webwiring

import controllers.{AdminAction, EnvConfigController, TasksController, UptimeController}
import modules.Wiring
import services.UptimeMonitor
import services.fallback.{FallbackStore, MongoFallbackStore}
import services.tasks.{BulkTaskResultStore, MongoBulkTaskResultStore, MongoTaskQueue, TaskQueue}
import tools.Env

/** ── Operator pages ────────────────────────────────────────────────────────
 *  What sits behind the `AdminAction` gate: /uptime, /tasks and /admin/config,
 *  and the read-only views of worker-owned state they render. */
trait AdminWiring { self: Wiring =>

  // surfaceExternalWrites: the worker records all scraper + enrichment metrics
  // and writes them (batched) to the shared uptimeBuckets collection. This
  // serving process POLLS that collection every ~10s so /uptime reflects the
  // worker's activity — a fixed, bounded cost rather than reacting to every
  // write (the per-write change stream pegged the serving vCPU at multi-city
  // scrape volume).
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database, surfaceExternalWrites = true)

  // Comma-separated allowlist of admin EMAILS permitted to reach the operational
  // pages (/uptime, /tasks) and the rehydrate trigger. Empty
  // (unset) → nobody is authorised, so those pages are closed by default. The
  // shared AdminAction gate resolves the session's user UUID and checks its email
  // against this set.
  lazy val adminAllowlist: Set[String] =
    Env.get("ADMIN_ALLOWLIST").map(_.split(",").map(_.trim).filter(_.nonEmpty).toSet).getOrElse(Set.empty)
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
  lazy val uptimeController = new UptimeController(controllerComponents, adminAction, uptimeMonitor, filmwebFallbackStore, models.Country.fromEnv)(using materializer)
  lazy val tasksController  = new TasksController(controllerComponents, adminAction, taskQueue, bulkTaskResultStore)
  // Live config: install the override cache as Env's source + publish web's knobs
  // to the shared registry, and serve the /admin/config page (see EnvConfigService).
  lazy val envConfigService = new services.config.EnvConfigService(
    app          = "web",
    overrides    = new services.config.MongoEnvOverrideStore(mongoConnection.database),
    registry     = new services.config.MongoEnvRegistryStore(mongoConnection.database),
    tickInterval = scala.concurrent.duration.Duration(Env.positiveLong("KINOWO_CONFIG_REFRESH_SECONDS", 30L), "seconds"))
  lazy val envConfigController = new EnvConfigController(controllerComponents, adminAction, envConfigService)
}
