package modules.wiring

import modules.WorkerWiring
import services.config.{EnvConfigService, MongoEnvOverrideStore, MongoEnvRegistryStore}
import services.tasks.{BulkRefreshHandler, BulkRefreshResult, BulkTaskResultStore, MongoBulkTaskResultStore, ResolveImdbIdHandler, ResolveTmdbHandler, TaskHandler, TaskType}
import tools.Env

import scala.concurrent.duration.DurationLong

/** What an operator drives from the web app: the live-config registry behind
 *  `/admin/config`, and the corpus-wide / per-row refresh handlers behind the
 *  `/tasks` and `/debug` buttons. */
trait OperatorWiring { self: WorkerWiring =>

  // Live config: install the Mongo override cache as Env's override source and
  // publish this process's (non-secret) knobs to the shared registry so the web
  // `/admin/config` page can list + flip them mid-flight. See EnvConfigService.
  lazy val envConfigService = new EnvConfigService(
    app       = "worker",
    overrides = new MongoEnvOverrideStore(mongoConnection.database),
    registry  = new MongoEnvRegistryStore(mongoConnection.database),
    tickInterval = Env.positiveLong("KINOWO_CONFIG_REFRESH_SECONDS", 30L).seconds)

  // Persists each operator-triggered bulk-refresh outcome so it survives the task
  // doc's instant deletion and the web `/tasks` page can show it. Written here by
  // BulkRefreshHandler; read by the web (same shared Mongo, like `tasks` itself).
  lazy val bulkTaskResultStore: BulkTaskResultStore =
    new MongoBulkTaskResultStore(mongoConnection.database)

  // Operator-triggered handlers — ALWAYS registered (not gated by
  // queueEnrichment): the web `/tasks` buttons enqueue a corpus-wide refresh and
  // the `/debug` row button enqueues a per-movie re-resolve, regardless of which
  // enrichment mode the worker runs. The bulk handlers call each source's
  // existing refreshAll / retryUnresolvedTmdb; ResolveTmdb forces one row and
  // lets the event chain re-run the downstream ratings.
  lazy val operatorHandlers: Seq[TaskHandler] = Seq(
    // TMDB re-enrich + settle have no per-source count tally, so they report a
    // generic "ran" message; the four `*Ratings` and OMDb walks return real counts.
    // Each operator button FORGETS that source's memoised resolutions before it
    // walks. Without this the walk re-derives from the very answers it exists to
    // re-check — the same trap the per-film re-enrich had, where a wrong URL was
    // replayed from `resolve_*` rather than re-probed (see `ResolutionCache.forgetAll`).
    new BulkRefreshHandler(TaskType.RefreshAllTmdb,       "TMDB",       () => { tmdbIdCache.forgetAll(); movieService.retryUnresolvedTmdb(); BulkRefreshResult.message("re-enrich dispatched for unresolved-TMDB rows") }, bulkTaskResultStore),
    new BulkRefreshHandler(TaskType.RefreshAllImdb,       "IMDb",       () => { imdbIdCache.forgetAll(); imdbRatings.refreshAllNow() },         bulkTaskResultStore),
    new BulkRefreshHandler(TaskType.RefreshAllMetacritic, "Metacritic", () => { mcLinkCache.forgetAll(); metascoreRatings.refreshAllNow() },    bulkTaskResultStore),
    new BulkRefreshHandler(TaskType.RefreshAllRt,         "RT",         () => { rtLinkCache.forgetAll(); rottenTomatoesRatings.refreshAllNow() }, bulkTaskResultStore),
    new BulkRefreshHandler(TaskType.SettleNow,            "Settle",     () => { movieService.settle(); BulkRefreshResult.message("consolidation complete") }, bulkTaskResultStore),
    new ResolveTmdbHandler(movieService.resolveTmdbOnce),
    // Movies-path IMDb-id recovery as a task (was inline off ImdbIdMissing) — so
    // the merge-retrigger path can re-kick it; resolveSync writes the id, and the
    // EnrichmentReaper then enqueues the now-eligible IMDb rating on its next pass.
    new ResolveImdbIdHandler(imdbIdResolver)
  ) ++
    // The Filmweb bulk-refresh button only when this country's Filmweb path is on.
    Option.when(filmwebEnabled)(
      new BulkRefreshHandler(TaskType.RefreshAllFilmweb, "Filmweb", () => { filmwebLinkCache.forgetAll(); filmwebRatings.refreshAllNow() }, bulkTaskResultStore)) ++
    // OMDb identifier sweep as a coarse task — only when the feature is on
    // (`omdbBackfill` is `Some`). Enqueued daily by `omdbBackfillReaper`, run here
    // off a background EC like the other corpus-wide refreshes.
    omdbBackfill.map(ob => new BulkRefreshHandler(TaskType.RefreshAllOmdb, "OMDb", () => ob.refreshAllNow(), bulkTaskResultStore)).toSeq
}
