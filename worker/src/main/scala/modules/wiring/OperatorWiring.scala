package modules.wiring

import scala.concurrent.duration.DurationLong

import services.config.{EnvConfigService, MongoEnvOverrideStore, MongoEnvRegistryStore}
import services.enrichment.ResolveImdbIdHandler
import services.tasks.{BulkRefreshHandler, BulkRefreshResult, BulkTaskResultStore, MongoBulkTaskResultStore, TaskHandler}
import services.tasktypes.TaskType
import settings.ConfigRefreshInterval

/** What an operator drives from the web app: the live-config registry behind
 *  `/admin/config`, and the corpus-wide / per-row refresh handlers behind the
 *  `/tasks` and `/debug` buttons. */
trait OperatorWiring { self: WorkerWiring =>

  // Live config: install the Mongo override cache as Env's override source and
  // publish this process's (non-secret) knobs to the shared registry so the web
  // `/admin/config` page can list + flip them mid-flight. See EnvConfigService.
  lazy val envConfigService = managedResources.stopping(new EnvConfigService(
    app       = "worker",
    overrides = new MongoEnvOverrideStore(mongoConnection.database),
    registry  = new MongoEnvRegistryStore(mongoConnection.database),
    env          = env,
    tickInterval = configuration.configRefreshInterval(ConfigRefreshInterval(30.seconds)),
    ticks        = ticks,
    threadPools  = threadPools))

  // Persists each operator-triggered bulk-refresh outcome so it survives the task
  // doc's instant deletion and the web `/tasks` page can show it. Written here by
  // BulkRefreshHandler; read by the web (same shared Mongo, like `tasks` itself).
  lazy val bulkTaskResultStore: BulkTaskResultStore =
    new MongoBulkTaskResultStore(mongoConnection.database)

  // The stored sources an operator can have fetched again for given keys (`RefreshSource`, enqueued by
  // scripts.EnqueueSourceRefresh): the identity model's TMDB films, and each rating source's rows by TMDB id, its
  // memoised links forgotten first as its `/tasks` button forgets them all. Filmweb only where this country enables it.
  lazy val refreshableSources: services.tasks.RefreshableSources = {
    import services.refresh.RatingRefreshSource
    import services.tasks.RefreshableSources
    new RefreshableSources(Seq(
      identityTmdbFilmRefresh,
      new RatingRefreshSource(RefreshableSources.Imdb,       imdbRatings,           movieCache, imdbIdCache),
      new RatingRefreshSource(RefreshableSources.Metacritic, metascoreRatings,      movieCache, mcLinkCache),
      new RatingRefreshSource(RefreshableSources.Rt,         rottenTomatoesRatings, movieCache, rtLinkCache)) ++
      Option.when(filmwebEnabled)(new RatingRefreshSource(RefreshableSources.Filmweb, filmwebRatings, movieCache, filmwebLinkCache)))
  }

  // Operator-triggered handlers — ALWAYS registered (not gated by queueEnrichment):
  // the web `/tasks` buttons enqueue a corpus-wide refresh, regardless of which
  // enrichment mode the worker runs. Each bulk handler calls its source's refreshAll.
  lazy val operatorHandlers: Seq[TaskHandler] = Seq(
    // The projection has no per-source count tally, so it reports a generic message;
    // the four `*Ratings` and OMDb walks return real counts.
    // Each operator button FORGETS that source's memoised resolutions before it
    // walks. Without this the walk re-derives from the very answers it exists to
    // re-check — the same trap the per-film re-enrich had, where a wrong URL was
    // replayed from `resolve_*` rather than re-probed (see `ResolutionCache.forgetAll`).
    new BulkRefreshHandler(TaskType.RefreshAllImdb,       "IMDb",       () => { imdbIdCache.forgetAll(); imdbRatings.refreshAllNow() },         bulkTaskResultStore, clock, threadPools.virtualThreadEC("bulk-refresh")),
    new BulkRefreshHandler(TaskType.RefreshAllMetacritic, "Metacritic", () => { mcLinkCache.forgetAll(); metascoreRatings.refreshAllNow() },    bulkTaskResultStore, clock, threadPools.virtualThreadEC("bulk-refresh")),
    new BulkRefreshHandler(TaskType.RefreshAllRt,         "RT",         () => { rtLinkCache.forgetAll(); rottenTomatoesRatings.refreshAllNow() }, bulkTaskResultStore, clock, threadPools.virtualThreadEC("bulk-refresh")),
    new BulkRefreshHandler(TaskType.SettleNow,            "Settle",     () => { val _ = identityProjection.projectQuietly(); BulkRefreshResult.message("projection complete") }, bulkTaskResultStore, clock, threadPools.virtualThreadEC("bulk-refresh")),
    // Movies-path IMDb-id recovery as a task (was inline off ImdbIdMissing) — so
    // the merge-retrigger path can re-kick it; resolveSync writes the id, and the
    // EnrichmentReaper then enqueues the now-eligible IMDb rating on its next pass.
    new ResolveImdbIdHandler(imdbIdRecovery),
    new services.refresh.RefreshSourceHandler(refreshableSources, bulkTaskResultStore, clock)
  ) ++
    // The Filmweb bulk-refresh button only when this country's Filmweb path is on.
    Option.when(filmwebEnabled)(
      new BulkRefreshHandler(TaskType.RefreshAllFilmweb, "Filmweb", () => { filmwebLinkCache.forgetAll(); filmwebRatings.refreshAllNow() }, bulkTaskResultStore, clock, threadPools.virtualThreadEC("bulk-refresh"))) ++
    // OMDb identifier sweep as a coarse task — only when the feature is on
    // (`omdbBackfill` is `Some`). Enqueued daily by `omdbBackfillReaper`, run here
    // off a background EC like the other corpus-wide refreshes.
    omdbBackfill.map(ob => new BulkRefreshHandler(TaskType.RefreshAllOmdb, "OMDb", () => ob.refreshAllNow(), bulkTaskResultStore, clock, threadPools.virtualThreadEC("bulk-refresh"))).toSeq
}
