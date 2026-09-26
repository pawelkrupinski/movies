package modules.wiring

import settings.{StagingPromoteInitialDelay, StagingPromoteInterval}

import modules.WorkerWiring
import services.events.StagingFilmEnriched
import services.staging.{FoldOnStagingEnriched, MongoStagingFolder, MongoStagingRepository, StagingDetailHandler, StagingFoldHandler, StagingFolder, StagingReaper, StagingRepository, StagingResolveImdbIdHandler, StagingResolveTmdbHandler, StagingSteps}
import services.tasks.TaskHandler

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** ── Staging incubation (resolve-then-fold) ──────────────────────────────────
 *  A newcomer in `pending_movies` walks the SAME steps the direct path runs,
 *  but each is now a durable queue task (StagingDetail → StagingResolveTmdb →
 *  StagingResolveImdbId → StagingFold) so it retries/backs off + dedups like
 *  every other task. `StagingSteps` holds the shared logic (detail-enrich,
 *  cache-free `resolveStagingRecord`, IMDb recovery); `StagingReaper` chains the
 *  steps (off `TaskFinished`) and is the periodic backstop. On the fold step a
 *  `StagingFilmEnriched` event drives the transactional folder (through
 *  `FoldOnStagingEnriched`, subscribed in the root), which merges the concluded
 *  film into `movies` and deletes its staging rows. */
trait StagingWiring { self: WorkerWiring =>

  lazy val stagingRepository: StagingRepository =
    new MongoStagingRepository(mongoConnection.database, normalizer = titleNormalizer, writeMetrics = taskMetrics)
  lazy val stagingFolder: StagingFolder = new MongoStagingFolder(mongoConnection, titleNormalizer, movieRepository, clock = clock)
  // What a concluded newcomer's `StagingFilmEnriched` does: the group-scoped fold,
  // then `announceResolvedNewMovie` for each brand-new film it introduced (resolution
  // outcome re-published, ratings enqueued). The decision is the class's; this is
  // only its collaborators.
  lazy val foldOnStagingEnriched =
    new FoldOnStagingEnriched(stagingFolder, stagingRepository, movieService.announceResolvedNewMovie)
  lazy val stagingSteps = new StagingSteps(
    stagingRepository, detailEnrichers, movieService.resolveStagingRecord, imdbIdResolver.findIdFor,
    freshnessStore, screeningTokens, clock)
  lazy val stagingHandlers: Seq[TaskHandler] = Seq(
    new StagingDetailHandler(stagingSteps),
    new StagingResolveTmdbHandler(stagingSteps),
    new StagingResolveImdbIdHandler(stagingSteps),
    new StagingFoldHandler(title => eventBus.publish(StagingFilmEnriched(title)))
  )
  private val StagingReaperInitialDelay =
    configuration.stagingPromoteInitialDelay(StagingPromoteInitialDelay(FiniteDuration(30L, TimeUnit.SECONDS)))
  private val StagingReaperInterval     =
    configuration.stagingPromoteInterval(StagingPromoteInterval(FiniteDuration(120L, TimeUnit.SECONDS)))
  lazy val stagingReaper = new StagingReaper(stagingSteps, taskQueue, stagingRepository,
    interval     = StagingReaperInterval,
    initialDelay = StagingReaperInitialDelay,
    runStore     = scheduledRunStore,
    metrics      = taskMetrics)
}
