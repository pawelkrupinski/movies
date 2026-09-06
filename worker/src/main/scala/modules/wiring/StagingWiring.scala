package modules.wiring

import modules.WorkerWiring
import services.events.StagingFilmEnriched
import services.staging.{MongoStagingFolder, MongoStagingRepository, StagingDetailHandler, StagingFoldHandler, StagingFolder, StagingReaper, StagingRepository, StagingResolveImdbIdHandler, StagingResolveTmdbHandler, StagingSteps}
import services.tasks.TaskHandler
import tools.Env

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

/** ── Staging incubation (resolve-then-fold) ──────────────────────────────────
 *  A newcomer in `pending_movies` walks the SAME steps the direct path runs,
 *  but each is now a durable queue task (StagingDetail → StagingResolveTmdb →
 *  StagingResolveImdbId → StagingFold) so it retries/backs off + dedups like
 *  every other task. `StagingSteps` holds the shared logic (detail-enrich,
 *  cache-free `resolveStagingRecord`, IMDb recovery); `StagingReaper` chains the
 *  steps (off `TaskFinished`) and is the periodic backstop. On the fold step a
 *  `StagingFilmEnriched` event drives the transactional folder, which merges the
 *  concluded film into `movies` and deletes its staging rows. */
trait StagingWiring { self: WorkerWiring =>

  lazy val stagingRepository: StagingRepository =
    new MongoStagingRepository(mongoConnection.database, normalizer = titleNormalizer)
  lazy val stagingFolder: StagingFolder = new MongoStagingFolder(mongoConnection, titleNormalizer, movieRepository)
  lazy val stagingSteps = new StagingSteps(
    stagingRepository, detailEnrichers, movieService.resolveStagingRecord, imdbIdResolver.findIdFor,
    freshnessStore, screeningTokens, clock)
  lazy val stagingHandlers: Seq[TaskHandler] = Seq(
    new StagingDetailHandler(stagingSteps),
    new StagingResolveTmdbHandler(stagingSteps),
    new StagingResolveImdbIdHandler(stagingSteps),
    new StagingFoldHandler(title => eventBus.publish(StagingFilmEnriched(title)))
  )
  private val StagingReaperInitialDelay = Env.positiveLong("KINOWO_STAGING_PROMOTE_INITIAL_SECONDS", 30L)
  private val StagingReaperInterval     = Env.positiveLong("KINOWO_STAGING_PROMOTE_SECONDS", 120L)
  lazy val stagingReaper = new StagingReaper(stagingSteps, taskQueue, stagingRepository,
    interval     = FiniteDuration(StagingReaperInterval, TimeUnit.SECONDS),
    initialDelay = FiniteDuration(StagingReaperInitialDelay, TimeUnit.SECONDS),
    runStore     = scheduledRunStore)
}
