package controllers

import play.api.Mode
import play.api.test.Helpers
import services.movies.InMemoryMovieRepository
import services.readmodel.{TestReadModel, WebReadModel}
import services.tasks.{InMemoryTaskQueue, TaskQueue}

/** Shared builder for a fully-wired [[DebugController]]: an in-memory corpus,
 *  staging store and task queue behind a single-country [[DebugStack]], and the
 *  read model projected from the same `records`. Returns the concrete
 *  [[WebReadModel]] too, so a spec can `reload()` or inspect what `rehydrate`
 *  reloaded. */
object TestDebugController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    cinemaSourceUrls: Map[String, String] = Map.empty,
    adminAction: AdminAction = TestAdminAction(),
    taskQueue: TaskQueue = new InMemoryTaskQueue,
    // The on-demand corpus dump's source; defaults to an in-memory repo holding
    // the same `records`. Override to drive the /debug read path (e.g. to assert
    // the two scans run concurrently).
    movieRepository: Option[services.movies.MovieRepository] = None,
    stagingRepository: services.staging.StagingRepository = services.staging.StagingRepository.empty,
    ratingCadenceReader: services.cadence.RatingCadenceReader = services.cadence.RatingCadenceReader.empty,
    attemptReader: services.attempts.EnrichmentAttemptReader = services.attempts.EnrichmentAttemptReader.empty,
    // The per-country /debug stacks. Defaults to a single-country holder wrapping
    // the collaborators above; a spec exercising the Dev country switch injects a
    // multi-country `DebugCountries` instead.
    debugCountries: Option[DebugCountries] = None,
  ): (DebugController, WebReadModel) = {
    given play.api.i18n.Messages = testsupport.TestMessages.deployment
    val readModel = TestReadModel.fromRecords(records)
    val ctrl = new DebugController(
      cc               = Helpers.stubControllerComponents(),
      debugCountries   = debugCountries.getOrElse(DebugCountries.single(new DebugStack(
        models.Country.default,
        movieRepository.getOrElse(new InMemoryMovieRepository(records)),
        stagingRepository, taskQueue, ratingCadenceReader, attemptReader,
        readModelMovies       = () => readModel.allMovies(),
        readModelScreenings   = () => readModel.allScreenings(),
        readModelLastModified = () => readModel.lastModified))),
      readModel        = readModel,
      adminAction      = adminAction,
      environment      = mode,
      cinemaSourceUrls = () => cinemaSourceUrls,
    )
    (ctrl, readModel)
  }
}
