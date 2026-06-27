package controllers

import play.api.Mode
import play.api.test.Helpers
import services.movies.InMemoryMovieRepository
import services.readmodel.{TestReadModel, WebReadModel}
import services.tasks.{InMemoryTaskQueue, TaskQueue}

/** Shared builder for a fully-wired [[MovieController]] backed by an in-memory
 *  read model and no live HTTP — the seam every controller spec needs. Pass the
 *  records the read model should hold; get back the controller and the concrete
 *  [[WebReadModel]] so a test can `reload()` to bump the mtime. */
object TestMovieController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    cinemaSourceUrls: Map[String, String] = Map.empty,
    adminAction: AdminAction = TestAdminAction(),
    taskQueue: TaskQueue = new InMemoryTaskQueue,
    // On-demand corpus dump only (dev /debug); defaults to an in-memory repo
    // holding the same `records`. Override to drive the /debug read path (e.g.
    // to assert the two scans run concurrently).
    movieRepository: Option[services.movies.MovieRepository] = None,
    stagingRepository: services.staging.StagingRepository = services.staging.StagingRepository.empty,
    ratingCadenceReader: services.cadence.RatingCadenceReader = services.cadence.RatingCadenceReader.empty,
  ): (MovieController, WebReadModel) = {
    val readModel = TestReadModel.fromRecords(records)
    val ctrl  = new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = new MovieControllerService(readModel),
      readModel              = readModel,
      movieRepository        = movieRepository.getOrElse(new InMemoryMovieRepository(records)),
      taskQueue              = taskQueue,
      userRepository               = new services.users.InMemoryUserRepository,
      adminAction            = adminAction,
      oauthProviders         = Set.empty,
      environment            = mode,
      responseCache          = new GzippedResponseCache,
      // No live HTTP: a poster fetch that returns nothing decodes to None, so
      // the OG card falls back to text-only — fine for controller specs that
      // don't assert on the card image itself.
      ogCardService          = new tools.OgCardService((_: String) => None),
      cityOgCardService      = new tools.CityOgCardService((_: String) => None),
      cinemaSourceUrls       = () => cinemaSourceUrls,
      stagingRepository      = stagingRepository,
      ratingCadenceReader    = ratingCadenceReader,
    )
    (ctrl, readModel)
  }
}
