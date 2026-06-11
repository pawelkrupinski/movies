package controllers

import play.api.Mode
import play.api.test.Helpers
import services.movies.InMemoryMovieRepo
import services.readmodel.{TestReadModel, WebReadModel}

/** Shared builder for a fully-wired [[MovieController]] backed by an in-memory
 *  read model and no live HTTP — the seam every controller spec needs. Pass the
 *  records the read model should hold; get back the controller and the concrete
 *  [[WebReadModel]] so a test can `reload()` to bump the mtime. */
object TestMovieController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
  ): (MovieController, WebReadModel) = {
    val readModel = TestReadModel.fromRecords(records)
    val ctrl  = new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = new MovieControllerService(readModel),
      readModel              = readModel,
      // On-demand corpus dump only (dev /debug); holds the same rows.
      movieRepo              = new InMemoryMovieRepo(records),
      userRepo               = new services.users.InMemoryUserRepo,
      oauthProviders         = Set.empty,
      environment            = mode,
      responseCache          = new GzippedResponseCache,
    )
    (ctrl, readModel)
  }
}
