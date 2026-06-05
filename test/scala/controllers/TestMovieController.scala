package controllers

import clients.TmdbClient
import play.api.Mode
import play.api.test.Helpers
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo, MovieService}
import tools.RealHttpFetch

/** Shared builder for a fully-wired [[MovieController]] backed by an in-memory
 *  repo/cache and no live HTTP — the seam every controller spec needs. Pass the
 *  records the cache should hold; get back the controller and the concrete
 *  cache so a test can `rehydrate()` to bump the mtime. */
object TestMovieController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
  ): (MovieController, CaffeineMovieCache) = {
    val repo  = new InMemoryMovieRepo(records)
    val cache = new CaffeineMovieCache(repo)
    val svc   = new MovieService(cache, new InProcessEventBus(), new TmdbClient(new RealHttpFetch, apiKey = None))
    val ctrl  = new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = new MovieControllerService(svc),
      movieCache             = cache,
      userRepo               = new services.users.InMemoryUserRepo,
      oauthProviders         = Set.empty,
      environment            = mode,
      pageCache              = new PageResponseCache,
    )
    (ctrl, cache)
  }
}
