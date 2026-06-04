package controllers

import clients.TmdbClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo, MovieService}
import tools.RealHttpFetch

/**
 * `/debug/tune` is the dev-only visual-tuning page. It must render (200) in
 * Dev/Test mode and 404 in Prod — the same `devOnly` gate as `/debug`. The
 * sample films are built in-process (`MovieController.tuneSampleFilms`), so the
 * page renders without any cache contents; the controller below is wired with
 * an empty repo on purpose to prove that.
 */
class MovieControllerTuneSpec extends AnyFlatSpec with Matchers {

  private def buildController(mode: Mode): MovieController = {
    val repo  = new InMemoryMovieRepo(Seq.empty)
    val cache = new CaffeineMovieCache(repo)
    val svc   = new MovieService(cache, new InProcessEventBus(), new TmdbClient(new RealHttpFetch, apiKey = None))
    new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = new MovieControllerService(svc),
      movieCache             = cache,
      userRepo               = new services.users.InMemoryUserRepo,
      oauthProviders         = Set.empty,
      environment            = mode
    )
  }

  "GET /debug/tune" should "render the tuning page in dev mode" in {
    val ctrl   = buildController(Mode.Dev)
    val result = ctrl.tune().apply(FakeRequest(GET, "/debug/tune"))

    status(result) shouldBe OK
    contentAsString(result) should include("tune-scope")
    // The real card partial rendered — the sample film's title is present.
    contentAsString(result) should include("Incepcja")
  }

  it should "404 in production" in {
    val ctrl   = buildController(Mode.Prod)
    val result = ctrl.tune().apply(FakeRequest(GET, "/debug/tune"))

    status(result) shouldBe NOT_FOUND
  }
}
