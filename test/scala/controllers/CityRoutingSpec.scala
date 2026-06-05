package controllers

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo, MovieService}
import tools.RealHttpFetch

import java.time.LocalDateTime

/**
 * Every page/API is now city-scoped under `/{city}/`. This pins the resolution
 * contract: a known slug renders, an unknown slug 404s, and the rendered page
 * carries the city's label + city-prefixed links.
 */
class CityRoutingSpec extends AnyFlatSpec with Matchers {

  private def buildController(): MovieController = {
    val now    = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt1"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Testowy Film"),
          releaseYear = Some(2024),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        )
      )
    )
    val repo  = new InMemoryMovieRepo(Seq(("Testowy Film", Some(2024), record)))
    val cache = new CaffeineMovieCache(repo)
    val svc   = new MovieService(cache, new InProcessEventBus(), new TmdbClient(new RealHttpFetch, apiKey = None))
    new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = new MovieControllerService(svc),
      movieCache             = cache,
      userRepo               = new services.users.InMemoryUserRepo,
      oauthProviders         = Set.empty,
      environment            = Mode.Test
    )
  }

  "An unknown city slug" should "404 on every city-scoped route" in {
    val ctrl = buildController()
    status(ctrl.index("nieznane")(FakeRequest(GET, "/nieznane/")))               shouldBe NOT_FOUND
    status(ctrl.kina("nieznane")(FakeRequest(GET, "/nieznane/kina")))            shouldBe NOT_FOUND
    status(ctrl.film("nieznane", "x")(FakeRequest(GET, "/nieznane/film?title=x"))) shouldBe NOT_FOUND
    status(ctrl.apiRepertoire("nieznane")(FakeRequest(GET, "/nieznane/api/repertoire"))) shouldBe NOT_FOUND
    status(ctrl.apiDetails("nieznane")(FakeRequest(GET, "/nieznane/api/details"))) shouldBe NOT_FOUND
  }

  "The Poznań index" should "render with the city label and city-prefixed links" in {
    val ctrl = buildController()
    val res  = ctrl.index("poznan")(FakeRequest(GET, "/poznan/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include("Repertuar kinowy Poznań")
    // Navbar links carry the city prefix.
    html should include("""href="/poznan/kina"""")
    // The lone fixture film is in a Poznań cinema → present.
    html should include("Testowy Film")
  }

  "apiRepertoire for a known city" should "200 and list the city's films" in {
    val ctrl = buildController()
    val res  = ctrl.apiRepertoire("poznan")(FakeRequest(GET, "/poznan/api/repertoire"))
    status(res) shouldBe OK
    contentAsString(res) should include("Testowy Film")
  }

  // The read path is city-scoped: the fixture film plays only in Helios (a
  // Poznań venue), so it must NOT surface under another city even though the
  // global cache holds it. This is the core of the "city = cinema subset" model.
  "A film playing only in a Poznań cinema" should "be absent from another city's repertoire" in {
    val ctrl = buildController()
    val res  = ctrl.apiRepertoire("warszawa")(FakeRequest(GET, "/warszawa/api/repertoire"))
    status(res) shouldBe OK
    contentAsString(res) shouldBe "[]"
  }
}
