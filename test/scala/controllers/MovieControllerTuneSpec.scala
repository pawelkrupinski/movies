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
    val result = ctrl.tune("poznan").apply(FakeRequest(GET, "/debug/tune"))

    status(result) shouldBe OK
    contentAsString(result) should include("tune-scope")
    // The real card partial rendered — the sample film's title is present.
    contentAsString(result) should include("Incepcja")
  }

  it should "render the pill / rating edge cases the page exists to tune" in {
    val ctrl = buildController(Mode.Dev)
    val html = contentAsString(ctrl.tune("poznan").apply(FakeRequest(GET, "/debug/tune")))

    // RT below 60 → the `.rotten` red variant (Morbius, RT 15).
    html should include("rating-rt rotten")
    // A wide format token from the many-showtimes wrapping card.
    html should include("4DX")
    // The lone Metacritic bare-number pill (Aftersun) — and a card with no
    // ratings row at all (the preview / untitled film, enrichment = None).
    html should include("Aftersun")
    html should include("Niezatytułowany")
    // Programme-prefixed title kept verbatim (separate-row case).
    html should include("Kino Seniora")
  }

  "tuneSampleFilms" should "cover the pill/rating edge cases" in {
    val films = MovieController.tuneSampleFilms

    // A card with no enrichment (no ratings row).
    films.exists(_.enrichment.isEmpty) shouldBe true
    // A rotten RT (below 60) and the widest values (10.0 / 100 / 100%).
    films.flatMap(_.enrichment).flatMap(_.rottenTomatoes).min should be < 60
    films.flatMap(_.enrichment).flatMap(_.imdbRating).max shouldBe 10.0
    // A card whose single cinema has many showtimes (pills wrap several rows).
    val maxSlotsInOneCinema = films
      .flatMap(_.showings.flatMap(_._2))
      .map(_.showtimes.size)
      .max
    maxSlotsInOneCinema should be >= 8
    // At least one no-booking showtime → the `<span>` badge variant.
    films.flatMap(_.showings.flatMap(_._2)).flatMap(_.showtimes).exists(_.bookingUrl.isEmpty) shouldBe true
  }

  it should "404 in production" in {
    val ctrl   = buildController(Mode.Prod)
    val result = ctrl.tune("poznan").apply(FakeRequest(GET, "/debug/tune"))

    status(result) shouldBe NOT_FOUND
  }

  "GET /debug/tune/kina" should "render the kina tuning page in dev mode" in {
    val ctrl   = buildController(Mode.Dev)
    val result = ctrl.tuneKina("poznan").apply(FakeRequest(GET, "/debug/tune/kina"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("tune-scope")
    // The real `_cinemaCards` markup (cinema section header) is present.
    html should include("cinema-section-title")
    // A kina-specific slider var, and the sub-nav linking the sibling pages.
    html should include("--kina-title-font")
    html should include("/debug/tune/film")
  }

  it should "404 in production" in {
    val result = buildController(Mode.Prod).tuneKina("poznan").apply(FakeRequest(GET, "/debug/tune/kina"))
    status(result) shouldBe NOT_FOUND
  }

  "GET /debug/tune/film" should "render the film tuning page in dev mode" in {
    val ctrl   = buildController(Mode.Dev)
    val result = ctrl.tuneFilm("poznan").apply(FakeRequest(GET, "/debug/tune/film"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("tune-scope")
    // The real `_filmDetailContent` markup + a populated meta block.
    html should include("film-title")
    html should include("Christopher Nolan")
    // A film-specific slider var, and the sub-nav linking the sibling pages.
    html should include("--film-title-font")
    html should include("/debug/tune/kina")
  }

  it should "404 in production" in {
    val result = buildController(Mode.Prod).tuneFilm("poznan").apply(FakeRequest(GET, "/debug/tune/film"))
    status(result) shouldBe NOT_FOUND
  }

  "tuneSampleCinemas" should "pivot the sample films into cinema sections" in {
    val cinemas = MovieController.tuneSampleCinemas
    cinemas should not be empty
    all(cinemas.map(_.movies.size)) should be >= 1
    // The pivot must preserve showtimes (no empty sections).
    cinemas.flatMap(_.movies).flatMap(_.showings).flatMap(_._2) should not be empty
  }

  "tuneSampleFilm" should "populate synopsis + cast + director for the meta blocks" in {
    val f = MovieController.tuneSampleFilm
    f.synopsis should not be empty
    f.cast should not be empty
    f.director should not be empty
  }
}
