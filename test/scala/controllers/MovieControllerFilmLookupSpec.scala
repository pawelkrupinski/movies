package controllers

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
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
 * Regression: the home page links to `/film?title=<displayed title>` where
 * the displayed title still carries the cinema-reported Arabic numeral
 * ("Diabeł ubiera się u Prady 2"). The controller's `normalizeTitle` folds
 * "2" → "II" on the URL side, so the lookup must also normalise the
 * candidate schedule's title before comparing — otherwise every film with
 * a single-digit Arabic numeral 404s from its own home-page link.
 */
class MovieControllerFilmLookupSpec extends AnyFlatSpec with Matchers {

  private def buildController(title: String, year: Option[Int]): MovieController = {
    val now = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt12340108"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title          = Some(title),
          releaseYear    = year,
          showtimes      = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        ),
        Tmdb -> SourceData(originalTitle = Some("The Devil Wears Prada 2"))
      )
    )
    val repo  = new InMemoryMovieRepo(Seq((title, year, record)))
    val cache = new CaffeineMovieCache(repo)
    val svc   = new MovieService(cache, new InProcessEventBus(), new TmdbClient(new RealHttpFetch, apiKey = None))
    val movieControllerService   = new MovieControllerService(svc)
    new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = movieControllerService,
      movieCache             = cache,
      userRepo               = new services.users.InMemoryUserRepo,
      oauthProviders         = Set.empty,
      environment            = Mode.Test
    )
  }

  "GET /film?title=…" should "resolve a displayed title that contains a single-digit Arabic numeral" in {
    // Cinema-reported title: "Diabeł ubiera się u Prady 2".
    // Displayed on the home page as-is; the link encodes the same string.
    // Controller folds 2 → II via TitleNormalizer.normalize — the schedule
    // side must apply the same fold before comparing.
    val title = "Diabeł ubiera się u Prady 2"
    val ctrl  = buildController(title, Some(2025))

    val result = ctrl.film("poznan", title).apply(FakeRequest(GET, s"/poznan/film?title=$title"))

    status(result) shouldBe OK
  }

  it should "still 404 a title that doesn't match any schedule" in {
    val ctrl = buildController("Something Else", Some(2025))
    val result = ctrl.film("poznan", "No Such Film").apply(FakeRequest(GET, "/poznan/film?title=No+Such+Film"))
    status(result) shouldBe NOT_FOUND
  }
}
