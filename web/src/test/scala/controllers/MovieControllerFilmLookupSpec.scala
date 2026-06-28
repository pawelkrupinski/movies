package controllers

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

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
          posterUrl      = Some("https://cinema.example/poster.jpg"),
          showtimes      = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        ),
        Tmdb -> SourceData(originalTitle = Some("The Devil Wears Prada 2"))
      )
    )
    TestMovieController.build(Seq((title, year, record)))._1
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

  "the film page <title>" should "lead with the film, its year and a seans keyword" in {
    val title  = "Diuna"
    val ctrl   = buildController(title, Some(2024))
    val result = ctrl.film("poznan", title).apply(FakeRequest(GET, s"/poznan/film?title=$title"))
    val html   = contentAsString(result)
    val pageTitle = "<title>(.*?)</title>".r.findFirstMatchIn(html).map(_.group(1)).getOrElse("")
    pageTitle should include("Diuna (2024)")
    pageTitle should include("godziny seansów")
    pageTitle should include("Poznań")
    pageTitle should endWith("| Kinowo")
  }

  "the film page" should "preconnect to the poster CDN and prioritise the LCP poster" in {
    val title  = "Diuna"
    val ctrl   = buildController(title, Some(2024))
    val html   = contentAsString(ctrl.film("poznan", title).apply(FakeRequest(GET, s"/poznan/film?title=$title")))
    html should include("""<link rel="preconnect" href="https://images.weserv.nl" crossorigin>""")
    // The detail poster is the LCP element — eager + high priority + async decode.
    html should include("""fetchpriority="high"""")
    html should include("""decoding="async"""")
  }
}
