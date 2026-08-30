package controllers

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * The film page is addressed by slug (`/{city}/movie/{slug}`); the older
 * `?title=` form stays routable and 301s onto it.
 *
 * The legacy path still has to resolve the *displayed* title, which carries the
 * cinema-reported Arabic numeral ("Diabeł ubiera się u Prady 2"). The
 * controller's `normalizeTitle` folds "2" → "II", so the lookup must apply the
 * same fold to the candidate schedule before comparing — otherwise every film
 * with a single-digit Arabic numeral 404s from its own old link.
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

  "GET /movie/{slug}" should "resolve a displayed title that contains a single-digit Arabic numeral" in {
    val title = "Diabeł ubiera się u Prady 2"
    val ctrl  = buildController(title, Some(2025))

    val result = ctrl.filmBySlug("poznan", "diabel-ubiera-sie-u-prady-2")
      .apply(FakeRequest(GET, "/poznan/movie/diabel-ubiera-sie-u-prady-2"))

    status(result) shouldBe OK
  }

  it should "404 a slug that matches no schedule" in {
    val ctrl = buildController("Something Else", Some(2025))
    val result = ctrl.filmBySlug("poznan", "no-such-film")
      .apply(FakeRequest(GET, "/poznan/movie/no-such-film"))
    status(result) shouldBe NOT_FOUND
  }

  "the legacy GET /movie?title=…" should "301 onto the slug address" in {
    // Old sitemap entries, shared links, and installed app builds all carry this
    // form; it must keep resolving, and must consolidate onto one address.
    val title = "Diabeł ubiera się u Prady 2"
    val ctrl  = buildController(title, Some(2025))

    val result = ctrl.film("poznan", title).apply(FakeRequest(GET, s"/poznan/movie?title=$title"))

    status(result) shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some("/poznan/movie/diabel-ubiera-sie-u-prady-2")
  }

  it should "still 404 a title that doesn't match any schedule" in {
    val ctrl = buildController("Something Else", Some(2025))
    val result = ctrl.film("poznan", "No Such Film").apply(FakeRequest(GET, "/poznan/movie?title=No+Such+Film"))
    status(result) shouldBe NOT_FOUND
  }

  it should "render in place rather than redirect to itself when the title has no slug" in {
    // "!!!" folds to the empty slug, so there is no slug address to 301 to —
    // redirecting would loop the request onto the same URL forever.
    val title = "!!!"
    val ctrl  = buildController(title, Some(2025))

    val result = ctrl.film("poznan", title).apply(FakeRequest(GET, s"/poznan/movie?title=$title"))

    status(result) shouldBe OK
  }

  "the film page <title>" should "lead with the film, its year and a seans keyword" in {
    val ctrl   = buildController("Diuna", Some(2024))
    val result = ctrl.filmBySlug("poznan", "diuna").apply(FakeRequest(GET, "/poznan/movie/diuna"))
    val html   = contentAsString(result)
    val pageTitle = "<title>(.*?)</title>".r.findFirstMatchIn(html).map(_.group(1)).getOrElse("")
    pageTitle should include("Diuna (2024)")
    pageTitle should include("godziny seansów")
    pageTitle should include("Poznań")
    pageTitle should endWith("| Kinowo")
  }

  "the film page" should "preconnect to the poster CDN and prioritise the LCP poster" in {
    val ctrl = buildController("Diuna", Some(2024))
    val html = contentAsString(ctrl.filmBySlug("poznan", "diuna").apply(FakeRequest(GET, "/poznan/movie/diuna")))
    html should include("""<link rel="preconnect" href="https://images.weserv.nl" crossorigin>""")
    // The detail poster is the LCP element — eager + high priority + async decode.
    html should include("""fetchpriority="high"""")
    html should include("""decoding="async"""")
  }
}
