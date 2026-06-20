package controllers

import models.{Movie, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.readmodel.TestReadModel

import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

// The dynamic per-city OG card endpoint (`GET /:city/og-image`). Wired through
// the real controller with an empty read model + a no-network poster fetch, so
// the card degrades to the gradient-only render — enough to assert the route,
// content type and 1200×630 canvas without touching the network.
class CityOgImageControllerSpec extends AnyFlatSpec with Matchers {

  private val (controller, _) = TestMovieController.build(records = Nil)

  "GET /:city/og-image" should "serve a 1200×630 image/png for a known city" in {
    val result = controller.cityOgImage("poznan").apply(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("image/png")
    header("Cache-Control", result) shouldBe Some("public, max-age=3600")
    val img = ImageIO.read(new ByteArrayInputStream(contentAsBytes(result).toArray))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "404 an unknown city slug" in {
    status(controller.cityOgImage("atlantyda").apply(FakeRequest())) shouldBe NOT_FOUND
  }

  private def sched(title: String, poster: Option[String] = None): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = poster, synopsis = None, cast = Nil, director = Nil,
      cinemaFilmUrls = Nil, showings = Nil, resolved = TestReadModel.resolved(title, None, MovieRecord()))

  "MovieController.distinctByMovie" should "collapse a film's programme variant so its poster isn't repeated" in {
    // A base showing + its accessibility variant share an upstream search key
    // ("Kino bez barier: Freak Show" → "Freak Show"), so they're one film/poster.
    val out = MovieController.distinctByMovie(Seq(
      sched("Freak Show"), sched("Kino bez barier: Freak Show"), sched("Toy Story 5"))
    ).map(_.movie.title)
    out shouldBe Seq("Freak Show", "Toy Story 5")
  }

  it should "drop unrelated films that share one poster image (generic placeholder)" in {
    // A retrospective where several distinct titles all use one placeholder
    // poster — keep only the first so the image isn't repeated.
    val out = MovieController.distinctByMovie(Seq(
      sched("Ziemia obiecana", Some("https://cdn/generic.jpg")),
      sched("Brzezina",        Some("https://cdn/generic.jpg")),
      sched("Toy Story 5",     Some("https://cdn/toy.jpg")))
    ).map(_.movie.title)
    out shouldBe Seq("Ziemia obiecana", "Toy Story 5")
  }

  "MovieController.dailyCardFilms" should "rotate to a different, non-overlapping set each day (stable within a day)" in {
    val pool = (1 to 12).map(i => sched(s"Film $i", Some(s"https://cdn/$i.jpg")))
    val d1 = MovieController.dailyCardFilms(pool, epochDay = 100, count = 5).map(_.movie.title)
    val d2 = MovieController.dailyCardFilms(pool, epochDay = 101, count = 5).map(_.movie.title)
    d1 should have size 5
    d2 should have size 5
    d1 should not equal d2              // a different day shows a different set
    d1.intersect(d2) shouldBe empty     // count-per-day step → adjacent days disjoint
    MovieController.dailyCardFilms(pool, 100, 5).map(_.movie.title) shouldBe d1 // deterministic
  }

  it should "drop films without a poster (no grey slots) and survive an empty pool" in {
    MovieController.dailyCardFilms(Seq(sched("No poster", None)), epochDay = 1, count = 5) shouldBe empty
  }

  it should "cycle through the FULL repertoire, not just the first 40" in {
    val pool = (1 to 50).map(i => sched(s"Film $i", Some(s"https://cdn/$i.jpg")))
    // epochDay 9, count 5 → window starts at index 45, so films 46–50 show —
    // unreachable when the pool was capped at the first 40.
    MovieController.dailyCardFilms(pool, epochDay = 9, count = 5).map(_.movie.title) should contain ("Film 50")
  }
}
