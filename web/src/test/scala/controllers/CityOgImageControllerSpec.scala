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
    header("Cache-Control", result) shouldBe Some("public, max-age=86400")
    val img = ImageIO.read(new ByteArrayInputStream(contentAsBytes(result).toArray))
    img.getWidth  shouldBe 1200
    img.getHeight shouldBe 630
  }

  it should "404 an unknown city slug" in {
    status(controller.cityOgImage("atlantyda").apply(FakeRequest())) shouldBe NOT_FOUND
  }

  private def sched(title: String): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = None, synopsis = None, cast = Nil, director = Nil,
      cinemaFilmUrls = Nil, showings = Nil, resolved = TestReadModel.resolved(title, None, MovieRecord()))

  "MovieController.distinctByMovie" should "collapse a film's programme variant so its poster isn't repeated" in {
    // A base showing + its accessibility variant share an upstream search key
    // ("Kino bez barier: Freak Show" → "Freak Show"), so they're one film/poster.
    val out = MovieController.distinctByMovie(Seq(
      sched("Freak Show"), sched("Kino bez barier: Freak Show"), sched("Toy Story 5"))
    ).map(_.movie.title)
    out shouldBe Seq("Freak Show", "Toy Story 5")
  }
}
