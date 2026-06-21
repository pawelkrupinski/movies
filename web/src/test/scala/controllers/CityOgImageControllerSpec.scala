package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

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
}
