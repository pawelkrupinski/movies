package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** Links minted while venues printed their own genre spellings keep working: they 301 onto the vocabulary's. */
class GenreLinkRedirectSpec extends AnyFlatSpec with Matchers {
  private val controller = TestMovieController.build(Seq(TestMovieController.showing("Bing", Some(2025))))._1

  "GET /{city}/movies?genre=przygoda" should "301 onto ?genre=Przygodowy" in {
    val result = controller.browse("poznan", None, None, None, Some("przygoda")).apply(FakeRequest(GET, "/poznan/movies?genre=przygoda"))
    status(result) shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some("/poznan/movies?genre=Przygodowy")
  }

  "GET /{city}/?genre=…" should "301 each lowercase genre, keeping the rest of the query" in {
    val result = controller.index("poznan").apply(FakeRequest(GET, "/poznan/?cinema=X&genre=dla+dzieci&genre=Akcja"))
    status(result) shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some("/poznan/?cinema=X&genre=Familijny&genre=Akcja")
  }

  it should "serve a canonical genre link as it is" in {
    status(controller.index("poznan").apply(FakeRequest(GET, "/poznan/?genre=Akcja"))) shouldBe OK
  }
}
