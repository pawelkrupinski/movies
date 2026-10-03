package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** The film and browse pages are written fragment by fragment (`ResponseBody.html`)
 *  rather than through Play's `Writeable[Html]`; they must still answer as UTF-8 HTML,
 *  with non-ASCII text encoded as such. */
class HtmlPageBodySpec extends AnyFlatSpec with Matchers {

  private val title = "Milcząca przyjaciółka"
  private val (ctrl, _) = TestMovieController.build(Seq(TestMovieController.showing(title, year = Some(2026))))

  "a film page" should "answer as UTF-8 HTML" in {
    val result = ctrl.filmBySlug("poznan", "milczaca-przyjaciolka")(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/html"); charset(result) shouldBe Some("utf-8")
    contentAsString(result) should include (s"<title>$title")
  }

  "a browse page" should "answer as UTF-8 HTML" in {
    val result = ctrl.browse("poznan", None, None, None, Some("Dramat"))(FakeRequest("GET", "/poznan/movies?genre=Dramat"))
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/html"); charset(result) shouldBe Some("utf-8")
    contentAsString(result) should startWith ("<!DOCTYPE html>")
  }
}
