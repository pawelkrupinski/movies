package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._


/** The city listing's `<h1>`. The page had none at all — the city was marked up
 *  only as a `<title>` and a meta tag — and for a listing that spans several
 *  towns this heading is the one place their names are page text a search can
 *  match. Rendered through the real controller, because the thing under test is
 *  what the SERVED html contains.
 */
class CityHeadingSpec extends AnyFlatSpec with Matchers {

  private def controller(): MovieController =
    TestMovieController.build(Seq(TestMovieController.showing("Testowy Film", Some(2024), imdbId = Some("tt1"))))._1

  private def h1Of(html: String): Option[String] =
    """<h1[^>]*>([^<]*)</h1>""".r.findFirstMatchIn(html).map(_.group(1))

  "the city index" should "carry exactly one h1, naming the city" in {
    val html = contentAsString(controller().index("poznan")(EdgeRequest("/poznan/")))
    // Exactly one: the day carousel clones `#view-root` as the user swipes, so a
    // heading rendered inside it would come back as a second and third h1.
    "<h1".r.findAllIn(html).size shouldBe 1
    h1Of(html).getOrElse("") should startWith("Repertuar kin w Poznaniu")
  }

  "a multi-town city's index" should "name the covered towns in the heading" in {
    val html = contentAsString(controller().index("trojmiasto")(EdgeRequest("/trojmiasto/")))
    h1Of(html) shouldBe Some("Repertuar kin w Trójmieście – Gdańsk, Gdynia, Sopot")
    // The point of the exercise: "Sopot" is now text on the page, where before
    // it appeared in no heading, no meta tag and no cinema name.
    html should include("Sopot")
  }
}
