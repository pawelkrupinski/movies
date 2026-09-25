package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import tools.Env

/** `fb:app_id` is `FB_APP_ID` read through the deployment's [[Env]] on every
 *  render. It used to be a `lazy val` memo on `PageMeta`, frozen at first access,
 *  so an `/admin/config` override installed after the first page view never
 *  reached the page — and the template's `@fbAppId.foreach` rendered nothing at
 *  all (Twirl emits a block's VALUE, and `foreach` yields Unit), so the tag was
 *  never on any page even with the variable set. */
class FbAppIdSpec extends AnyFlatSpec with Matchers {

  private def controller(env: Env): MovieController = {
    val rec = MovieRecord(
      imdbId = Some("tt1"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title = Some("Testowy Film"), releaseYear = Some(2024),
          showtimes = Seq(models.Showtime(TestMovieController.now.plusHours(2), None, None, Nil)))))
    TestMovieController.build(Seq(("Testowy Film", Some(2024), rec)), fbAppId = () => env.get("FB_APP_ID"))._1
  }

  private def fbAppIdOf(html: String): Option[String] =
    """<meta property="fb:app_id"\s+content="([^"]+)">""".r.findFirstMatchIn(html).map(_.group(1))

  private def render(ctrl: MovieController): String =
    contentAsString(ctrl.index("poznan")(FakeRequest(GET, "/poznan/")))

  "the city index" should "emit no fb:app_id when FB_APP_ID is unset" in {
    fbAppIdOf(render(controller(Env.of()))) shouldBe None
  }

  it should "emit fb:app_id when FB_APP_ID is set" in {
    fbAppIdOf(render(controller(Env.of("FB_APP_ID" -> "111")))) shouldBe Some("111")
  }

  it should "pick up an admin override of FB_APP_ID installed after the first render" in {
    val env  = Env.of("FB_APP_ID" -> "111")
    val ctrl = controller(env)
    fbAppIdOf(render(ctrl)) shouldBe Some("111")
    env.installOverrides(Map("FB_APP_ID" -> "222").get)
    fbAppIdOf(render(ctrl)) shouldBe Some("222")
  }
}
