package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import tools.Env

/** The third-party page tags (`fb:app_id`, GA4, Sentry) come from the deployment's
 *  [[Env]], read on every render. They used to be process-global reads: `fb:app_id`
 *  a `lazy val` memo on `PageMeta`, frozen at first access so an `/admin/config`
 *  override never reached the page — and the template's `@fbAppId.foreach`
 *  rendered nothing at all (Twirl emits a block's VALUE, and `foreach` yields
 *  Unit), so the tag was never on any page even with the variable set; GA and
 *  Sentry were `tools.Env.get` calls inside the templates themselves. */
class PageTagsSpec extends AnyFlatSpec with Matchers {

  private def controller(env: Env): MovieController = {
    val rec = MovieRecord(
      imdbId = Some("tt1"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title = Some("Testowy Film"), releaseYear = Some(2024),
          showtimes = Seq(models.Showtime(TestMovieController.now.plusHours(2), None, None, Nil)))))
    TestMovieController.build(Seq(("Testowy Film", Some(2024), rec)), pageTags = () => PageTags.from(new settings.ProcessConfiguration(env)))._1
  }

  private def fbAppIdOf(html: String): Option[String] =
    """<meta property="fb:app_id"\s+content="([^"]+)">""".r.findFirstMatchIn(html).map(_.group(1))

  private def render(ctrl: MovieController): String =
    contentAsString(ctrl.index("poznan")(FakeRequest(GET, "/poznan/")))

  "the city index" should "emit no third-party tags when none is configured" in {
    val html = render(controller(Env.of()))
    fbAppIdOf(html) shouldBe None
    html should not include "googletagmanager.com"
    html should not include "js.sentry-cdn.com"
  }

  it should "emit each tag its Env configures" in {
    val html = render(controller(Env.of(
      "FB_APP_ID" -> "111", "GA_MEASUREMENT_ID" -> "G-TEST123",
      "SENTRY_LOADER_URL" -> "https://js.sentry-cdn.com/abc.min.js")))
    fbAppIdOf(html) shouldBe Some("111")
    html should include("googletagmanager.com/gtag/js?id=G-TEST123")
    html should include("https://js.sentry-cdn.com/abc.min.js")
  }

  it should "pick up an admin override of FB_APP_ID installed after the first render" in {
    val env  = Env.of("FB_APP_ID" -> "111")
    val ctrl = controller(env)
    fbAppIdOf(render(ctrl)) shouldBe Some("111")
    env.installOverrides(Map("FB_APP_ID" -> "222").get)
    fbAppIdOf(render(ctrl)) shouldBe Some("222")
  }
}
