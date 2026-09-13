package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Cookie
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * The filters UI's language picker: an explicit pick (the `PLAY_LANG` cookie
 * `LanguageController.set` writes) changes the rendered copy on its own,
 * independently of the deployment's country — and, because that makes the
 * response able to vary per visitor, `MovieController.renderIndex` must stop
 * offering a non-default pick to the shared edge cache.
 *
 * This is the regression coverage for the bug `SharedCacheableListingSpec`
 * caught during development: `deploymentDefaultLang` compared region-qualified
 * against the bundle-negotiated `Lang`, so the equality in
 * `cacheablePlainPage` never held and EVERY plain listing — not just a
 * non-default pick — silently dropped out of the shared cache.
 */
class LanguagePickSpec extends AnyFlatSpec with Matchers {

  private val Now = LocalDateTime.now()

  private def controller() = TestMovieController.build(
    Seq(("Test Film", Some(2024), MovieRecord(
      imdbId = Some("tt999"),
      data = Map[Source, SourceData](Helios -> SourceData(
        title = Some("Test Film"), releaseYear = Some(2024),
        showtimes = Seq(models.Showtime(Now.plusHours(2), None, None, Nil)))))))
  )._1

  "a PLAY_LANG cookie" should "render the picked language's copy, on a Polish deployment" in {
    val html = contentAsString(controller().index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "de"))))
    html should include("<html lang=\"de\"")
  }

  it should "render English when the pick is \"en\", on a Polish deployment" in {
    val html = contentAsString(controller().index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "en"))))
    html should include("<html lang=\"en\"")
  }

  it should "not disturb the deployment default when absent" in {
    val html = contentAsString(controller().index("poznan")(FakeRequest("GET", "/poznan/")))
    html should include("<html lang=\"pl\"")
  }

  "the deployment-default plain listing" should "still be offered to the shared edge cache" in {
    val header_ = header("Cache-Control", controller().index("poznan")(FakeRequest("GET", "/poznan/"))).value
    header_ should include("public")
    header_ should include("must-revalidate")
  }

  "a non-default language pick" should "fall out of the shared cache, like a `?filter=` variant" in {
    val header_ = header("Cache-Control", controller().index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "de")))).value
    header_ shouldBe "private, no-cache, no-transform"
  }

  it should "not share an ETag with the deployment-default rendering, even for the same city/query" in {
    val ctrl = controller()
    val defaultEtag = header("ETag", ctrl.index("poznan")(FakeRequest("GET", "/poznan/"))).value
    val germanEtag  = header("ETag", ctrl.index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "de")))).value
    germanEtag should not be defaultEtag
  }
}
