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
 * The plain city listing renders the SAME bytes for every visitor, no matter
 * what language they carry — `MovieController` always renders the
 * deployment's own default `Messages` now; an explicit language pick swaps
 * the visible copy client-side (`shared.js`'s `applyLanguage`), never by
 * re-rendering server-side.
 *
 * This is the regression coverage for a real production bug: the OLD design
 * rendered a DIFFERENT `Cache-Control`/ETag per visitor (`public, max-age=0,
 * must-revalidate` for the deployment default, `private, no-cache` with its
 * own lang-keyed ETag otherwise) at the SAME URL — and Cloudflare's edge
 * cache rule for that URL matches on path alone, with no awareness of the
 * cookie deciding which branch actually rendered. So the first request after
 * picking a language would routinely get served/revalidated against the
 * WRONG cached entry (a stale render, or another visitor's), self-correcting
 * on the very next identical request — reproduced 100% of the time via
 * direct curl against production and root-caused to exactly this per-visitor
 * cache-key coupling.
 *
 * The fix removes the coupling structurally rather than patching around it:
 * there is no longer any request input (cookie, Accept-Language, or
 * otherwise) that changes what this handler renders, so there is nothing
 * left for an edge cache to get wrong.
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

  "the plain city listing" should "always render the deployment's default language, ignoring any PLAY_LANG cookie" in {
    val ctrl    = controller()
    val plain   = contentAsString(ctrl.index("poznan")(FakeRequest("GET", "/poznan/")))
    val cookied = contentAsString(ctrl.index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "de"))))
    plain should include("<html lang=\"pl\"")
    cookied should include("<html lang=\"pl\"")
  }

  it should "ignore Accept-Language too" in {
    val html = contentAsString(controller().index("poznan")(
      FakeRequest("GET", "/poznan/").withHeaders("Accept-Language" -> "en")))
    html should include("<html lang=\"pl\"")
  }

  it should "always be offered to the shared edge cache, no matter what language the visitor carries" in {
    val ctrl           = controller()
    val plainHeader   = header("Cache-Control", ctrl.index("poznan")(FakeRequest("GET", "/poznan/"))).value
    val cookiedHeader = header("Cache-Control", ctrl.index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "de")))).value
    cookiedHeader shouldBe plainHeader
    plainHeader should include("public")
    plainHeader should include("must-revalidate")
  }

  it should "carry the SAME ETag regardless of the visitor's PLAY_LANG cookie" in {
    val ctrl         = controller()
    val plainEtag   = header("ETag", ctrl.index("poznan")(FakeRequest("GET", "/poznan/"))).value
    val cookiedEtag = header("ETag", ctrl.index("poznan")(
      FakeRequest("GET", "/poznan/").withCookies(Cookie("PLAY_LANG", "de")))).value
    cookiedEtag shouldBe plainEtag
  }
}
