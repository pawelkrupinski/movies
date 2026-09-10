package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._

/** `LanguageController` — the `/lang/:code` write path. */
class LanguageControllerSpec extends AnyFlatSpec with Matchers {

  private val controller = new LanguageController(
    Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi))

  "set" should "write the PLAY_LANG cookie for a supported code and redirect back" in {
    val res = controller.set("de", "/poznan/?cinema=Helios").apply(FakeRequest("GET", "/lang/de"))
    status(res) shouldBe SEE_OTHER
    redirectLocation(res) shouldBe Some("/poznan/?cinema=Helios")
    cookies(res).get("PLAY_LANG").map(_.value) shouldBe Some("de")
  }

  it should "redirect without setting a cookie for an unsupported code" in {
    val res = controller.set("fr", "/poznan/").apply(FakeRequest("GET", "/lang/fr"))
    status(res) shouldBe SEE_OTHER
    redirectLocation(res) shouldBe Some("/poznan/")
    cookies(res).get("PLAY_LANG") shouldBe None
  }

  // `play.i18n.langCookieMaxAge = 31536000` in application.conf is a bare
  // number, which HOCON parses as MILLISECONDS for a duration setting absent a
  // unit suffix — so it was read as 31536000ms = 31536s (~8.8 hours), not the
  // 31536000 SECONDS (365 days) the value and the config's own comment intend.
  // A pick that survives "a browser restart rather than just the session" is
  // the whole point of setting this at all; at 8.8 hours it barely outlives a
  // lunch break, and a visitor who picked German yesterday finds themselves
  // back on the deployment default today with no further action of their own.
  it should "set the PLAY_LANG cookie to survive a year, not ~8.8 hours" in {
    val res = controller.set("de", "/poznan/").apply(FakeRequest("GET", "/lang/de"))
    cookies(res).get("PLAY_LANG").flatMap(_.maxAge) shouldBe Some(31536000)
  }
}

/** `LanguageController.safeBack` — pure, so asserted without a request. */
class LanguageControllerSafeBackSpec extends AnyFlatSpec with Matchers {

  "safeBack" should "keep a same-origin relative path" in {
    LanguageController.safeBack("/uk/kent/?cinema=Odeon") shouldBe "/uk/kent/?cinema=Odeon"
  }

  it should "fall back to the root for an empty string" in {
    LanguageController.safeBack("") shouldBe "/"
  }

  it should "fall back to the root for a protocol-relative spelling" in {
    LanguageController.safeBack("//evil.example.com/phish") shouldBe "/"
  }

  it should "fall back to the root for an absolute URL" in {
    LanguageController.safeBack("https://evil.example.com/phish") shouldBe "/"
  }
}
