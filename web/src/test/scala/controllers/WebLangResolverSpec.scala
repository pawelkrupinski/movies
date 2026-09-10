package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Lang
import play.api.mvc.Cookie
import play.api.test.FakeRequest

/**
 * `WebLangResolver.resolve` is the web's half of the language-fallback chain
 * every platform implements (explicit pick → device/browser language → region
 * → English) — see `LanguageSelection`/`StorefrontLanguage` on iOS and
 * `LanguageDefault`/`RegionLanguage` on Android for the other two.
 *
 * No running app needed: `resolve` is a pure function of a `RequestHeader`.
 */
class WebLangResolverSpec extends AnyFlatSpec with Matchers {

  private val PolishDefault = Lang("pl")

  "resolve" should "fall back to the deployment default when the request names nothing" in {
    WebLangResolver.resolve(FakeRequest("GET", "/poznan/"), PolishDefault) shouldBe PolishDefault
  }

  it should "prefer the PLAY_LANG cookie over everything else" in {
    val request = FakeRequest("GET", "/poznan/")
      .withCookies(Cookie("PLAY_LANG", "de"))
      .withHeaders("Accept-Language" -> "es")
    WebLangResolver.resolve(request, PolishDefault) shouldBe Lang("de")
  }

  it should "fall back to Accept-Language when there is no cookie" in {
    val request = FakeRequest("GET", "/poznan/").withHeaders("Accept-Language" -> "es-ES,es;q=0.9")
    WebLangResolver.resolve(request, PolishDefault) shouldBe Lang("es")
  }

  it should "ignore a cookie naming an unsupported language" in {
    val request = FakeRequest("GET", "/poznan/")
      .withCookies(Cookie("PLAY_LANG", "fr"))
      .withHeaders("Accept-Language" -> "de")
    WebLangResolver.resolve(request, PolishDefault) shouldBe Lang("de")
  }

  it should "ignore an Accept-Language header naming only unsupported languages" in {
    val request = FakeRequest("GET", "/poznan/").withHeaders("Accept-Language" -> "fr-FR,fr;q=0.9,it;q=0.8")
    WebLangResolver.resolve(request, PolishDefault) shouldBe PolishDefault
  }

  it should "take the first SUPPORTED language in a multi-value Accept-Language, not just the first entry" in {
    val request = FakeRequest("GET", "/poznan/").withHeaders("Accept-Language" -> "fr-FR,fr;q=0.9,de;q=0.8")
    WebLangResolver.resolve(request, PolishDefault) shouldBe Lang("de")
  }

  "Supported" should "be exactly the four bundles every deployment ships" in {
    WebLangResolver.Supported shouldBe Set("pl", "en", "de", "es")
  }
}
