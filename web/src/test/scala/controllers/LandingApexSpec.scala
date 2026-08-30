package controllers

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}

/**
 * `/` serves two different screens depending on the HOST it was asked on, and
 * this is the only place in the app where the host decides anything — every
 * other page takes its country from `KINOWO_COUNTRY` once at boot.
 *
 * The bare `showtimes.cc` apex is the brand front door: a country picker rather
 * than a city picker, because there is no one country it could mean. Every
 * deployment renders it identically, which is what lets the proxy point the apex
 * at whichever country's pods it likes without a fourth deployment existing.
 */
class LandingApexSpec extends AnyFlatSpec with Matchers {

  private val controller = new LandingController(
    Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi))

  private def bodyOn(host: String): String =
    contentAsString(controller.index().apply(
      FakeRequest("GET", "/").withHeaders("X-Forwarded-Host" -> host)))

  "the showtimes.cc apex" should "offer every deployed country, each on its own domain" in {
    val html = bodyOn("showtimes.cc")
    html should include ("""<ul class="country-list"""")
    models.Country.switchable.foreach { c =>
      html should include (s"""href="${c.webUrl.get}/"""")
      html should include (c.displayName)
    }
  }

  // Poland answers on kinowo.net under a different brand, so it is the one entry
  // that could plausibly be left off the Showtimes front door. It must not be:
  // someone who lands here wanting Polish listings should not have to know the
  // two brands are one product.
  it should "include Poland, despite its separate domain and brand" in {
    val html = bodyOn("showtimes.cc")
    html should include ("Polska")
    html should include ("""href="https://kinowo.net/"""")
    html should include ("kinowo.net")
  }

  it should "pick a country, not a city — no city links and no geolocation" in {
    val html = bodyOn("showtimes.cc")
    html should not include ("""class="city-list"""")
    html should not include ("navigator.geolocation")
  }

  it should "answer the www. spelling too, in case the proxy redirect is bypassed" in {
    bodyOn("www.showtimes.cc") should include ("""<ul class="country-list"""")
  }

  it should "ignore a port suffix on the host" in {
    bodyOn("showtimes.cc:9000") should include ("""<ul class="country-list"""")
  }

  "a country's own host" should "still get the city picker, not the country picker" in {
    val html = bodyOn("kinowo.net")
    html should include ("""<ul class="city-list"""")
    html should not include ("""class="country-list"""")
  }

  // Since the Showtimes countries moved under `showtimes.cc/{code}/`, the apex
  // is also the host their OWN pages arrive on — so the host alone can no longer
  // decide. A deployment mounted under a country segment must never answer the
  // front door, or `showtimes.cc/uk/` replaces the UK homepage with a picker.
  it should "not be answered by a deployment mounted under a country segment" in {
    val uk = new LandingController(Helpers.stubControllerComponents(), models.Country.UnitedKingdom)
    val html = contentAsString(uk.index().apply(
      FakeRequest("GET", "/").withHeaders("X-Forwarded-Host" -> "showtimes.cc")))
    html should include ("""<ul class="city-list"""")
    html should not include ("""class="country-list"""")
  }

  // The front door is BRAND chrome, not a country's site. It used to be English
  // by accident (the apex sat on the UK pod); the deployment that can answer it
  // now is the one mounted at the root, which is Poland — so the language is
  // pinned rather than inherited, and a visitor to showtimes.cc is not asked
  // "Wybierz kraj".
  it should "speak the brand's language, not the serving deployment's" in {
    // The implicit `Messages` this controller carries is Polish (the deployment
    // that can answer the front door is the one at the root, i.e. Poland), so a
    // picker that inherited it would read "Wybierz kraj" on showtimes.cc.
    val html = bodyOn("showtimes.cc")
    html should include ("""<html lang="en"""")
    html should include ("Choose your country")
    html should not include ("Wybierz")
  }

  "a returning visitor's city bounce" should "stay inside the deployment's mount point" in {
    val uk = new LandingController(Helpers.stubControllerComponents(), models.Country.UnitedKingdom)
    val res = uk.index().apply(FakeRequest("GET", "/")
      .withHeaders("X-Forwarded-Host" -> "showtimes.cc")
      .withCookies(play.api.mvc.Cookie("city", "kent")))
    redirectLocation(res) shouldBe Some("/uk/kent/")
  }

  it should "be unprefixed on the country that owns its domain" in {
    val res = controller.index().apply(FakeRequest("GET", "/")
      .withHeaders("X-Forwarded-Host" -> "kinowo.net")
      .withCookies(play.api.mvc.Cookie("city", "poznan")))
    redirectLocation(res) shouldBe Some("/poznan/")
  }
}
