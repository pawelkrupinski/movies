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

  private val controller = new LandingController(Helpers.stubControllerComponents())

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

  it should "not treat a country subdomain of the apex as the apex" in {
    // uk.showtimes.cc is a COUNTRY, not the front door. Matching it here would
    // replace the UK site's homepage with a country picker.
    val html = bodyOn("uk.showtimes.cc")
    html should include ("""<ul class="city-list"""")
    html should not include ("""class="country-list"""")
  }
}
