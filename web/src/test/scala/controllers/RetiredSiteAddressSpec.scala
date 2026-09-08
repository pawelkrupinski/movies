package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The address arithmetic a retired host does on every request it forwards. */
class RetiredSiteAddressSpec extends AnyFlatSpec with Matchers {

  "the destination" should "be the same path on the live origin" in {
    RetiredSite.destination("https://kinowo.net", "/poznan/movies", "") shouldBe "https://kinowo.net/poznan/movies"
  }

  it should "carry the query string, which is the whole request for a filtered call" in {
    RetiredSite.destination("https://kinowo.net", "/poznan/movies", "genre=Horror&date=2026-08-30") shouldBe
      "https://kinowo.net/poznan/movies?genre=Horror&date=2026-08-30"
  }

  // The incoming path already carries whatever mount prefix the request arrived
  // on, so the base it is appended to must be the bare ORIGIN. Appending it to
  // the prefixed `webUrl` instead is how `showtimes.cc/uk/uk/kent/` happens —
  // a shape this codebase has produced three separate times.
  it should "not double a country's mount prefix" in {
    val uk = models.Country.UnitedKingdom
    RetiredSite.destination(uk.webOrigin.get, "/uk/kent/", "") shouldBe "https://showtimes.cc/uk/kent/"
  }

  "a read" should "move permanently" in {
    RetiredSite.redirectStatus("GET")  shouldBe 301
    RetiredSite.redirectStatus("HEAD") shouldBe 301
  }

  // 308 keeps the method and body; a 301 on a PUT lets the client re-issue it as
  // a bodyless GET, which reads as "the write silently did nothing".
  "a write" should "move permanently without being turned into a read" in {
    RetiredSite.redirectStatus("POST")   shouldBe 308
    RetiredSite.redirectStatus("PUT")    shouldBe 308
    RetiredSite.redirectStatus("DELETE") shouldBe 308
  }

  "an API path" should "be recognised under a city prefix or bare" in {
    RetiredSite.isApiPath("/api/catalog")           shouldBe true
    RetiredSite.isApiPath("/poznan/api/repertoire")  shouldBe true
    RetiredSite.isApiPath("/poznan/movie/diuna")     shouldBe false
    RetiredSite.isApiPath("/auth/token")             shouldBe false
  }

  "a machine file" should "be robots.txt, sitemap.xml, or an og-image, and nothing else" in {
    RetiredSite.isMachineFile("/robots.txt")             shouldBe true
    RetiredSite.isMachineFile("/sitemap.xml")             shouldBe true
    RetiredSite.isMachineFile("/poznan/og-image")         shouldBe true
    RetiredSite.isMachineFile("/poznan/movie/og-image")   shouldBe true
    RetiredSite.isMachineFile("/poznan/movie/diuna")      shouldBe false
  }
}
