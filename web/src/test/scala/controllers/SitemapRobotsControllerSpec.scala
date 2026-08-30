package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/** End-to-end checks on the two crawl-control endpoints: robots.txt advertises
 *  the sitemap + fences off the operational noise (while keeping `Allow: /` for
 *  Facebook's scraper), and sitemap.xml enumerates the live corpus. */
class SitemapRobotsControllerSpec extends AnyFlatSpec with Matchers {

  private def controller(): MovieController = {
    val now = LocalDateTime.now()
    val rec = MovieRecord(
      imdbId = Some("tt1"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Testowy Film"),
          releaseYear = Some(2024),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil)),
        )
      )
    )
    TestMovieController.build(Seq(("Testowy Film", Some(2024), rec)))._1
  }

  // X-Forwarded-* mirror the Fly edge so PageMeta.origin yields the prod host.
  private def req(path: String) =
    FakeRequest(GET, path)
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "kinowo.net")

  "robots.txt" should "stay crawlable, advertise the sitemap, and fence off noise" in {
    val res  = controller().robotsTxt(req("/robots.txt"))
    status(res)      shouldBe OK
    contentType(res) shouldBe Some("text/plain")
    val body = contentAsString(res)
    body should include("User-agent: *")
    body should include("Allow: /")
    body should include("Sitemap: https://kinowo.net/sitemap.xml")
    body should include("Disallow: /debug")
    body should include("Disallow: /admin")
    body should include("Disallow: /*/api/")
    // og:image PNGs must stay crawlable — Facebook honours robots.txt for them.
    body should not include "og-image"
  }

  it should "fence off the browse facets, which the sitemap deliberately omits" in {
    val body = contentAsString(controller().robotsTxt(req("/robots.txt")))
    body should include("Disallow: /*/filmy")
  }

  it should "keep the film deep-links crawlable — they carry the long tail" in {
    val body = contentAsString(controller().robotsTxt(req("/robots.txt")))
    body should not include "Disallow: /*/film\n"
    body should not include "Disallow: /*/film?"
  }

  "sitemap.xml" should "enumerate the landing, the city, its plan, and live films" in {
    val res = controller().sitemap(req("/sitemap.xml"))
    status(res)      shouldBe OK
    contentType(res) shouldBe Some("application/xml")
    header("Cache-Control", res) shouldBe Some("public, max-age=3600")
    val body = contentAsString(res)
    body should include("<urlset")
    body should include("<loc>https://kinowo.net/</loc>")
    body should include("<loc>https://kinowo.net/poznan/</loc>")
    body should include("<loc>https://kinowo.net/poznan/plan</loc>")
    body should include("<loc>https://kinowo.net/poznan/film/testowy-film</loc>")
  }

  /** A chooser city's `/{slug}/` is the metro pick screen — the crawlable
   *  listings are the per-area URLs, so the sitemap has to name them or the long
   *  tail of a 486-venue state is reachable only by clicking through. */
  it should "advertise the per-area listings of a city whose index is a chooser" in {
    val us   = TestMovieController.build(Seq.empty, servingCountry = models.Country.UnitedStates)._1
    val body = contentAsString(us.sitemap(req("/sitemap.xml")))
    body should include("<loc>https://kinowo.net/california/</loc>")
    body should include("<loc>https://kinowo.net/california/los-angeles/</loc>")
    body should include("<loc>https://kinowo.net/california/san-francisco/</loc>")
    // A split city BELOW the chooser threshold has no area URLs to advertise…
    body should not include "/alaska/anchorage/"
    // …and neither does a flat one.
    body should include("<loc>https://kinowo.net/alaska/</loc>")
  }

  it should "scope to this deployment's country, not the global City.all" in {
    // KINOWO_COUNTRY is unset in tests → Poland. A Poland host must NOT advertise
    // the UK/Germany cities that also live in City.all (they render empty here).
    val body = contentAsString(controller().sitemap(req("/sitemap.xml")))
    body should include("/warszawa/")          // a Polish city stays
    body should not include "/london/"          // UK city — different deployment
    body should not include "/kent/"            // UK region added in the Flicks roster
    body should not include "/berlin/"          // German city
  }
}
