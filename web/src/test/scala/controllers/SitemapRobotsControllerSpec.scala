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

  /** The US crawl map is its metros — the state is not an address, so naming
   *  `/california/` would sitemap a 404 while the metros people search for went
   *  unadvertised. */
  it should "advertise every US metro, and no state" in {
    val us   = TestMovieController.build(Seq.empty, servingCountry = models.Country.UnitedStates)._1
    val body = contentAsString(us.sitemap(req("/sitemap.xml")))
    body should include("<loc>https://kinowo.net/us/los-angeles/</loc>")
    body should include("<loc>https://kinowo.net/us/san-francisco/</loc>")
    // The state is how a metro is FOUND, never a page of its own.
    body should not include "/us/california/"
    // A district is a filter inside a metro, never a URL of its own.
    body should not include "/us/los-angeles/santa-monica/"
    // A state small enough to be one city is advertised as that city.
    body should include("<loc>https://kinowo.net/us/alaska/</loc>")
  }

  /** A country that shares `showtimes.cc` is served one segment down, so every
   *  `<loc>` has to carry that segment — a sitemap advertising `/kent/` on a
   *  deployment reachable at `/uk/kent/` is a file of 404s, and it is the one
   *  file a crawler trusts to enumerate the site. */
  it should "hang every URL off the mount point on a country sharing the brand domain" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = models.Country.UnitedKingdom)._1
    val body = contentAsString(uk.sitemap(
      FakeRequest(GET, "/sitemap.xml")
        .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "showtimes.cc")))
    body should include("<loc>https://showtimes.cc/uk/</loc>")
    body should include("<loc>https://showtimes.cc/uk/kent/</loc>")
    body should not include "<loc>https://showtimes.cc/kent/</loc>"
  }

  /** The brand front door owns the apex ROOT, which is the only `robots.txt` and
   *  `sitemap.xml` a crawler will ever fetch for `showtimes.cc` — the countries
   *  mounted beneath it have no host root of their own. Answered by the
   *  deployment mounted at `/`, which is the one on its own domain. */
  private def apexReq(path: String) =
    FakeRequest(GET, path)
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "showtimes.cc")

  "the front door's sitemap.xml" should "be an index of the countries mounted under the apex" in {
    val body = contentAsString(controller().sitemap(apexReq("/sitemap.xml")))
    body should include("<sitemapindex")
    body should include("<loc>https://showtimes.cc/uk/sitemap.xml</loc>")
    body should include("<loc>https://showtimes.cc/de/sitemap.xml</loc>")
    body should include("<loc>https://showtimes.cc/us/sitemap.xml</loc>")
    // Poland is a different host with a root sitemap of its own, and this
    // deployment's own cities have no business being crawled off the apex.
    body should not include "kinowo.net"
    body should not include "/poznan/"
  }

  "the front door's robots.txt" should "point at each mounted country's sitemap and fence off its noise" in {
    val body = contentAsString(controller().robotsTxt(apexReq("/robots.txt")))
    body should include("Sitemap: https://showtimes.cc/uk/sitemap.xml")
    body should include("Sitemap: https://showtimes.cc/de/sitemap.xml")
    body should include("Disallow: /uk/debug")
    body should include("Disallow: /us/*/filmy")
    // The apex is not Poland's front page, so it must not advertise Poland's.
    body should not include "kinowo.net"
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
