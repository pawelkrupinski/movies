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
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "kinowo.fly.dev")

  "robots.txt" should "stay crawlable, advertise the sitemap, and fence off noise" in {
    val res  = controller().robotsTxt(req("/robots.txt"))
    status(res)      shouldBe OK
    contentType(res) shouldBe Some("text/plain")
    val body = contentAsString(res)
    body should include("User-agent: *")
    body should include("Allow: /")
    body should include("Sitemap: https://kinowo.fly.dev/sitemap.xml")
    body should include("Disallow: /debug")
    body should include("Disallow: /admin")
    body should include("Disallow: /*/api/")
    // og:image PNGs must stay crawlable — Facebook honours robots.txt for them.
    body should not include "og-image"
  }

  "sitemap.xml" should "enumerate the landing, the city, its plan, and live films" in {
    val res = controller().sitemap(req("/sitemap.xml"))
    status(res)      shouldBe OK
    contentType(res) shouldBe Some("application/xml")
    header("Cache-Control", res) shouldBe Some("public, max-age=3600")
    val body = contentAsString(res)
    body should include("<urlset")
    body should include("<loc>https://kinowo.fly.dev/</loc>")
    body should include("<loc>https://kinowo.fly.dev/poznan/</loc>")
    body should include("<loc>https://kinowo.fly.dev/poznan/plan</loc>")
    body should include("<loc>https://kinowo.fly.dev/poznan/film?title=Testowy%20Film</loc>")
  }
}
