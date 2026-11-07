package services.cinemas.common

import models.{Cinema, CinemaMovie, KinoMuranow, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{GetOnlyHttpFetch, HttpFetch}

class ListingPageScraperSpec extends AnyFlatSpec with Matchers {

  private val PageUrl   = "https://kino.example.pl/repertuar/"
  private val PublicUrl = "https://kino.example.pl"

  /** Answers the one page with its URL as the body, so the parse can show what it was given. */
  private class PageFetch extends GetOnlyHttpFetch {
    var reads: List[String] = Nil
    def get(url: String): String = { reads = reads :+ url; s"<html>$url</html>" }
  }

  private class OnePageClient(http: HttpFetch) extends ListingPageScraper(http, PageUrl, PublicUrl) {
    val cinema: Cinema = KinoMuranow
    protected def parsePage(html: String): Seq[CinemaMovie] =
      Seq(CinemaMovie(Movie(html), cinema, None, None, None, Nil, Nil, Nil))
  }

  "A ListingPageScraper" should "read its one page on each scrape and parse that body" in {
    val http   = new PageFetch
    val client = new OnePageClient(http)
    client.fetch().map(_.movie.title) shouldBe Seq(s"<html>$PageUrl</html>")
    client.fetch()
    http.reads shouldBe List(PageUrl, PageUrl)
  }

  it should "scrape the page's host and link the venue's public page" in {
    val client = new OnePageClient(new PageFetch)
    client.scrapeHosts shouldBe CinemaScraper.hostsOf(PageUrl)
    client.sourceUrl shouldBe Some(PublicUrl)
  }

  it should "fail the scrape when the page cannot be read, rather than list nothing" in {
    val failing = new GetOnlyHttpFetch { def get(url: String): String = throw new java.io.IOException(s"down: $url") }
    an[Exception] should be thrownBy new OnePageClient(failing).fetch()
  }
}
