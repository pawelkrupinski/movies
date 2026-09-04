package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/** rel=canonical contract: the city index consolidates its `/movies` alias and
 *  every filter variation to the bare `/{city}/`, while og:url still reflects
 *  the actual (possibly filtered) URL; the film page self-canonicalises.
 *
 *  The FACETED browse pages (`?cast=`, `?director=`, `?genre=`, `?country=`)
 *  fold the same way and additionally carry `noindex,follow`: their URL space
 *  is city x every cast member, so it is combinatorial rather than large. What
 *  must not follow it is the film pages — those are the content, `follow` is
 *  what keeps a crawler walking through to them, and the assertions below pin
 *  both halves. */
class CanonicalLinkSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def controller(): MovieController = {
    val now = LocalDateTime.now()
    val rec = MovieRecord(
      imdbId = Some("tt1"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title = Some("Testowy Film"), releaseYear = Some(2024),
          showtimes = Seq(models.Showtime(now.plusHours(2), None, None, Nil)),
        )
      )
    )
    TestMovieController.build(Seq(("Testowy Film", Some(2024), rec)))._1
  }

  private def req(path: String) =
    FakeRequest(GET, path)
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "kinowo.net")

  private def canonicalOf(html: String): Option[String] =
    """<link rel="canonical" href="([^"]+)">""".r.findFirstMatchIn(html).map(_.group(1))

  private def robotsOf(html: String): Option[String] =
    """<meta name="robots"\s+content="([^"]+)">""".r.findFirstMatchIn(html).map(_.group(1))

  "the city index" should "canonicalise to the bare city URL" in {
    val html = contentAsString(controller().index("poznan")(req("/poznan/")))
    canonicalOf(html) shouldBe Some("https://kinowo.net/poznan/")
  }

  "the /movies alias" should "canonicalise to the bare city URL, not /movies" in {
    val html = contentAsString(controller().browse("poznan", None, None, None, None)(req("/poznan/movies")))
    canonicalOf(html) shouldBe Some("https://kinowo.net/poznan/")
  }

  "a filtered index" should "canonicalise to the bare city URL while og:url keeps the filter" in {
    val html = contentAsString(controller().index("poznan")(req("/poznan/?date=tomorrow")))
    canonicalOf(html) shouldBe Some("https://kinowo.net/poznan/")
    html should include("""<meta property="og:url"         content="https://kinowo.net/poznan/?date=tomorrow">""")
  }

  // On the shared brand domain the whole app is MOUNTED under a country segment
  // (`showtimes.cc/uk/…`), and Play strips that before route matching — so a URL
  // assembled from the city slug alone silently loses it. A canonical tag is the
  // worst place for that to happen: it is an instruction to search engines to
  // index the OTHER address, and the other address 404s.
  "a country sharing the brand domain" should "canonicalise under its mount point" in {
    val uk   = TestMovieController.build(Nil, servingCountry = models.Country.UnitedKingdom)._1
    val html = contentAsString(uk.index("kent")(
      FakeRequest(GET, "/kent/").withHeaders(
        "X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "showtimes.cc")))
    canonicalOf(html) shouldBe Some("https://showtimes.cc/uk/kent/")
  }

  // ── The faceted browse pages ───────────────────────────────────────────────
  //
  // robots.txt has disallowed `/{city}/movies` all along, and a crawler that
  // honours it never gets here. `meta-externalagent` made 300,040 requests to
  // exactly these paths without fetching robots.txt once, which is what these
  // two tags are for.

  "a facet page" should "canonicalise to the city listing and refuse to be indexed" in {
    val html = contentAsString(
      controller().browse("poznan", None, None, cast = Some("Tom Hanks"), None)(req("/poznan/movies?cast=Tom+Hanks")))
    canonicalOf(html) shouldBe Some("https://kinowo.net/poznan/")
    robotsOf(html)    shouldBe Some("noindex,follow")
  }

  it should "keep the crawler walking through to the film pages" in {
    val html = contentAsString(
      controller().browse("poznan", None, director = Some("Someone"), None, None)(req("/poznan/movies?director=Someone")))
    // `follow`, never `none` or `nofollow`: the film links on this page are the
    // content, and each lands somewhere that IS meant to be indexed.
    robotsOf(html).value should endWith ("follow")
    robotsOf(html).value should not include "nofollow"
  }

  "the pages we want indexed" should "carry no robots meta at all" in {
    // The absence of the tag is the default, so a page can only become
    // unindexable by someone deciding it should be.
    robotsOf(contentAsString(controller().index("poznan")(req("/poznan/")))) shouldBe None
    robotsOf(contentAsString(controller().browse("poznan", None, None, None, None)(req("/poznan/movies")))) shouldBe None
    robotsOf(contentAsString(controller().filmBySlug("poznan", "testowy-film")(req("/poznan/movie/testowy-film")))) shouldBe None
  }

  "the film page" should "self-canonicalise to its own deep-link" in {
    val html = contentAsString(controller().filmBySlug("poznan", "testowy-film")(req("/poznan/movie/testowy-film")))
    canonicalOf(html) shouldBe Some("https://kinowo.net/poznan/movie/testowy-film")
  }
}
