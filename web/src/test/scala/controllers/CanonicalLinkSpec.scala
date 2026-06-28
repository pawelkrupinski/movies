package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/** rel=canonical contract: the city index consolidates its `/filmy` alias and
 *  every filter variation to the bare `/{city}/`, while og:url still reflects
 *  the actual (possibly filtered) URL; the film page self-canonicalises. */
class CanonicalLinkSpec extends AnyFlatSpec with Matchers {

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
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "kinowo.fly.dev")

  private def canonicalOf(html: String): Option[String] =
    """<link rel="canonical" href="([^"]+)">""".r.findFirstMatchIn(html).map(_.group(1))

  "the city index" should "canonicalise to the bare city URL" in {
    val html = contentAsString(controller().index("poznan")(req("/poznan/")))
    canonicalOf(html) shouldBe Some("https://kinowo.fly.dev/poznan/")
  }

  "the /filmy alias" should "canonicalise to the bare city URL, not /filmy" in {
    val html = contentAsString(controller().browse("poznan", None, None, None, None)(req("/poznan/filmy")))
    canonicalOf(html) shouldBe Some("https://kinowo.fly.dev/poznan/")
  }

  "a filtered index" should "canonicalise to the bare city URL while og:url keeps the filter" in {
    val html = contentAsString(controller().index("poznan")(req("/poznan/?date=tomorrow")))
    canonicalOf(html) shouldBe Some("https://kinowo.fly.dev/poznan/")
    html should include("""<meta property="og:url"         content="https://kinowo.fly.dev/poznan/?date=tomorrow">""")
  }

  "the film page" should "self-canonicalise to its own deep-link" in {
    val html = contentAsString(controller().film("poznan", "Testowy Film")(req("/poznan/film?title=Testowy%20Film")))
    canonicalOf(html) shouldBe Some("https://kinowo.fly.dev/poznan/film?title=Testowy%20Film")
  }
}
