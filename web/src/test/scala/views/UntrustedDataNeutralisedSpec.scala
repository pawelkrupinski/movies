package views

import controllers.TestMovieController
import models.{Helios, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

/**
 * Scraped text and URLs reach every public page type neutralised.
 *
 * One film carries everything an upstream site could send against us: a title and a
 * synopsis that close an inline `<script>`, and `javascript:` / `data:` URLs as its
 * poster, fallback poster, cinema page, booking link and Metacritic link. The listing,
 * the film page and a browse facet each render it; none may emit the script-closing text
 * raw or any of the URLs at all (the `/debug` pages are dev-only — `DevMode.gate` — and
 * so out of scope). `RenderSafetyLintSpec` keeps the templates on the doors this relies
 * on (`ScriptJson`, `WebHref`); this proves the doors hold on real renders.
 */
class UntrustedDataNeutralisedSpec extends AnyFlatSpec with Matchers {

  private val Breakout  = "</script><script>alert(1)</script>"
  private val Title     = s"Zło $Breakout"
  private val Hostile   = Seq("javascript:alert(2)", "data:text/html,alert3", "JaVaScRiPt:alert(4)", " javascript:alert(5)", "data:image/svg+xml,alert6")

  private val (controller, _) = TestMovieController.build(Seq((Title, Some(2025), MovieRecord(
    metascore     = Some(70),
    metacriticUrl = Some(Hostile(3)),
    data = Map[Source, SourceData](
      Helios -> SourceData(
        title     = Some(Title),
        synopsis  = Some(s"Opis $Breakout"),
        director  = Seq("Reżyser"),
        posterUrl = Some(Hostile(0)),
        filmUrl   = Some(Hostile(2)),
        showtimes = Seq(Showtime(TestMovieController.now.plusHours(2), Some(Hostile(1)), None, Nil))),
      Tmdb -> SourceData(posterUrl = Some(Hostile(4))))))))

  private def page(result: scala.concurrent.Future[play.api.mvc.Result]): String = {
    status(result) shouldBe OK
    contentAsString(result)
  }

  private def slug: String =
    """/poznan/movie/([a-z0-9-]+)""".r.findFirstMatchIn(listing).map(_.group(1)).getOrElse(fail("the listing links no film page"))

  private lazy val listing = page(controller.index("poznan").apply(FakeRequest(GET, "/poznan/")))

  private lazy val pages: Seq[(String, String)] = Seq(
    "listing"   -> listing,
    "film page" -> page(controller.filmBySlug("poznan", slug).apply(FakeRequest(GET, s"/poznan/movie/$slug"))),
    "browse"    -> page(controller.browse("poznan", None, Some("Reżyser"), None, None).apply(FakeRequest(GET, "/poznan/movies"))),
  )

  "every public page" should "render the film (or the check below proves nothing)" in {
    pages.foreach { case (name, html) => withClue(s"$name: ")(html should include("Zło")) }
  }

  it should "never let a scraped title or synopsis close an inline script" in {
    pages.foreach { case (name, html) => withClue(s"$name: ")(html should not include "<script>alert(1)") }
  }

  it should "never emit a javascript: or data: URL it was handed" in {
    for ((name, html) <- pages; url <- Hostile) withClue(s"$name, $url: ")(html should not include url.trim)
  }
}
