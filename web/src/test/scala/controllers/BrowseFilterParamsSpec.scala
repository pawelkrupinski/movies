package controllers

import models.{City, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/** The `/{city}/filmy` browse facets are reached by query param, and the param
 *  names are English in every country — the site serves Poland, Germany and the
 *  UK off one route table, and the index page's own filters (`genre`, `country`,
 *  `director`, `cast`, `date`, …) were already English, so the browse axes match
 *  them rather than carrying Polish names only one country's users can read.
 *
 *  Two halves have to agree and live in different files: [[BrowseHref]] writes
 *  the URLs, the routes file binds them. Nothing else pins them together, so a
 *  rename on one side silently degrades every facet link to the plain city
 *  listing (an unbound param falls through to `renderIndex`). These specs pin
 *  both sides to the same names. */
class BrowseFilterParamsSpec extends AnyFlatSpec with Matchers {

  private val expectedParams = Set("country", "director", "cast", "genre")

  private implicit val poznan: City = City.bySlug("poznan").get

  "the browse hrefs" should "use English query-param names" in {
    BrowseHref.country("Polska")        shouldBe "/poznan/filmy?country=Polska"
    BrowseHref.director("Jane Doe")     shouldBe "/poznan/filmy?director=Jane+Doe"
    BrowseHref.actor("John Roe")        shouldBe "/poznan/filmy?cast=John+Roe"
    BrowseHref.genre("Science Fiction") shouldBe "/poznan/filmy?genre=Science+Fiction"
  }

  it should "only emit params the routes file actually binds" in {
    // Off the classpath, not the filesystem — the spec's working directory
    // differs between an sbt module run and a full-build run.
    val stream = getClass.getResourceAsStream("/routes")
    stream should not be null
    val source = scala.io.Source.fromInputStream(stream)
    val line = try source.getLines().find(_.contains("/:city/filmy")) finally source.close()
    line.isDefined shouldBe true

    // `browse(city: String, country: Option[String] ?= None, …)` — take every
    // bound name except the `city` path param.
    val bound = """(\w+):\s*Option\[String\]""".r
      .findAllMatchIn(line.get).map(_.group(1)).toSet
    bound shouldBe expectedParams

    val emitted = Seq(
      BrowseHref.country("x"), BrowseHref.director("x"),
      BrowseHref.actor("x"),   BrowseHref.genre("x"),
    ).map(_.split('?').last.split('=').head).toSet
    withClue("BrowseHref emits a param the routes file does not bind: ") {
      emitted shouldBe bound
    }
  }

  private def controller(): MovieController = {
    val now = LocalDateTime.now()
    def record(title: String, data: SourceData) =
      (title, Some(2024), MovieRecord(
        imdbId = Some("tt" + math.abs(title.hashCode).toString.take(7)),
        data   = Map[Source, SourceData](Helios -> data.copy(
          title       = Some(title),
          releaseYear = Some(2024),
          posterUrl   = Some("https://cinema.example/poster.jpg"),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil)),
        )),
      ))
    TestMovieController.build(Seq(
      record("Country Match",  SourceData(countries = Seq("Polska"))),
      record("Director Match", SourceData(director  = Seq("Jane Doe"))),
      record("Cast Match",     SourceData(cast      = Seq("John Roe"))),
      record("Genre Match",    SourceData(genres    = Seq("Komedia"))),
    ))._1
  }

  private def headingOf(html: String): String =
    "<title>(.*?)</title>".r.findFirstMatchIn(html).map(_.group(1)).getOrElse("")

  /** Each axis has to filter its own field. The action matches the four options
   *  positionally, so a mixed-up arm still compiles and still renders a page —
   *  only the contents are wrong. */
  "each browse axis" should "filter on the field it names" in {
    val request = FakeRequest(GET, "/poznan/filmy")
    val cases = Seq(
      ("country",  controller().browse("poznan", Some("Polska"),   None, None, None), "Country Match"),
      ("director", controller().browse("poznan", None, Some("Jane Doe"), None, None), "Director Match"),
      ("cast",     controller().browse("poznan", None, None, Some("John Roe"), None), "Cast Match"),
      ("genre",    controller().browse("poznan", None, None, None, Some("Komedia")),  "Genre Match"),
    )
    cases.foreach { case (axis, action, expectedFilm) =>
      val html = contentAsString(action.apply(request))
      withClue(s"the $axis axis: ") {
        html should include(expectedFilm)
        cases.map(_._3).filterNot(_ == expectedFilm).foreach(html should not include _)
      }
    }
  }
}
