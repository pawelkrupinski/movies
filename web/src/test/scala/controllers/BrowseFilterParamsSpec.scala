package controllers

import models.{City, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/** The `/{city}/movies` browse facets are reached by query param, and the param
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

  /** The names these axes carried until the rename, still bound so links minted before it
   *  keep filtering. Every browse URL in the wild — a bookmark, a shared link, anything
   *  already crawled — spells them this way. */
  private val legacyParams = Set("kraj", "rezyser", "aktor", "gatunek")

  private implicit val poznan: City = City.bySlug("poznan").get

  "the browse hrefs" should "use English query-param names" in {
    BrowseHref.country("Polska")        shouldBe "/poznan/movies?country=Polska"
    BrowseHref.director("Jane Doe")     shouldBe "/poznan/movies?director=Jane+Doe"
    BrowseHref.actor("John Roe")        shouldBe "/poznan/movies?cast=John+Roe"
    BrowseHref.genre("Science Fiction") shouldBe "/poznan/movies?genre=Science+Fiction"
  }

  // Same mount-point trap the canonical tag has: these are absolute `<a href>`s
  // on every card's meta rows, so on the shared brand domain a prefix-less one
  // sends the visitor to `showtimes.cc/kent/…` — off this country's site, and a
  // 404. The prefix comes off the CITY, which is the only thing these builders
  // are handed.
  it should "carry the mount point of a country that shares the brand domain" in {
    implicit val kent: City = City.bySlug("kent").getOrElse(fail("no city 'kent'"))
    BrowseHref.country("United Kingdom") shouldBe "/uk/kent/movies?country=United+Kingdom"
    BrowseHref.director("Jane Doe")      shouldBe "/uk/kent/movies?director=Jane+Doe"
    BrowseHref.actor("John Roe")         shouldBe "/uk/kent/movies?cast=John+Roe"
    BrowseHref.genre("Comedy")           shouldBe "/uk/kent/movies?genre=Comedy"
  }

  it should "only emit params the routes file actually binds" in {
    // Off the classpath, not the filesystem — the spec's working directory
    // differs between an sbt module run and a full-build run.
    val stream = getClass.getResourceAsStream("/routes")
    stream should not be null
    val source = scala.io.Source.fromInputStream(stream)
    val line = try source.getLines().find(_.contains("/:city/movies")) finally source.close()
    line.isDefined shouldBe true

    // `browse(city: String, country: Option[String] ?= None, …)` — take every
    // bound name except the `city` path param.
    val bound = """(\w+):\s*Option\[String\]""".r
      .findAllMatchIn(line.get).map(_.group(1)).toSet
    bound shouldBe (expectedParams ++ legacyParams)

    val emitted = Seq(
      BrowseHref.country("x"), BrowseHref.director("x"),
      BrowseHref.actor("x"),   BrowseHref.genre("x"),
    ).map(_.split('?').last.split('=').head).toSet
    withClue("BrowseHref emits a param the routes file does not bind: ") {
      emitted shouldBe expectedParams
    }
    withClue("a legacy param must stay BOUND but must never be EMITTED again: ") {
      emitted intersect legacyParams shouldBe empty
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
    val request = FakeRequest(GET, "/poznan/movies")
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

  /** The rename dropped the Polish bindings outright, and an unbound param does NOT 404 —
   *  all four options arrive `None` and `browse` falls through to `renderIndex`, so an old
   *  link returns 200 with the whole city listing instead of the facet it asked for. Wrong
   *  content under a success status is the failure nobody reports. */
  "a legacy Polish param" should "still filter on the axis it named" in {
    val request = FakeRequest(GET, "/poznan/movies")
    val cases = Seq(
      ("kraj",    controller().browse("poznan", None, None, None, None, kraj    = Some("Polska")),   "Country Match"),
      ("rezyser", controller().browse("poznan", None, None, None, None, rezyser = Some("Jane Doe")), "Director Match"),
      ("aktor",   controller().browse("poznan", None, None, None, None, aktor   = Some("John Roe")), "Cast Match"),
      ("gatunek", controller().browse("poznan", None, None, None, None, gatunek = Some("Komedia")),  "Genre Match"),
    )
    cases.foreach { case (axis, action, expectedFilm) =>
      val html = contentAsString(action.apply(request))
      withClue(s"the legacy $axis param: ") {
        html should include(expectedFilm)
        cases.map(_._3).filterNot(_ == expectedFilm).foreach(html should not include _)
      }
    }
  }

  it should "lose to the English one when a link somehow carries both" in {
    val html = contentAsString(
      controller().browse("poznan", country = Some("Polska"), None, None, None, kraj = Some("Nonsense"))
        .apply(FakeRequest(GET, "/poznan/movies")))
    html should include("Country Match")
  }
}
