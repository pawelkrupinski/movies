package controllers

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * `GET /:city/api/cinemas` serves the static cinema universe + area grouping the
 * mobile filter renders. A flat city returns empty `areas`; a split city returns
 * one entry per area — London's five compass names, a US state's metros —
 * together partitioning the full cinema list.
 */
class ApiCinemasSpec extends AnyFlatSpec with Matchers {

  private def controller(servingCountry: models.Country = models.Country.default): MovieController = {
    val now = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt999"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title     = Some("Test Film"),
          showtimes = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        ),
        Tmdb -> SourceData()
      )
    )
    TestMovieController.build(Seq(("Test Film", None, record)), servingCountry = servingCountry)._1
  }

  "apiCinemas for a flat city" should "list every venue and an empty areas array" in {
    val result = controller().apiCinemas("poznan")(FakeRequest())
    status(result) shouldBe OK
    val json = Json.parse(contentAsString(result))
    (json \ "cinemas").as[Seq[String]] shouldBe models.Poznan.cinemaDisplayNames
    (json \ "areas").as[Seq[play.api.libs.json.JsValue]] shouldBe empty
  }

  "apiCinemas for London (split)" should "return the five compass areas partitioning the cinema list" in {
    // London is only ours on the UK deployment — a Poland host 404s it now,
    // so this states which country's host it is testing.
    val result = controller(models.Country.UnitedKingdom).apiCinemas("london")(FakeRequest())
    status(result) shouldBe OK
    val json  = Json.parse(contentAsString(result))
    val cinemas = (json \ "cinemas").as[Seq[String]]
    cinemas shouldBe models.London.cinemaDisplayNames

    val areas = (json \ "areas").as[Seq[play.api.libs.json.JsValue]]
    areas.map(a => (a \ "name").as[String]) shouldBe Seq("Central", "North", "East", "South", "West")
    areas.map(a => (a \ "slug").as[String]) shouldBe Seq("central", "north", "east", "south", "west")

    // The areas partition the cinema universe: union == full list, no dupes.
    val grouped = areas.flatMap(a => (a \ "cinemas").as[Seq[String]])
    grouped.toSet shouldBe cinemas.toSet
    grouped.distinct should have size grouped.size.toLong.toInt
  }

  "apiCinemas for a US state (split by metro)" should "return the metro areas partitioning the cinema list" in {
    val result = controller(models.Country.UnitedStates).apiCinemas("california")(FakeRequest())
    status(result) shouldBe OK
    val json    = Json.parse(contentAsString(result))
    val cinemas = (json \ "cinemas").as[Seq[String]]

    val areas = (json \ "areas").as[Seq[play.api.libs.json.JsValue]]
    // Data-driven names, not the five compass cases: California's 486 venues are
    // grouped by their Flicks metro, biggest first, catch-all last.
    areas.map(a => (a \ "name").as[String]).take(3) shouldBe Seq("Los Angeles", "San Francisco", "Inland Empire")
    areas.map(a => (a \ "slug").as[String]).take(3) shouldBe Seq("los-angeles", "san-francisco", "inland-empire")
    (areas.last \ "name").as[String] shouldBe "Other areas"

    val grouped = areas.flatMap(a => (a \ "cinemas").as[Seq[String]])
    grouped.toSet shouldBe cinemas.toSet
    grouped.distinct should have size grouped.size.toLong.toInt
  }

  "apiCinemas for an unknown city" should "404" in {
    status(controller().apiCinemas("nieznane")(FakeRequest())) shouldBe NOT_FOUND
  }
}
