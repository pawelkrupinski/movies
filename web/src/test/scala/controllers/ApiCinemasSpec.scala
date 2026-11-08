package controllers

import models.{SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._


/**
 * `GET /:city/api/cinemas` serves the static cinema universe + area grouping the
 * mobile filter renders. A flat city returns empty `areas`; a split city returns
 * one entry per area — London's five compass names, a US state's metros —
 * together partitioning the full cinema list.
 */
class ApiCinemasSpec extends AnyFlatSpec with Matchers {

  private def controller(servingCountry: models.Country = models.Country.default): TestCityControllers = {
    TestCityControllers.build(Seq(TestCityControllers.showing("Test Film", imdbId = Some("tt999"),
      otherSources = Map(Tmdb -> SourceData()))), servingCountry = servingCountry)
  }

  "apiCinemas for a flat city" should "list every venue and an empty areas array" in {
    val result = controller().api.apiCinemas("poznan")(FakeRequest())
    status(result) shouldBe OK
    val json = Json.parse(contentAsString(result))
    (json \ "cinemas").as[Seq[String]] shouldBe models.Poznan.cinemaDisplayNames
    (json \ "areas").as[Seq[play.api.libs.json.JsValue]] shouldBe empty
  }

  "apiCinemas for London (split)" should "return the five compass areas partitioning the cinema list" in {
    // London is only ours on the UK deployment — a Poland host 404s it now,
    // so this states which country's host it is testing.
    val result = controller(models.Country.UnitedKingdom).api.apiCinemas("london")(FakeRequest())
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

  "apiCinemas for a US page" should "list its own venues flat, with no areas" in {
    // Los Angeles was a 133-venue metro split into twelve regions; it is the city
    // alone now, one list — no US page is big enough to need areas.
    val result = controller(models.Country.UnitedStates).api.apiCinemas("los-angeles")(FakeRequest())
    status(result) shouldBe OK
    val json = Json.parse(contentAsString(result))
    (json \ "cinemas").as[Seq[String]] shouldBe models.Country.UnitedStates.bySlug.get("los-angeles").get.cinemaDisplayNames
    (json \ "areas").as[Seq[play.api.libs.json.JsValue]] shouldBe empty
  }

  "apiCinemas" should "leave seasons out entirely for a city with no seasonal venue" in {
    val json = Json.parse(contentAsString(controller().api.apiCinemas("poznan")(FakeRequest())))
    (json \ "seasons").toOption shouldBe None
  }

  private val kreuzberg = models.GermanRoster.theaterIdByCinema.map(_.swap)("A2198")    // Freiluftkino Kreuzberg
  private val berlin    = models.City.all.find(_.cinemas.contains(kreuzberg)).get
  private def at(instant: String) = java.time.Clock.fixed(java.time.Instant.parse(instant), java.time.ZoneOffset.UTC)
  private def germanController(clock: java.time.Clock): TestCityControllers =
    TestCityControllers.build(Seq.empty, servingCountry = models.Country.Germany, clock = clock)

  it should "name a city's seasonal venues with their window as month numbers, and whether today is outside it" in {
    val json      = Json.parse(contentAsString(germanController(at("2026-11-20T12:00:00Z")).api.apiCinemas(berlin.slug)(FakeRequest())))
    val seasons   = (json \ "seasons").as[Seq[play.api.libs.json.JsValue]]
    seasons should contain (Json.obj("cinema" -> "Freiluftkino Kreuzberg", "opens" -> 5, "closes" -> 9, "offSeason" -> true))
    seasons.map(s => (s \ "cinema").as[String]) should not contain "Kino Central Berlin"
    seasons.map(s => (s \ "cinema").as[String]).toSet.subsetOf((json \ "cinemas").as[Seq[String]].toSet) shouldBe true
  }

  it should "read the server's own clock: in July the open-air screen is in season" in {
    val json = Json.parse(contentAsString(germanController(at("2026-07-15T12:00:00Z")).api.apiCinemas(berlin.slug)(FakeRequest())))
    (json \ "seasons").as[Seq[play.api.libs.json.JsValue]].find(s => (s \ "cinema").as[String] == "Freiluftkino Kreuzberg")
      .map(s => (s \ "offSeason").as[Boolean]) shouldBe Some(false)
  }

  // The web filter panel reads the same verdict off the listing page, never re-deriving it.
  "the listing page" should "hand its script the seasonal venues with the server's off-season verdict" in {
    val html = contentAsString(germanController(at("2026-11-20T12:00:00Z")).movie.index(berlin.slug)(FakeRequest()))
    html should include ("window.CINEMA_SEASONS")
    html should include (""""Freiluftkino Kreuzberg":{"opens":5,"closes":9,"offSeason":true}""")
  }

  it should "emit no seasons where the city has none" in {
    contentAsString(controller().movie.index("poznan")(FakeRequest())) should not include "CINEMA_SEASONS"
  }

  // The payload is cached until the city's data moves, and nothing in the read model moves
  // when a season ends: only the calendar does. The validator has to retire a copy that said
  // "in season" the night the month turns, or a client revalidating on October 1st is told
  // 304 and keeps September's verdict until the city next changes.
  "a held seasons verdict" should "be retired at the month boundary, not 304'd, though the city's data never moved" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-30T21:30:00Z"))   // 23:30 in Berlin
    val controller = germanController(clock)
    def kreuzbergOffSeason(result: scala.concurrent.Future[play.api.mvc.Result]) =
      (Json.parse(contentAsString(result)) \ "seasons").as[Seq[play.api.libs.json.JsValue]]
        .find(s => (s \ "cinema").as[String] == "Freiluftkino Kreuzberg").map(s => (s \ "offSeason").as[Boolean])

    val september = controller.api.apiCinemas(berlin.slug)(FakeRequest())
    kreuzbergOffSeason(september) shouldBe Some(false)
    val etag = header("ETag", september).get
    status(controller.api.apiCinemas(berlin.slug)(FakeRequest().withHeaders("If-None-Match" -> etag))) shouldBe NOT_MODIFIED

    clock.setTo(java.time.Instant.parse("2026-09-30T22:30:00Z"))                               // 00:30 on 1 October
    val october = controller.api.apiCinemas(berlin.slug)(FakeRequest().withHeaders("If-None-Match" -> etag))
    status(october) shouldBe OK
    kreuzbergOffSeason(october) shouldBe Some(true)
    header("ETag", october) should not be Some(etag)

    // The listing page's `window.CINEMA_SEASONS` rides the same validator.
    clock.setTo(java.time.Instant.parse("2026-09-30T21:30:00Z"))
    val page = controller.movie.index(berlin.slug)(FakeRequest())
    contentAsString(page) should include (""""Freiluftkino Kreuzberg":{"opens":5,"closes":9,"offSeason":false}""")
    clock.setTo(java.time.Instant.parse("2026-09-30T22:30:00Z"))
    val nextPage = controller.movie.index(berlin.slug)(FakeRequest().withHeaders("If-None-Match" -> header("ETag", page).get))
    status(nextPage) shouldBe OK
    contentAsString(nextPage) should include (""""Freiluftkino Kreuzberg":{"opens":5,"closes":9,"offSeason":true}""")
  }

  "apiCinemas for an unknown city" should "404" in {
    status(controller().api.apiCinemas("nieznane")(FakeRequest())) shouldBe NOT_FOUND
  }
}
