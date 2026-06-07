package controllers

import models.{CinemaCityWroclavia, Helios, Kinoteka, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * Every page/API is now city-scoped under `/{city}/`. This pins the resolution
 * contract: a known slug renders, an unknown slug 404s, and the rendered page
 * carries the city's label + city-prefixed links.
 */
class CityRoutingSpec extends AnyFlatSpec with Matchers {

  private def buildController(): MovieController = {
    val now    = LocalDateTime.now()
    def filmIn(cinema: models.Cinema, title: String, imdb: String) = MovieRecord(
      imdbId = Some(imdb),
      data = Map[Source, SourceData](
        cinema -> SourceData(
          title       = Some(title),
          releaseYear = Some(2024),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        )
      )
    )
    // One film per city, each in a cinema scoped to that city, so the
    // city-scoping of the read path can be asserted from every side.
    TestMovieController.build(Seq(
      ("Testowy Film",    Some(2024), filmIn(Helios, "Testowy Film", "tt1")),
      ("Wrocławski Film", Some(2024), filmIn(CinemaCityWroclavia, "Wrocławski Film", "tt2")),
      ("Warszawski Film", Some(2024), filmIn(Kinoteka, "Warszawski Film", "tt3")),
    ))._1
  }

  "An unknown city slug" should "404 on every city-scoped route" in {
    val ctrl = buildController()
    status(ctrl.index("nieznane")(FakeRequest(GET, "/nieznane/")))               shouldBe NOT_FOUND
    status(ctrl.film("nieznane", "x")(FakeRequest(GET, "/nieznane/film?title=x"))) shouldBe NOT_FOUND
    status(ctrl.apiRepertoire("nieznane")(FakeRequest(GET, "/nieznane/api/repertoire"))) shouldBe NOT_FOUND
    status(ctrl.apiDetails("nieznane")(FakeRequest(GET, "/nieznane/api/details"))) shouldBe NOT_FOUND
  }

  "The Poznań index" should "render with the city label and city-prefixed links" in {
    val ctrl = buildController()
    val res  = ctrl.index("poznan")(FakeRequest(GET, "/poznan/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include("Repertuar kinowy Poznań")
    // Film-card links carry the city prefix (the navbar no longer holds a
    // city-scoped home tab — the film grid's `/{city}/film` links are the
    // city-prefixed links the page now renders).
    html should include("""href="/poznan/film""")
    // The lone fixture film is in a Poznań cinema → present.
    html should include("Testowy Film")
  }

  "apiRepertoire for a known city" should "200 and list the city's films" in {
    val ctrl = buildController()
    val res  = ctrl.apiRepertoire("poznan")(FakeRequest(GET, "/poznan/api/repertoire"))
    status(res) shouldBe OK
    contentAsString(res) should include("Testowy Film")
  }

  "The Wrocław index" should "render its own film + label + city-prefixed links and no other city's film" in {
    val ctrl = buildController()
    val res  = ctrl.index("wroclaw")(FakeRequest(GET, "/wroclaw/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include("Repertuar kinowy Wrocław")
    html should include("""href="/wroclaw/film""")
    html should include("Wrocławski Film")
    html should not include "Testowy Film"   // Poznań
    html should not include "Warszawski Film" // Warszawa
  }

  "The Warszawa index" should "render its own film + label + city-prefixed links and no other city's film" in {
    val ctrl = buildController()
    val res  = ctrl.index("warszawa")(FakeRequest(GET, "/warszawa/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include("Repertuar kinowy Warszawa")
    html should include("""href="/warszawa/film""")
    html should include("Warszawski Film")
    html should not include "Testowy Film"    // Poznań
    html should not include "Wrocławski Film"  // Wrocław
  }

  // The read path is city-scoped: a film plays only in its city's cinemas, so
  // it must NOT surface under another city even though the global cache holds
  // every city's films. This is the core of the "city = cinema subset" model.
  "A film playing only in a Poznań cinema" should "be absent from another city's repertoire" in {
    val ctrl = buildController()
    val res  = ctrl.apiRepertoire("warszawa")(FakeRequest(GET, "/warszawa/api/repertoire"))
    status(res) shouldBe OK
    val body = contentAsString(res)
    body should include("Warszawski Film")
    body should not include "Testowy Film"
  }
}
