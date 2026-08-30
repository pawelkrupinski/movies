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
    status(ctrl.film("nieznane", "x")(FakeRequest(GET, "/nieznane/movie?title=x"))) shouldBe NOT_FOUND
    status(ctrl.apiRepertoire("nieznane")(FakeRequest(GET, "/nieznane/api/repertoire"))) shouldBe NOT_FOUND
    status(ctrl.apiDetails("nieznane")(FakeRequest(GET, "/nieznane/api/details"))) shouldBe NOT_FOUND
  }

  /**
   * `City.all` is the union across every country, so a slug this deployment
   * doesn't serve still RESOLVES — Berlin is a real city, just not a Polish one.
   * Serving it 200 with an empty body is worse than useless: it looks like a
   * successful "no screenings" answer, and a client that caches it (with the
   * `Last-Modified` this deployment stamps) can then be told 304 by the German
   * deployment and strand an empty listing on a city that has a full one. That
   * is exactly how a cross-country deep link came up as "no screenings" in the
   * iOS app. A city this host doesn't serve is a 404, same as an unknown one —
   * the scope `sitemap` already applies for the same reason.
   */
  "A city from another country's deployment" should "404, not answer 200 with an empty listing" in {
    val ctrl = buildController()   // no KINOWO_COUNTRY set → the Poland default
    status(ctrl.apiRepertoire("berlin")(FakeRequest(GET, "/berlin/api/repertoire"))) shouldBe NOT_FOUND
    status(ctrl.apiDetails("berlin")(FakeRequest(GET, "/berlin/api/details")))       shouldBe NOT_FOUND
    status(ctrl.apiCinemas("berlin")(FakeRequest(GET, "/berlin/api/cinemas")))       shouldBe NOT_FOUND
    status(ctrl.index("london")(FakeRequest(GET, "/london/")))                        shouldBe NOT_FOUND
  }

  /** The scope is the country, not the data: a city this host serves that simply
   *  has nothing on today still answers 200 with an empty list. Confusing the
   *  two would 404 every quiet city overnight. */
  it should "still serve a city of this country that has no films" in {
    val ctrl = buildController()
    val res  = ctrl.apiRepertoire("krakow")(FakeRequest(GET, "/krakow/api/repertoire"))
    status(res) shouldBe OK
    contentAsString(res) shouldBe "[]"
  }

  "The Poznań index" should "render with the city label and city-prefixed links" in {
    val ctrl = buildController()
    val res  = ctrl.index("poznan")(FakeRequest(GET, "/poznan/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include("Repertuar kinowy Poznań")
    // Film-card links carry the city prefix (the navbar no longer holds a
    // city-scoped home tab — the film grid's `/{city}/movie` links are the
    // city-prefixed links the page now renders).
    html should include("""href="/poznan/movie""")
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
    html should include("""href="/wroclaw/movie""")
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
    html should include("""href="/warszawa/movie""")
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
