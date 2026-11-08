package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._


/** The detail page and the browse facets moved off their Polish spellings
 *  (`/{city}/film…`, `/{city}/filmy`) onto `/{city}/movie…` and
 *  `/{city}/movies`. The old addresses cannot simply go: they are what the film
 *  URLs already in search indexes carry, what every link shared before the
 *  rename carries, and what the share / copy-link buttons of every installed
 *  app build still mint. So they 301.
 *
 *  What each case here guards is a way that redirect can be silently wrong
 *  rather than absent — a dropped query string (a 200 with the wrong content,
 *  the failure mode nobody reports), and a dropped mount prefix on a country
 *  that shares the brand domain (a redirect off its own site). */
class RenamedFilmPathRedirectSpec extends AnyFlatSpec with Matchers {

  private def controller(country: models.Country = models.Country.default): LegacyRedirectController = {
    TestCityControllers.build(Seq(TestCityControllers.showing("Incepcja", Some(2010), imdbId = Some("tt1375666"),
      posterUrl = Some("https://cinema.example/poster.jpg"))), servingCountry = country).legacy
  }

  private def locationOf(result: scala.concurrent.Future[play.api.mvc.Result]): String =
    header(LOCATION, result).getOrElse(fail("no Location header"))

  "the pre-rename slug address" should "301 onto its /movie spelling" in {
    val result = controller().filmSubPathLegacy("poznan", "incepcja-2010")
      .apply(FakeRequest(GET, "/poznan/film/incepcja-2010"))
    status(result)     shouldBe MOVED_PERMANENTLY
    locationOf(result) shouldBe "/poznan/movie/incepcja-2010"
  }

  "the pre-rename title-query address" should "301 onto its /movie spelling, query intact" in {
    val result = controller().filmLegacy("poznan")
      .apply(FakeRequest(GET, "/poznan/film?title=Incepcja"))
    status(result)     shouldBe MOVED_PERMANENTLY
    locationOf(result) shouldBe "/poznan/movie?title=Incepcja"
  }

  "the pre-rename browse address" should "301 onto /movies carrying every facet" in {
    // Both spellings of the facet params ride the query string, the legacy
    // Polish `gatunek` included — dropping them would render the UNFILTERED
    // city listing under a URL that promised a genre.
    val result = controller().browseLegacy("poznan")
      .apply(FakeRequest(GET, "/poznan/filmy?genre=Komedia&gatunek=Komedia"))
    status(result)     shouldBe MOVED_PERMANENTLY
    locationOf(result) shouldBe "/poznan/movies?genre=Komedia&gatunek=Komedia"
  }

  it should "301 onto the bare listing when there is no facet" in {
    val result = controller().browseLegacy("poznan").apply(FakeRequest(GET, "/poznan/filmy"))
    status(result)     shouldBe MOVED_PERMANENTLY
    locationOf(result) shouldBe "/poznan/movies"
  }

  // Play strips `play.http.context` before matching, so the route's own `:city`
  // is all a redirect built from the request would see — and `showtimes.cc/uk/…`
  // would be answered with a Location pointing at `showtimes.cc/…`, off this
  // country's site entirely. The mount point comes off the CITY instead.
  "a country that shares the brand domain" should "keep its mount prefix in the redirect" in {
    val uk = controller(models.Country.UnitedKingdom)
    locationOf(uk.filmSubPathLegacy("canterbury", "inception-2010")
      .apply(FakeRequest(GET, "/canterbury/film/inception-2010"))) shouldBe "/uk/canterbury/movie/inception-2010"
    locationOf(uk.browseLegacy("canterbury")
      .apply(FakeRequest(GET, "/canterbury/filmy?genre=Comedy"))) shouldBe "/uk/canterbury/movies?genre=Comedy"
  }

  "an unknown city" should "404 rather than redirect onto a URL that 404s one hop later" in {
    val result = controller().filmSubPathLegacy("atlantyda", "incepcja-2010")
      .apply(FakeRequest(GET, "/atlantyda/film/incepcja-2010"))
    status(result) shouldBe NOT_FOUND
  }

  // The routes file is the only place that says the old addresses are still
  // REACHABLE — without these lines the actions above are dead code that no
  // request can ever land on, and every link minted before the rename 404s.
  "the routes file" should "still bind both pre-rename addresses" in {
    val stream = getClass.getResourceAsStream("/routes")
    stream should not be null
    val source = scala.io.Source.fromInputStream(stream)
    val lines  = try source.getLines().toList finally source.close()

    lines.exists(_.startsWith("GET     /:city/film "))    shouldBe true
    lines.exists(_.contains("/:city/film/*rest"))         shouldBe true
    lines.exists(_.startsWith("GET     /:city/filmy "))   shouldBe true
  }
}
