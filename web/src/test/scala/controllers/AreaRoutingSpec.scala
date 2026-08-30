package controllers

import models.{City, Cinema, CinemaAreaGroup, Country, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * The metro level: `/{city}/` is a PICK SCREEN for a city big enough to need
 * one (California lists 486 venues in one 18.9 MB page), and the films move one
 * level down to `/{city}/{area}/`.
 *
 * Everything here is about the three ways this can go wrong: a city that should
 * NOT get a chooser quietly getting one (London), an area page leaking films
 * from the rest of the state, and an unknown area answering 200 with the
 * unfiltered city listing instead of 404.
 */
class AreaRoutingSpec extends AnyFlatSpec with Matchers {

  private def state(slug: String): City = City.all.find(_.slug == slug).getOrElse(fail(s"no city '$slug'"))
  private def group(city: City, areaSlug: String): CinemaAreaGroup =
    city.areaBySlug(areaSlug).getOrElse(fail(s"no area '$areaSlug' in ${city.slug}"))

  private val california = state("california")
  private val losAngeles = group(california, "los-angeles")
  private val sanFrancisco = group(california, "san-francisco")

  private val laCinema: Cinema = losAngeles.cinemas.head
  private val sfCinema: Cinema = sanFrancisco.cinemas.head

  private val LaFilm = "Angeleno Feature"
  private val SfFilm = "Bay Area Feature"

  private def filmIn(cinema: Cinema, title: String, imdb: String): MovieRecord = {
    val now = LocalDateTime.now()
    MovieRecord(
      imdbId = Some(imdb),
      data = Map[Source, SourceData](
        cinema -> SourceData(
          title       = Some(title),
          releaseYear = Some(2024),
          // Two days out so the "future only" prune keeps them whatever zone the
          // suite runs in (California is eight hours behind the build machine).
          showtimes   = Seq(models.Showtime(now.plusDays(2), None, None, Nil)),
        )
      )
    )
  }

  /** A US-serving deployment holding one film in an LA venue and one in an SF
   *  venue — the two are in the same city (California) but different areas. */
  private def usController(): MovieController = TestMovieController.build(
    Seq(
      (LaFilm, Some(2024), filmIn(laCinema, LaFilm, "tt101")),
      (SfFilm, Some(2024), filmIn(sfCinema, SfFilm, "tt102")),
    ),
    servingCountry = Country.UnitedStates,
    // A US host renders English — assert on the copy it actually serves.
    messages       = testsupport.TestMessages.forLang("en"),
  )._1

  private def req(path: String) =
    FakeRequest(GET, path).withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "us.showtimes.cc")

  // ── The chooser screen ──────────────────────────────────────────────────────

  "/california/" should "serve the metro chooser, not the state-wide listing" in {
    val res = usController().index("california")(req("/california/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include("Choose an area")
    html should include("Los Angeles")
    html should include("San Francisco")
    html should include("Other areas")
    html should include("""href="/california/los-angeles/"""")
    // The whole point: no film cards on the chooser.
    html should not include LaFilm
    html should not include SfFilm
    html should not include "film-grid"
  }

  it should "show each metro's cinema count and keep City.areas' order" in {
    val html = contentAsString(usController().index("california")(req("/california/")))
    html should include(s"${losAngeles.cinemas.size} cinemas")
    // Biggest metro first, the catch-all last — the order the model already
    // publishes, so the page never re-sorts it into something else.
    val order = california.areas.map(g => html.indexOf(s"/california/${g.area.slug}/"))
    order.foreach(_ should be >= 0)
    order shouldBe order.sorted
  }

  it should "offer a way back to the country's city list" in {
    contentAsString(usController().index("california")(req("/california/"))) should include("""href="/"""")
  }

  it should "remember the city, so the bare / bounces back here" in {
    val res = usController().index("california")(req("/california/"))
    cookies(res).get("city").map(_.value) shouldBe Some("california")
  }

  it should "be a fraction of the state-wide listing it replaces" in {
    val chooser = contentAsString(usController().index("california")(req("/california/")))
    val listing = contentAsString(usController().area("california", "los-angeles")(req("/california/los-angeles/")))
    chooser.length should be < listing.length
  }

  // ── The scoped listing ──────────────────────────────────────────────────────

  "/california/los-angeles/" should "render the repertoire scoped to that metro only" in {
    val res = usController().area("california", "los-angeles")(req("/california/los-angeles/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include(LaFilm)
    // The San Francisco film plays in the same CITY — scoping is by area, and
    // this is the assertion that fails if the filter is dropped.
    html should not include SfFilm
  }

  it should "offer only that metro's cinemas to the filter panel" in {
    val html = contentAsString(usController().area("california", "los-angeles")(req("/california/los-angeles/")))
    html should include(laCinema.displayName)
    html should not include sfCinema.displayName
    // One area IS the whole page, so there is no grouping left to render.
    html should include("window.CINEMA_AREAS       = []")
  }

  it should "canonicalise to the area URL, not the city's" in {
    val html = contentAsString(usController().area("california", "los-angeles")(req("/california/los-angeles/")))
    html should include("""<link rel="canonical" href="https://us.showtimes.cc/california/los-angeles/"""")
    html should include("""content="https://us.showtimes.cc/california/los-angeles/"""")
  }

  it should "title and describe itself by the metro, not the state" in {
    val html = contentAsString(usController().area("california", "los-angeles")(req("/california/los-angeles/")))
    html should include("<title>Cinema listings in Los Angeles")
  }

  it should "keep the city cookie at the city, so / still resolves" in {
    val res = usController().area("california", "los-angeles")(req("/california/los-angeles/"))
    cookies(res).get("city").map(_.value) shouldBe Some("california")
  }

  // ── The failure mode that must stay a 404 ───────────────────────────────────

  "An unknown area" should "404, never fall through to the unfiltered city listing" in {
    val res = usController().area("california", "atlantis")(req("/california/atlantis/"))
    status(res) shouldBe NOT_FOUND
    // The tell of the bug: a 200 carrying every film in the state.
    contentAsString(res) should not include SfFilm
  }

  "An area URL under a city with no chooser" should "404" in {
    // Flat city, and a split-but-small one. Neither has area URLs at all.
    val us = usController()
    status(us.area("alaska", "anchorage")(req("/alaska/anchorage/"))) shouldBe NOT_FOUND
  }

  "An area URL under an unknown or foreign city" should "404 like every other city-scoped route" in {
    val us = usController()
    status(us.area("nieznane", "los-angeles")(req("/nieznane/los-angeles/"))) shouldBe NOT_FOUND
    // Poznań is real, but not on a US host.
    status(us.area("poznan", "los-angeles")(req("/poznan/los-angeles/")))     shouldBe NOT_FOUND
  }

  // ── What must NOT change ────────────────────────────────────────────────────

  /** London is split (five compass areas) but reads fine as one page, and it is
   *  the screen the chooser was modelled on rather than a target for it. */
  "London" should "keep serving its own listing, with no area URLs" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = Country.UnitedKingdom,
                                       messages = testsupport.TestMessages.forLang("en"))._1
    val res = uk.index("london")(req("/london/"))
    status(res) shouldBe OK
    contentAsString(res) should not include "Choose an area"
    status(uk.area("london", "central")(req("/london/central/"))) shouldBe NOT_FOUND
  }

  "A flat city" should "be untouched — its index is still its listing" in {
    val pl = TestMovieController.build(
      Seq(("Testowy Film", Some(2024), filmIn(models.Helios, "Testowy Film", "tt1")))
    )._1
    val html = contentAsString(pl.index("poznan")(req("/poznan/")))
    html should include("Testowy Film")
    html should not include "Wybierz obszar"
  }

  /** Load-bearing: the iOS and Android apps read the `/:city/api/…` endpoints
   *  and have no metro level. They must keep receiving the WHOLE city. */
  "The mobile API" should "stay city-wide on a chooser city" in {
    val us = usController()
    val repertoire = contentAsString(us.apiRepertoire("california")(req("/california/api/repertoire")))
    repertoire should include(LaFilm)
    repertoire should include(SfFilm)

    val details = contentAsString(us.apiDetails("california")(req("/california/api/details")))
    status(us.apiDetails("california")(req("/california/api/details"))) shouldBe OK
    details should not be empty

    val cinemas = contentAsString(us.apiCinemas("california")(req("/california/api/cinemas")))
    cinemas should include(laCinema.displayName)
    cinemas should include(sfCinema.displayName)
    // The area grouping the mobile filter renders is still every area.
    cinemas should include("Los Angeles")
    cinemas should include("San Francisco")
  }

  /** `/{city}/filmy` with no filter axis is the main listing — it must follow
   *  the chooser rather than staying a back door to the 18.9 MB page. */
  "/california/filmy with no filter axis" should "serve the chooser too" in {
    val res = usController().browse("california", None, None, None, None)(req("/california/filmy"))
    status(res) shouldBe OK
    contentAsString(res) should include("Choose an area")
  }
}
