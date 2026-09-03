package controllers

import models.{City, Cinema, Country, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * The metro level, after the state stopped being a place. A US metro IS the
 * city — `/los-angeles/` is its listing, scoped to its own venues — and the
 * state it sits in has no page at all: `/california/` is as unknown as
 * `/atlantis/`. What survives at the level BELOW is `City.areas`: London's five
 * compass areas, and a big metro's districts, which the cinema filter groups by
 * and the first-visit picker offers.
 *
 * The ways this can go wrong are what is asserted: a metro page leaking films
 * from the rest of the state, a state answering 200 with an 18 MB listing
 * instead of 404, and London — split too, and deliberately untouched by any of
 * this — losing its areas or its one-page listing.
 */
class AreaRoutingSpec extends AnyFlatSpec with Matchers {

  private def place(slug: String): City = City.all.find(_.slug == slug).getOrElse(fail(s"no city '$slug'"))

  private val losAngeles   = place("los-angeles")
  private val sanFrancisco = place("san-francisco-bay-area")

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
   *  venue — two different California metros, and so two different cities. */
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
    FakeRequest(GET, path).withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "showtimes.cc")

  // ── The metro's own listing ─────────────────────────────────────────────────

  "/los-angeles/" should "render the repertoire scoped to that metro only" in {
    val res = usController().index("los-angeles")(req("/los-angeles/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include(LaFilm)
    // The San Francisco film is in the same STATE — the scoping is by metro, and
    // this is the assertion that fails if a metro ever swallows its state again.
    html should not include SfFilm
  }

  it should "offer only that metro's cinemas to the filter panel" in {
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include(laCinema.displayName)
    html should not include sfCinema.displayName
    // Los Angeles is past UsMetroSubAreas' threshold, so the panel it offers is
    // grouped by REGION rather than flat.
    html should include("CINEMA_AREAS")
    html should not include "CINEMA_AREAS       = []"
    html should include("\"name\":\"Westside\"")
  }

  it should "title, describe and canonicalise itself by the metro" in {
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include("<title>Cinema listings in Los Angeles")
    html should include("""<link rel="canonical" href="https://showtimes.cc/us/los-angeles/"""")
  }

  it should "remember the metro as the city, so the bare / bounces back to it" in {
    val res = usController().index("los-angeles")(req("/los-angeles/"))
    cookies(res).get("city").map(_.value) shouldBe Some("los-angeles")
  }

  it should "offer the other metros grouped by their state in the city switcher" in {
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include ("""<optgroup label="California">""")
    html should include ("""<option value="san-francisco-bay-area">San Francisco Bay Area</option>""")
    // Two of the 457 are both called "Philadelphia" — a flat list offers them as
    // two identical options, which is why the US switcher is grouped at all.
    html should include ("""<optgroup label="Pennsylvania">""")
    html should include ("""<optgroup label="New Jersey">""")
  }

  "A small metro" should "leave its cinema list flat rather than wrapping it in one group" in {
    // Below UsMetroSubAreas' threshold there are no districts, and a single
    // collapsible section around the whole list would be chrome with no choice in
    // it. San Diego (36 venues) is well under.
    val res = usController().index("san-diego")(req("/san-diego/"))
    status(res) shouldBe OK
    contentAsString(res) should include ("CINEMA_AREAS       = []")
  }

  "A flat state" should "still serve its own listing — its venue list IS the page" in {
    // Vermont: 23 venues in two metros 142 km apart, which is one drive — under
    // both `MinCinemasToSplit` and `MaxSpanToStayWholeKm`, so it stays one page.
    val res = usController().index("vermont")(req("/vermont/"))
    status(res) shouldBe OK
    contentAsString(res) should include ("CINEMA_AREAS       = []")
  }

  // ── The state is not a place ────────────────────────────────────────────────

  "A US state" should "404, never answer with the whole state's listing" in {
    val us = usController()
    Seq("california", "texas", "new-jersey").foreach { slug =>
      withClue(s"$slug: ") {
        val res = us.index(slug)(req(s"/$slug/"))
        status(res) shouldBe NOT_FOUND
        // The tell of the bug this replaced: a 200 carrying every film in the state.
        contentAsString(res) should not include SfFilm
      }
    }
  }

  "An unknown or foreign city" should "404 like every other city-scoped route" in {
    val us = usController()
    status(us.index("atlantis")(req("/atlantis/"))) shouldBe NOT_FOUND
    // Poznań is real, but not on a US host.
    status(us.index("poznan")(req("/poznan/")))     shouldBe NOT_FOUND
  }

  "/los-angeles/movies with no filter axis" should "serve the metro's listing, not a browse page" in {
    val res = usController().browse("los-angeles", None, None, None, None)(req("/los-angeles/movies"))
    status(res) shouldBe OK
    contentAsString(res) should include(LaFilm)
    contentAsString(res) should not include SfFilm
  }

  // ── The mobile API ─────────────────────────────────────────────────────────

  /** Load-bearing: the iOS and Android apps read the `/:city/api/…` endpoints and
   *  have no level below the city. They get the metro, whole. */
  "The mobile API" should "serve the metro's own universe" in {
    val us = usController()
    val repertoire = contentAsString(us.apiRepertoire("los-angeles")(req("/los-angeles/api/repertoire")))
    repertoire should include(LaFilm)
    repertoire should not include SfFilm

    status(us.apiDetails("los-angeles")(req("/los-angeles/api/details"))) shouldBe OK

    val cinemas = contentAsString(us.apiCinemas("los-angeles")(req("/los-angeles/api/cinemas")))
    cinemas should include(laCinema.displayName)
    cinemas should not include sfCinema.displayName
    // The area grouping the mobile filter renders is the metro's regions.
    cinemas should include("Westside")
  }

  // ── What must NOT change ────────────────────────────────────────────────────

  "London" should "keep serving its own listing, split into its five compass areas" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = Country.UnitedKingdom,
                                       messages = testsupport.TestMessages.forLang("en"))._1
    val res = uk.index("london")(req("/london/"))
    status(res) shouldBe OK
    val body = contentAsString(res)
    body should include ("\"name\":\"Central\"")
    body should include ("\"name\":\"South\"")
    models.London.areas.map(_.area.slug) shouldBe Seq("central", "north", "east", "south", "west")
  }

  it should "still arm the first-visit area picker, its only way to pick an area" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = Country.UnitedKingdom,
                                       messages = testsupport.TestMessages.forLang("en"))._1
    val body = contentAsString(uk.index("london")(req("/london/")))
    // The overlay opens for any city with areas; the flag that used to suppress
    // it existed only for the states that had a chooser screen instead.
    body should include ("CINEMA_AREAS")
    body should not include "AREA_PICKER_ENABLED"
  }

  "A flat city" should "be untouched — its index is still its listing" in {
    val pl = TestMovieController.build(
      Seq(("Testowy Film", Some(2024), filmIn(models.Helios, "Testowy Film", "tt1")))
    )._1
    val html = contentAsString(pl.index("poznan")(req("/poznan/")))
    html should include("Testowy Film")
    html should include("CINEMA_AREAS       = []")
  }
}
