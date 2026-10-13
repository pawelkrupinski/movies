package controllers

import models.{City, Cinema, Country, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._


/**
 * The page level, after the state stopped being a place. A US town or cluster
 * page IS the city — `/los-angeles/` is its listing, scoped to its own venues —
 * and the state it sits in has no page at all: `/texas/` is as unknown as
 * `/atlantis/`. What survives at the level BELOW is `City.areas`: London's five
 * compass areas, which the cinema filter groups by and the first-visit picker
 * offers. No US page is big enough to need any since the metros became towns.
 *
 * The ways this can go wrong are what is asserted: a page leaking films from the
 * rest of the state, a state answering 200 with an 18 MB listing instead of 404,
 * and London — split, and deliberately untouched by any of this — losing its
 * areas or its one-page listing.
 */
class AreaRoutingSpec extends AnyFlatSpec with Matchers {

  private def place(slug: String): City = City.all.find(_.slug == slug).getOrElse(fail(s"no city '$slug'"))

  private val losAngeles   = place("los-angeles")
  private val sanFrancisco = place("san-francisco")

  private val laCinema: Cinema = losAngeles.cinemas.head
  private val sfCinema: Cinema = sanFrancisco.cinemas.head

  private val LaFilm = "Angeleno Feature"
  private val SfFilm = "Bay Area Feature"

  private def filmIn(cinema: Cinema, title: String, imdb: String): MovieRecord = {
    val now = TestMovieController.now
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
   *  venue — two different California pages, and so two different cities. */
  private def usController(): MovieController = TestMovieController.build(
    Seq(
      (LaFilm, Some(2024), filmIn(laCinema, LaFilm, "tt101")),
      (SfFilm, Some(2024), filmIn(sfCinema, SfFilm, "tt102")),
    ),
    // A US host renders English — asserted on below — for free from
    // `servingCountry` now (`MovieController` derives its `Messages` from it).
    servingCountry = Country.UnitedStates,
  )._1

  private def req(path: String) = EdgeRequest(path, host = "showtimes.cc")

  // The unified `/` picker (`landing.scala.html`) and the apps (`/api/catalog`) read
  // `models.Catalog.json` rather than a server-rendered `<optgroup>` tree, so these
  // read the same field the picker's JS drill-down groups by.
  private def catalogRegion(slug: String): Option[String] =
    s""""slug":"$slug"[^}]*"region":"([^"]+)"""".r.findFirstMatchIn(models.Catalog.json).map(_.group(1))
  private def catalogSubregion(slug: String): Option[String] =
    s""""slug":"$slug"[^}]*"subregion":"([^"]+)"""".r.findFirstMatchIn(models.Catalog.json).map(_.group(1))

  // ── The metro's own listing ─────────────────────────────────────────────────

  "/los-angeles/" should "render the repertoire scoped to that page only" in {
    val res = usController().index("los-angeles")(req("/los-angeles/"))
    status(res) shouldBe OK
    val html = contentAsString(res)
    html should include(LaFilm)
    // The San Francisco film is in the same STATE — the scoping is by page, and
    // this is the assertion that fails if a page ever swallows its state again.
    html should not include SfFilm
  }

  it should "offer only that metro's cinemas to the filter panel" in {
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include(laCinema.displayName)
    html should not include sfCinema.displayName
    // Los Angeles is the city alone now — 40 venues, one flat list.
    html should include("CINEMA_AREAS       = []")
  }

  it should "title, describe and canonicalise itself by the city" in {
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include("<title>Cinema listings in Los Angeles")
    html should include("""<link rel="canonical" href="https://showtimes.cc/us/los-angeles/"""")
  }

  // The metro is still what gets remembered — but the CLIENT writes the cookie
  // now, not the response. The listing has to reach Cloudflare with no
  // `Set-Cookie` or the edge bypasses the cache for it (measured: DYNAMIC ->
  // BYPASS with that header the only thing left on the response), so `shared.js`
  // writes `city=` on load from `CURRENT_CITY`. What this page owes is therefore
  // the right slug in that constant; that the cookie is then written is asserted
  // in a real browser by `PageJsBehaviourSpec`.
  it should "remember the page as the city, so the bare / bounces back to it" in {
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include ("CURRENT_CITY")
    html should include ("\"los-angeles\";")
  }

  it should "name its country for the client, and not carry every country's city catalog" in {
    // `shared.js` keys a visitor's per-country state by this; the whole catalog it
    // was once looked up in is the `/` picker's to embed, not every listing's.
    val html = contentAsString(usController().index("los-angeles")(req("/los-angeles/")))
    html should include ("CURRENT_COUNTRY     = \"us\"")
    html should not include "KINOWO_CATALOG"
    html should not include "\"slug\":\"san-francisco\""
  }

  it should "carry the other pages' state grouping in the catalog the city picker drills through" in {
    val html = models.Catalog.json
    catalogRegion("san-francisco") shouldBe Some("California")
    // Several pages are called "Springfield" — a flat list offers them as
    // identical rows, which is why the picker groups by state at all; the catalog
    // is how it (and a visitor) can tell them apart.
    val springfieldRegions = """"name":"Springfield"[^}]*"region":"([^"]+)"""".r
      .findAllMatchIn(html).map(_.group(1)).toList
    springfieldRegions should contain allOf ("Illinois", "Missouri", "Massachusetts")
    // A page reaching across a state line is under its anchor town's state.
    catalogRegion("woodbury") shouldBe Some("Minnesota")
  }

  "The UK's catalog" should "group its counties by nation, with a further subregion where a county holds more than one place" in {
    // The catalog reads `Country.cityGroups` — the same grouping the landing
    // picker's tree does — through `models.Catalog`'s `region`/`subregion`
    // fields, so a nation groups its counties and a county that really holds
    // several places (unlike one that collapsed onto its single place —
    // `CityGroup.soleCity`) earns a subregion of its own.
    catalogRegion("glasgow") shouldBe Some("Scotland")
    catalogRegion("cardiff") shouldBe Some("Wales")
    catalogSubregion("cardiff") shouldBe Some("Glamorgan")
    // Shetland collapsed onto its one place, so Lerwick reads correctly through
    // `region` alone — no subregion heading for a visitor to open for nothing.
    catalogRegion("lerwick") shouldBe Some("Scotland")
    catalogSubregion("lerwick") shouldBe None
  }

  it should "group Germany's regions by Bundesland, the same way" in {
    // Germany joined the grouped countries with the UK; the catalog carries the
    // same `Country.cityGroups`, so 158 regions are grouped rather than run
    // together in one A-to-Z.
    catalogRegion("muenchen") shouldBe Some("Bayern")
    catalogRegion("koeln") shouldBe Some("Nordrhein-Westfalen")
    // A Land is a heading a visitor drills through, never a place of its own.
    models.Catalog.json should not include """"slug":"bayern""""
  }

  "A US page" should "leave its cinema list flat rather than wrapping it in one group" in {
    // No US page has districts, and a single collapsible section around the whole
    // list would be chrome with no choice in it.
    val res = usController().index("san-diego")(req("/san-diego/"))
    status(res) shouldBe OK
    contentAsString(res) should include ("CINEMA_AREAS       = []")
  }

  "A one-cinema town" should "still serve its own listing — its venue list IS the page" in {
    // Bishop, California: one cinema, 59 km from the next — too far to share a page.
    val res = usController().index("bishop")(req("/bishop/"))
    status(res) shouldBe OK
    contentAsString(res) should include ("CINEMA_AREAS       = []")
  }

  // ── The state is not a place ────────────────────────────────────────────────

  "A US state" should "404, never answer with the whole state's listing" in {
    val us = usController()
    Seq("texas", "new-jersey").foreach { slug =>
      withClue(s"$slug: ") {
        val res = us.index(slug)(req(s"/$slug/"))
        status(res) shouldBe NOT_FOUND
        // The tell of the bug this replaced: a 200 carrying every film in the state.
        contentAsString(res) should not include SfFilm
      }
    }
    // A state's name answers only where a TOWN has it: /california/ is
    // California, Maryland, listing that town's cinemas — not the state's.
    val california = us.index("california")(req("/california/"))
    status(california) shouldBe OK
    contentAsString(california) should not include SfFilm
  }

  "An unknown or foreign city" should "404 like every other city-scoped route" in {
    val us = usController()
    status(us.index("atlantis")(req("/atlantis/"))) shouldBe NOT_FOUND
    // Poznań is real, but not on a US host.
    status(us.index("poznan")(req("/poznan/")))     shouldBe NOT_FOUND
  }

  "/los-angeles/movies with no filter axis" should "serve the page's listing, not a browse page" in {
    val res = usController().browse("los-angeles", None, None, None, None)(req("/los-angeles/movies"))
    status(res) shouldBe OK
    contentAsString(res) should include(LaFilm)
    contentAsString(res) should not include SfFilm
  }

  // ── The mobile API ─────────────────────────────────────────────────────────

  /** Load-bearing: the iOS and Android apps read the `/:city/api/…` endpoints and
   *  have no level below the city. They get the page, whole. */
  "The mobile API" should "serve the page's own universe" in {
    val us = usController()
    val repertoire = contentAsString(us.apiRepertoire("los-angeles")(req("/los-angeles/api/repertoire")))
    repertoire should include(LaFilm)
    repertoire should not include SfFilm

    status(us.apiDetails("los-angeles")(req("/los-angeles/api/details"))) shouldBe OK

    val cinemas = contentAsString(us.apiCinemas("los-angeles")(req("/los-angeles/api/cinemas")))
    cinemas should include(laCinema.displayName)
    cinemas should not include sfCinema.displayName
  }

  // ── What must NOT change ────────────────────────────────────────────────────

  "London" should "keep serving its own listing, split into its five compass areas" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = Country.UnitedKingdom)._1
    val res = uk.index("london")(req("/london/"))
    status(res) shouldBe OK
    val body = contentAsString(res)
    body should include ("\"name\":\"Central\"")
    body should include ("\"name\":\"South\"")
    models.London.areas.map(_.area.slug) shouldBe Seq("central", "north", "east", "south", "west")
  }

  it should "still arm the first-visit area picker, its only way to pick an area" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = Country.UnitedKingdom)._1
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
