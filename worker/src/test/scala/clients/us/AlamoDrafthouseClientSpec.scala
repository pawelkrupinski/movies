package clients.us

import clients.tools.FakeHttpFetch
import models.{CinemaMovie, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailFetchOutcome, ScrapeHorizon}
import services.cinemas.us.{AlamoDrafthouseClient, AlamoDrafthouseParser}
import services.identity.{CatalogueId, CatalogueLinkReader, Listing, TmdbIdentityLookups, VenueDetails}
import services.movies.TitleNormalizer
import tools.{HttpFetch, RoutingHttpFetch}

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays Alamo Drafthouse Lakeline (venue slug `lakeline`, Austin) through
 * [[AlamoDrafthouseClient]] entirely from disk — the real response recorded
 * 2026-08-30 from `drafthouse.com/s/mother/v2/schedule/venue/lakeline`, which is
 * the venue's WHOLE scrape: one request, 300 sessions, 44 programme days.
 *
 * Recorded by `clients.tools.RecordUsChains`. `today` is pinned to the capture
 * date because the client's far-date sanity bound is relative to it.
 */
class AlamoDrafthouseClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Today    = LocalDate.of(2026, 8, 30)
  private val Lakeline = UsRoster.byDisplayName("Alamo Drafthouse Lakeline")

  private def clientOn(fixtures: HttpFetch, venueSlug: String = "lakeline") =
    new AlamoDrafthouseClient(fixtures, fixtures, venueSlug, Lakeline, today = Today)

  private val films: Seq[CinemaMovie] = clientOn(new FakeHttpFetch("alamo-drafthouse")).fetch()

  private def film(title: String): CinemaMovie = films.find(_.movie.title == title).value

  "fetch" should "join the venue's sessions against its presentations into one row per film" in {
    films.size shouldBe 48
    films.map(_.cinema).toSet shouldBe Set(Lakeline)
    films.map(_.movie.title) shouldBe films.map(_.movie.title).sorted
    all(films.map(_.showtimes.size)) should be > 0
    films.flatMap(_.showtimes).size shouldBe 300
  }

  it should "carry the show's title, poster and film page" in {
    val avengers = film("Avengers: Doomsday")
    avengers.posterUrl.value should startWith(
      "https://img-assets.drafthouse.com/images/shows/avengers-doomsday/")
    avengers.filmUrl.value shouldBe "https://drafthouse.com/show/avengers-doomsday"
    avengers.externalIds shouldBe Map("alamo" -> "avengers-doomsday")
    avengers.showtimes.size shouldBe 38
  }

  it should "parse each session to the venue's local date-time, screen and booking link" in {
    // The venue's own wall-clock (`showTimeClt`), stored as-is — the sibling
    // `showTimeUtc` would need the zone applied back to mean the same thing.
    val first = film("Avengers: Doomsday").showtimes.head
    first.dateTime shouldBe LocalDateTime.of(2026, 12, 17, 13, 0)
    first.room.value shouldBe "4"
    // Derived from the session's own cinemaId + sessionId — the payload carries
    // no booking-URL field at all.
    first.bookingUrl.value shouldBe "https://drafthouse.com/ticketing/0007/226878"
  }

  it should "read the MPAA certificate off the show" in {
    film("Avengers: Doomsday").ageRating.value shouldBe "PG-13"
    film("Akira (Dubbed) in 4K").ageRating.value shouldBe "R"
    // Nothing but the MPAA vocabulary reaches the age-rating chip. The live
    // corpus also held "(Standard)", "Focus" and "Ages" in this field — fragments
    // of an age-POLICY name that had leaked into it.
    films.flatMap(_.ageRating).distinct.foreach { rating =>
      withClue(s"'$rating' is not an MPAA certificate: ") {
        Set("G", "PG", "PG-13", "R", "NC-17", "NR") should contain(rating)
      }
    }
  }

  // ── a presentation is not a film ──────────────────────────────────────────

  "a film screened several ways" should "be ONE row with its variants' showtimes unioned" in {
    // Lakeline's payload carries 51 presentations over 48 shows: "Dune: Part
    // Three" appears both as its regular run and as an "advance screening …
    // insider screening", with the SAME title. Keying rows by presentation would
    // put two identically-titled rows on one cinema; keying by `show.slug` — the
    // chain's own film-level id — is what collapses them.
    val dune = film("Dune: Part Three")
    dune.filmUrl.value shouldBe "https://drafthouse.com/show/dune-part-three"
    dune.showtimes.size shouldBe 8
    films.count(_.movie.title == "Dune: Part Three") shouldBe 1
    films.map(_.movie.title).distinct.size shouldBe films.size
  }

  // ── the dotted format/attribute vocabulary → format tokens ────────────────

  "format tokens" should "read the dimension off the format slug" in {
    film("Akira (Dubbed) in 4K").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 9, 4, 21, 15)).value
      .format shouldBe List("2D")
  }

  it should "combine the premium format with the session's audio attribute" in {
    // formatSlug `hdr` + attributes Digital/Atmos — dimension first, then premium.
    film("Dune: Part Three").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 12, 15, 18, 0)).value
      .format shouldBe List("2D", "HDR", "ATMOS")
    // An Infinity Vision screen carries no digital tag at all, so there is no
    // dimension token to lead with — the premium pair is the whole story.
    film("Avengers: Doomsday").showtimes.head.format shouldBe List("HDR", "INFINITY", "ATMOS")
  }

  it should "treat an open-caption screening as a flat digital one that you read" in {
    film("Spider-Man: Brand New Day").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 8, 31, 11, 0)).value
      .format shouldBe List("2D", "SUB")
  }

  it should "keep the vendor's own title for a format slug the map doesn't know yet" in {
    // A NEW projection format must not vanish from the row — it is the
    // screening's only distinguishing mark. Attributes get no such treatment:
    // most of them are audience policy, not format.
    AlamoDrafthouseParser.formatTokens(
      Seq("2d-digital", "holo-vision"), Map("holo-vision" -> "Holo Vision")
    ) shouldBe List("2D", "HOLO VISION")
    AlamoDrafthouseParser.formatTokens(Seq("2d-digital", "BD", "KF"), Map.empty) shouldBe List("2D")
  }

  it should "map the celluloid gauges from either namespace" in {
    // Alamo spells these BOTH as a format slug and as a session attribute
    // ("35mm" / "35MM"); either alone means the same thing.
    AlamoDrafthouseParser.formatTokens(Seq("35mm"), Map.empty) shouldBe List("35MM")
    AlamoDrafthouseParser.formatTokens(Seq("2d-digital", "70MM"), Map.empty) shouldBe List("2D", "70MM")
    AlamoDrafthouseParser.formatTokens(Seq("3d-digital"), Map.empty) shouldBe List("3D")
  }

  // ── the horizon: ONE request, and the whole advertised programme ──────────

  "the venue's whole programme" should "arrive in a single request" in {
    AlamoDrafthouseClient.scheduleUrl("lakeline") shouldBe
      "https://drafthouse.com/s/mother/v2/schedule/venue/lakeline"
  }

  it should "reach far past a one-week grid, gap days simply absent" in {
    // THE CHECK THAT LETS THIS CHAIN GO PRIMARY. Measured against the same venue
    // on flicks.us the same day: flicks advertised 38 days ending 2026-12-22, this
    // feed 44 days ending 2026-12-22 — the same furthest date and six more
    // populated days, so switching primary cannot narrow the listing and let
    // scrape-prune delete the advance-sale tail (see ScrapeHorizon).
    val days = films.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    days.size shouldBe 44
    days.head shouldBe Today
    days.last shouldBe LocalDate.of(2026, 12, 22)
    all(days.map(_.toString)) should be <= Today.plusDays(ScrapeHorizon.MaxDays.toLong).toString
  }

  it should "surface the venue's own public page for /uptime" in {
    clientOn(new FakeHttpFetch("alamo-drafthouse")).sourceUrl.value shouldBe
      "https://drafthouse.com/theater/lakeline"
  }

  // ── an idle venue is DATA, not an outage ──────────────────────────────────

  "a venue with nothing on" should "return empty rather than throw" in {
    // No Alamo venue was dark on the capture date (all 40 were OPEN and every one
    // had sessions), so this replays the REAL Lakeline envelope with its
    // programme arrays emptied — market roster, format vocabulary and age
    // policies all still present, exactly as a quiet venue would serve them.
    //
    // Throwing here is what left five UK venues permanently red on /uptime: an
    // empty programme must record as a SUCCESSFUL scrape of an empty repertoire,
    // so the venue keeps its last-known listing and its uptime row stays green.
    clientOn(new FakeHttpFetch("alamo-drafthouse-idle")).fetch() shouldBe empty
  }

  // ── a failed fetch must FAIL, so last-known data survives ─────────────────

  "a fetch failure" should "propagate so the scrape records as failed" in {
    // The opposite of the case above, and the distinction the cache depends on:
    // a throw is recorded as a failed scrape and the venue keeps its previous
    // showtimes, whereas swallowing it into an empty listing would let
    // scrape-prune delete the venue's whole programme on an upstream blip.
    // An unknown venue slug really does 404 on this API (verified 2026-08-30);
    // here it is an unrecorded fixture, which `FakeHttpFetch` raises the same way.
    a[java.io.FileNotFoundException] should be thrownBy
      clientOn(new FakeHttpFetch("alamo-drafthouse"), venueSlug = "no-such-venue").fetch()
  }

  // ── the show's own detail: what the venue payload leaves out ──────────────
  //
  // Replays `drafthouse.com/s/mother/v2/schedule/presentation/<show slug>`,
  // recorded 2026-10-06 for every show of the Lakeline fixture plus Last Shot
  // (`clients.tools.RecordUsChains` records them beside the venue).

  private val ShowPage = "https://drafthouse.com/show/"

  "a show's detail" should "read the runtime and release year the show page displays" in {
    // Andy Palmer's 2026 "Last Shot" (tt15426712), not the 2020 film of the same
    // name: the year and runtime are what let identity resolution tell them apart.
    val lastShot = clientOn(new FakeHttpFetch("alamo-drafthouse")).fetchFilmDetail(ShowPage + "last-shot").value
    lastShot.runtimeMinutes.value shouldBe 105
    lastShot.releaseYear.value shouldBe 2026
    lastShot.synopsis.value should startWith("12-year-old basketball prodigy Caden Issacs navigates")
    lastShot.synopsis.value should not include "<p>"
    lastShot.posterUrl.value should startWith("https://img-assets.drafthouse.com/images/shows/last-shot/poster-last_shot.jpg")
    lastShot.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=_TGoETRHtfA"
    // Last Shot credits no one and carries no certificate yet: empty, not invented.
    lastShot.director shouldBe empty
    lastShot.cast shouldBe empty
    lastShot.ageRating shouldBe None
  }

  it should "credit the director and cast where the show names them" in {
    val spyKids = clientOn(new FakeHttpFetch("alamo-drafthouse")).fetchFilmDetail(ShowPage + "spy-kids").value
    spyKids.director shouldBe Seq("Robert Rodriguez")
    spyKids.cast should contain ("Antonio Banderas")
    spyKids.releaseYear.value shouldBe 2001
    spyKids.runtimeMinutes.value shouldBe 88
    spyKids.ageRating.value shouldBe "PG"
  }

  it should "give every listing of the recorded venue the show's facts" in {
    val client  = clientOn(new FakeHttpFetch("alamo-drafthouse"))
    val details = films.map(f => client.fetchFilmDetail(f.filmUrl.value).value)
    details.size shouldBe 48
    details.count(_.releaseYear.isDefined) shouldBe 48
    details.count(_.runtimeMinutes.isDefined) shouldBe 46
    details.count(_.director.nonEmpty) shouldBe 15
    details.count(_.synopsis.isDefined) shouldBe 48
  }

  it should "let a show the API does not know escape as gone, not retry it every tick" in {
    val nothing = new RoutingHttpFetch(Nil, unroutedIsNotFound = true)
    clientOn(nothing).fetchDetail(ShowPage + "no-such-show") shouldBe DetailFetchOutcome.Gone(404)
  }

  it should "be the VENUE's stated facts, read by identity through its own detail page" in {
    val client  = clientOn(new FakeHttpFetch("alamo-drafthouse"))
    val listing = Listing.of(Lakeline,
      CinemaMovie(models.Movie(title = "Last Shot"), Lakeline, None, Some(ShowPage + "last-shot"), None, Nil, Nil, Nil),
      TitleNormalizer.forCountry(models.Country.UnitedStates))
    val facts = new VenueDetails(Seq(client), TmdbIdentityLookups.NoGaps).detail(listing)
    facts.toOption.flatten.map(f => (f.year, f.runtime)) shouldBe Some((Some(2026), Some(105)))
  }

  "a show's IMDb id" should "be the venue's own catalogue link to the film" in {
    val reader = new CatalogueLinkReader(Seq(AlamoDrafthouseClient.CatalogueLinkPages -> new FakeHttpFetch("alamo-drafthouse")))
    reader.links(ShowPage + "spy-kids") shouldBe Seq(CatalogueId("imdb", "tt0227538"))
    reader.links(ShowPage + "last-shot") shouldBe Nil
  }

  // ── the scraper's contract with the rest of the pipeline ──────────────────

  "the scraper" should "declare its host and count as a chain" in {
    val client = clientOn(new FakeHttpFetch("alamo-drafthouse"))
    // Derived from the same base URL the client fetches with, so uptime
    // suppression and the scrape can't drift.
    client.scrapeHosts shouldBe Set("drafthouse.com")
    // A national chain fed by one central service: the per-cinema Filmweb
    // fallback must not shadow it.
    client.chain shouldBe true
  }
}
