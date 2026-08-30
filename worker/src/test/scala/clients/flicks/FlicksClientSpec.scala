package clients.flicks

import models.{BarnCinemaDartingtonArtCentre, OdeonNorwich}
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{FlicksClient, FlicksMarket}

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/** Replays a recorded Flicks sessions fragment (Odeon Cinema Norwich, date
 *  2026-07-11) through the pure `parseDay`. Pins that the AJAX
 *  `/cinema/sessions/<slug>/<date>/` fragment parses: one film per
 *  `article.cinema-times__article`, its `/movie/<slug>` id + title + runtime +
 *  director + Flicks `content_id` + `content_cast`/`content_genre` (lifted from
 *  the button's `data-eventjson`) + the `/trailer/` link, the session times (24h
 *  `data-optlabel` and the 12h fallback) as local `LocalDateTime`s, the
 *  cinema-chain booking deep-links, and the premium/format labels (IMAX…). */
class FlicksClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Uk   = FlicksMarket.UnitedKingdom
  private val date = LocalDate.of(2026, 7, 11)

  private def fixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/odeon-cinema-norwich/2026-07-11.html")
    try src.mkString finally src.close()
  }

  private val slots = FlicksClient.parseDay(fixture, date, Uk)

  "parseDay" should "read every film on the day" in {
    slots.map(_.slug).distinct.size shouldBe 15
  }

  it should "carry a film's title, slug, runtime, director and Flicks id" in {
    val minions = slots.find(_.slug == "minions-3").value
    minions.title shouldBe "Minions & Monsters"       // jsoup decodes the &amp;
    minions.runtimeMinutes.value shouldBe 90           // "90 mins"
    minions.director.value shouldBe "Pierre Coffin"
    minions.contentId.value shouldBe "25079"
  }

  it should "carry the BBFC age rating off the card's .cinema__movie-classification element" in {
    slots.find(_.slug == "minions-3").value.ageRating.value shouldBe "U"
    // A different film carries its own label, kept verbatim ("12A" stays "12A").
    slots.find(_.slug == "supergirl-woman-of-tomorrow").value.ageRating.value shouldBe "12A"
  }

  it should "lift the cast + genres from the button's data-eventjson blob" in {
    val minions = slots.find(_.slug == "minions-3").value
    // content_cast is a 9-name comma list, split + trimmed.
    minions.cast should have size 9
    minions.cast should contain("christoph waltz")
    // content_genre "kids & family" — jsoup decodes the &amp;, one entry, no comma.
    minions.genres shouldBe Seq("kids & family")
    // A multi-genre film confirms the comma split.
    slots.find(_.slug == "supergirl-woman-of-tomorrow").value.genres.size should be > 1
  }

  it should "surface the film's absolutised /trailer/ link" in {
    val minions = slots.find(_.slug == "minions-3").value
    minions.trailerUrl.value shouldBe s"${Uk.baseUrl}/trailer/minions-3/48677/"
  }

  it should "parse session times to local date-times with the chain booking link" in {
    val tenTen = slots.find(s => s.slug == "minions-3" && s.dateTime == LocalDateTime.of(2026, 7, 11, 10, 10)).value
    tenTen.booking.value should include("odeon.co.uk")
  }

  it should "surface premium/format labels on variant screenings" in {
    slots.flatMap(_.format).toSet should contain("IMAX")
  }

  // ── date discovery off the programme page's data-date day tabs ────────────
  // The horizon lever: the venue programme page renders one `data-date` day tab
  // per day it has a programme — a sparse list reaching months out — so the
  // scrape fetches exactly those days instead of a blind fixed 7-day grid.

  /** A real Flicks programme page (Odeon Cinema Norwich, captured 2026-07-19).
   *  Carries 36 `data-date` day tabs, sparse (gap days omitted) and reaching to
   *  2026-12-13 (~5 months). Read directly (NOT via FakeHttpFetch's URL mapping)
   *  and fed to the discovery tests below. Source:
   *  https://www.flicks.co.uk/cinema/odeon-cinema-norwich/ */
  private def programmePage: String = {
    val src = Source.fromFile("test/resources/fixtures/flicks/odeon-cinema-norwich-programme.html")
    try src.mkString finally src.close()
  }

  "parseProgrammeDates" should "read every day tab, sparse and months out, deduped + sorted" in {
    val dates = FlicksClient.parseProgrammeDates(programmePage)
    dates.head shouldBe LocalDate.of(2026, 7, 19)
    dates.last shouldBe LocalDate.of(2026, 12, 13)   // ~5 months out — far past a 7-day grid
    dates.size shouldBe 36
    dates shouldBe dates.sorted
    // Gap days between the near-term tabs are absent (they return empty fragments).
    dates should contain noneOf (LocalDate.of(2026, 7, 27), LocalDate.of(2026, 7, 28))
  }

  it should "return empty when no day tab is present" in {
    FlicksClient.parseProgrammeDates("<html><body>no timetable here</body></html>") shouldBe empty
  }

  private val NorwichProgrammeUrl = s"${Uk.baseUrl}/cinema/odeon-cinema-norwich/"

  "planChunks" should "discover the venue's advertised days far beyond the fixed 7-day grid" in {
    val client = new FlicksClient(
      new ScriptedByUrl(url =>
        if (url == NorwichProgrammeUrl) programmePage
        else throw new java.io.IOException("planChunks must not fetch per-day fragments")),
      "odeon-cinema-norwich", OdeonNorwich, Uk, today = Some(LocalDate.of(2026, 7, 19)))

    val days = client.planChunks()
    days.size shouldBe 36
    days should contain("2026-12-13")        // reaches ~5 months, not today+6
    days should not contain "2026-07-27"     // gap day skipped
    days shouldBe days.sorted
  }

  // The bound is a SANITY valve, not a coverage cap — see `ScrapeHorizon`. Every day the
  // venue advertises is planned, however far out, because a day we do not fetch is a day
  // whose films `MovieCache`'s scrape-prune then deletes as "no longer screening". The
  // old 210-day cap did exactly that to the UK's advance-sale programme.
  it should "plan every advertised day, however far out, and bound only the absurd" in {
    val today  = LocalDate.of(2026, 5, 1)
    val capStr = today.plusDays(FlicksClient.MaxHorizonDays.toLong).toString
    val client = new FlicksClient(
      new ScriptedByUrl(_ => programmePage),
      "odeon-cinema-norwich", OdeonNorwich, Uk, today = Some(today))

    val days = client.planChunks()
    all(days) should be <= capStr          // ISO dates order lexicographically
    days should contain("2026-11-23")
    days should contain("2026-12-01")      // advance-sale tail — KEPT now
    days should contain("2026-12-13")
  }

  it should "THROW (failing the scrape) when the programme page can't be fetched" in {
    // No fixed-grid fallback: an index-page failure fails the whole scrape, so
    // recordCinemaScrape keeps last-known data rather than narrowing to a guess.
    val pageDown = new ScriptedByUrl(_ => throw new java.io.IOException("HTTP 500"))
    a[java.io.IOException] should be thrownBy
      new FlicksClient(pageDown, "odeon-cinema-norwich", OdeonNorwich, Uk,
        today = Some(LocalDate.of(2026, 7, 11))).planChunks()
  }

  it should "THROW when the programme page carries no timetable block at all" in {
    // No timetable container => not the page we think we're parsing (markup drift,
    // an error page, a redirect). That IS a failure: throwing keeps last-known data.
    val noTimetable = new ScriptedByUrl(_ => "<html><body>no timetable here</body></html>")
    an[IllegalStateException] should be thrownBy
      new FlicksClient(noTimetable, "odeon-cinema-norwich", OdeonNorwich, Uk,
        today = Some(LocalDate.of(2026, 7, 11))).planChunks()
  }

  /** A real Flicks programme page for a venue with NOTHING on (Woolton Picture
   *  House, captured 2026-07-27 via the residential proxy). Carries the
   *  `timetable timetable--cinema` container like any other venue page, but zero
   *  `timetable__day` tabs inside it. Source:
   *  https://www.flicks.co.uk/cinema/woolton-picture-house/ */
  private def emptyProgrammePage: String = {
    val src = Source.fromFile("test/resources/fixtures/flicks/woolton-picture-house-programme-empty.html")
    try src.mkString finally src.close()
  }

  // A venue that simply has no screenings on is EXPECTED DATA, not an outage: the
  // page renders fine, the timetable block is there, it just holds no days. Five
  // UK venues sat permanently red on /uptime for this (2026-07-26) because it threw.
  // Germany already draws the line here — WebediaShowtimesClient throws only when
  // the dates ATTRIBUTE is missing, and returns empty when it's present-but-empty.
  // Empty is safe: MovieCache.recordCinemaScrape bails on an empty result, so the
  // venue keeps its last-known listing either way.
  it should "return empty (not throw) when the timetable block is present but holds no day tabs" in {
    val emptyVenue = new ScriptedByUrl(_ => emptyProgrammePage)
    new FlicksClient(emptyVenue, "woolton-picture-house", OdeonNorwich, Uk,
      today = Some(LocalDate.of(2026, 7, 27))).planChunks() shouldBe empty
  }

  // ── chunked scrape: one chunk per day, one AJAX call each ─────────────────
  // fetchChunk hits only the per-day sessions URL (the recorded 2026-07-11
  // fragment); fetch() also needs a programme page, so it runs over a scripted
  // page that lists exactly that one recorded day, with sessions delegated to
  // the FakeHttpFetch corpus.
  private val fake = new FakeHttpFetch("flicks")

  private def programmeListing(days: String*) = new ScriptedByUrl(url =>
    if (url == NorwichProgrammeUrl)
      days.map(d => s"""<div class="timetable__day" data-date="$d"></div>""").mkString("\n")
    else fake.get(url))

  "fetchChunk" should "fetch + parse a single day's sessions fragment into films" in {
    val movies = new FlicksClient(fake, "odeon-cinema-norwich", OdeonNorwich, Uk,
      today = Some(LocalDate.of(2026, 7, 11))).fetchChunk("2026-07-11")
    movies.map(_.movie.title) should contain("Minions & Monsters")
    movies.map(_.filmUrl).flatten should contain(s"${Uk.baseUrl}/movie/minions-3/")
  }

  // ── fetch() (trait-composed planChunks → fetchChunk → reduceChunks) ───────
  "fetch" should "assemble films for the venue via the programme + sessions endpoints" in {
    val movies = new FlicksClient(programmeListing("2026-07-11"), "odeon-cinema-norwich",
      OdeonNorwich, Uk, today = Some(LocalDate.of(2026, 7, 11))).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(OdeonNorwich)
    movies.map(_.movie.title) should contain("Minions & Monsters")
  }

  // ── sessions that carry no booking link ──────────────────────────────────
  // Flicks renders a session button as an `<a>` only when the venue has a
  // bookable deep-link behind it. Venues without online booking wired in (small
  // independents, arts centres) get the SAME card with the button as a plain
  // `<span class="times-calendar-times__button">` — the time is still there, it
  // just isn't clickable. Keying the session on the `<a>` dropped every one of
  // those showtimes, so the venue scraped to zero films and sat white on
  // /uptime while Flicks was plainly listing its programme (2026-07-28: Barn
  // Cinema Dartington, Broadway Cinema Villa Marina, Watersmeet).

  /** A real Flicks sessions fragment for a venue whose showtimes are NOT
   *  bookable (Barn Cinema, Dartington Art Centre, captured 2026-07-28). Two
   *  films, six `<span class="times-calendar-times__button">` buttons and zero
   *  `<a>` ones. Source:
   *  https://www.flicks.co.uk/cinema/sessions/dartington-art-centre-totnes/2026-07-28/ */
  private def unbookableFixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/dartington-art-centre-totnes/2026-07-28.html")
    try src.mkString finally src.close()
  }

  private val dartingtonDate  = LocalDate.of(2026, 7, 28)
  private lazy val unbookable = FlicksClient.parseDay(unbookableFixture, dartingtonDate, Uk)

  "parseDay" should "read sessions rendered as a <span> button (no booking link)" in {
    unbookable.map(_.slug).distinct should contain theSameElementsAs Seq("the-odyssey-2026", "minions-3")
  }

  it should "take an unbookable session's time from the visible 12h text" in {
    // No `data-optlabel` on a span button, so the "4:00 pm" / "7:30 pm" text is
    // the only time source.
    unbookable.filter(_.slug == "the-odyssey-2026").map(_.dateTime).distinct should contain theSameElementsAs
      Seq(LocalDateTime.of(2026, 7, 28, 16, 0), LocalDateTime.of(2026, 7, 28, 19, 30))
    unbookable.filter(_.slug == "minions-3").map(_.dateTime).distinct shouldBe
      Seq(LocalDateTime.of(2026, 7, 28, 14, 0))
  }

  it should "leave the booking link empty rather than inventing one" in {
    unbookable.flatMap(_.booking) shouldBe empty
  }

  it should "still carry the film metadata off an unbookable card" in {
    val odyssey = unbookable.find(_.slug == "the-odyssey-2026").value
    odyssey.title shouldBe "The Odyssey"
    odyssey.runtimeMinutes.value shouldBe 172
    odyssey.director.value shouldBe "Christopher Nolan"
    odyssey.ageRating.value shouldBe "15"
  }

  it should "collapse the duplicated desktop/mobile buttons into one showtime each" in {
    // The card renders each session twice (a desktop-only and a mobile block);
    // the (time, booking) dedup in moviesFor must leave one showtime per screening.
    val movies = new FlicksClient(fake, "dartington-art-centre-totnes",
      BarnCinemaDartingtonArtCentre, Uk, today = Some(dartingtonDate)).fetchChunk("2026-07-28")
    movies.map(_.movie.title) should contain theSameElementsAs Seq("The Odyssey", "Minions & Monsters")
    movies.find(_.movie.title == "The Odyssey").value.showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 7, 28, 16, 0), LocalDateTime.of(2026, 7, 28, 19, 30))
    movies.find(_.movie.title == "Minions & Monsters").value.showtimes should have size 1
  }

  private class ScriptedByUrl(respond: String => String) extends tools.GetOnlyHttpFetch {
    def get(url: String): String = respond(url)
  }
}
