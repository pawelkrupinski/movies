package clients.flicks

import models.OdeonNorwich
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.uk.FlicksClient

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/** Replays a recorded Flicks sessions fragment (Odeon Cinema Norwich, date
 *  2026-07-11) through the pure `parseDay`. Pins that the AJAX
 *  `/cinema/sessions/<slug>/<date>/` fragment parses: one film per
 *  `article.cinema-times__article`, its `/movie/<slug>` id + title + runtime +
 *  director + Flicks `content_id`, the session times (24h `data-optlabel` and
 *  the 12h fallback) as local `LocalDateTime`s, the cinema-chain booking
 *  deep-links, and the premium/format labels (IMAX…). */
class FlicksClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val date = LocalDate.of(2026, 7, 11)

  private def fixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/flicks/www.flicks.co.uk/cinema/sessions/odeon-cinema-norwich/2026-07-11.html")
    try src.mkString finally src.close()
  }

  private val slots = FlicksClient.parseDay(fixture, date)

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

  private val NorwichProgrammeUrl = s"${FlicksClient.BaseUrl}/cinema/odeon-cinema-norwich/"

  "planChunks" should "discover the venue's advertised days far beyond the fixed 7-day grid" in {
    val client = new FlicksClient(
      new ScriptedByUrl(url =>
        if (url == NorwichProgrammeUrl) programmePage
        else throw new java.io.IOException("planChunks must not fetch per-day fragments")),
      "odeon-cinema-norwich", OdeonNorwich, today = LocalDate.of(2026, 7, 19))

    val days = client.planChunks()
    days.size shouldBe 36
    days should contain("2026-12-13")        // reaches ~5 months, not today+6
    days should not contain "2026-07-27"     // gap day skipped
    days shouldBe days.sorted
  }

  it should "cap the discovered horizon at today+MaxHorizonDays" in {
    // today set early enough that the page's last dates sit past the horizon cap.
    val today  = LocalDate.of(2026, 5, 1)
    val capStr = today.plusDays(FlicksClient.MaxHorizonDays.toLong).toString  // 2026-11-27
    val client = new FlicksClient(
      new ScriptedByUrl(_ => programmePage),
      "odeon-cinema-norwich", OdeonNorwich, today = today)

    val days = client.planChunks()
    all(days) should be <= capStr          // ISO dates order lexicographically
    days should contain("2026-11-23")      // within the cap — kept
    days should not contain "2026-12-01"   // past the cap — dropped
    days should not contain "2026-12-13"
  }

  it should "THROW (failing the scrape) when the programme page can't be fetched" in {
    // No fixed-grid fallback: an index-page failure fails the whole scrape, so
    // recordCinemaScrape keeps last-known data rather than narrowing to a guess.
    val pageDown = new ScriptedByUrl(_ => throw new java.io.IOException("HTTP 500"))
    a[java.io.IOException] should be thrownBy
      new FlicksClient(pageDown, "odeon-cinema-norwich", OdeonNorwich,
        today = LocalDate.of(2026, 7, 11)).planChunks()
  }

  it should "THROW when the programme page lists no in-horizon day tab" in {
    val noTabs = new ScriptedByUrl(_ => "<html><body>no timetable here</body></html>")
    an[IllegalStateException] should be thrownBy
      new FlicksClient(noTabs, "odeon-cinema-norwich", OdeonNorwich,
        today = LocalDate.of(2026, 7, 11)).planChunks()
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
    val movies = new FlicksClient(fake, "odeon-cinema-norwich", OdeonNorwich,
      today = LocalDate.of(2026, 7, 11)).fetchChunk("2026-07-11")
    movies.map(_.movie.title) should contain("Minions & Monsters")
    movies.map(_.filmUrl).flatten should contain(s"${FlicksClient.BaseUrl}/movie/minions-3/")
  }

  // ── fetch() (trait-composed planChunks → fetchChunk → reduceChunks) ───────
  "fetch" should "assemble films for the venue via the programme + sessions endpoints" in {
    val movies = new FlicksClient(programmeListing("2026-07-11"), "odeon-cinema-norwich",
      OdeonNorwich, today = LocalDate.of(2026, 7, 11)).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(OdeonNorwich)
    movies.map(_.movie.title) should contain("Minions & Monsters")
  }

  private class ScriptedByUrl(respond: String => String) extends tools.GetOnlyHttpFetch {
    def get(url: String): String = respond(url)
  }
}
