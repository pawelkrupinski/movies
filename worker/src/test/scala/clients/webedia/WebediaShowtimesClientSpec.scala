package clients.webedia

import models.GermanCinema
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{WebediaMarket, WebediaShowtimesClient}

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/** Replays a recorded Webedia website-JSON capture (Germany, Filmstarts theater
 *  A0263, date 2026-07-11) through the pure `parsePage`. Pins that the surviving
 *  `www.filmstarts.de/_/showtimes/theater-<id>/d-<date>/p-<n>/` endpoint parses:
 *  one film per `results[]`, title + `originalTitle` (Moana → a TMDB hint),
 *  production year, "1 Std. 56 Min."-style runtime, director off `credits`, the
 *  German FSK certificate off `releases[].certificate.code`, the `startsAt` local
 *  `LocalDateTime`s, a `3D` format token read from `tags`, a `DF` version token
 *  read from the bucket key, and cleaned relay
 *  booking links. */
class WebediaShowtimesClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def fixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/webedia-de/www.filmstarts.de/_/showtimes/theater-A0263/d-2026-07-11/p-1.json")
    try src.mkString finally src.close()
  }

  /** A real Filmstarts venue page (theater A0263, captured 2026-07-19). Carries
   *  `data-showtimes-dates` = the 24 days with showtimes across Filmstarts' fixed
   *  ~28-day window (2026-07-19 → 2026-08-15), gap days omitted. Read directly (not
   *  via FakeHttpFetch's URL mapping) so the grid-fallback tests below still find
   *  no venue page under `webedia-de`. Source URL:
   *  https://www.filmstarts.de/kinoprogramm/kino/A0263/ */
  private def venuePage: String = {
    val src = Source.fromFile("test/resources/fixtures/webedia-de/theater-A0263-venue-page.html")
    try src.mkString finally src.close()
  }

  private val page = WebediaShowtimesClient.parsePage(fixture, WebediaMarket.Germany)

  "parsePage" should "read every film and the page count" in {
    page.totalPages shouldBe 1
    page.films.map(_.internalId).distinct.size shouldBe 13
  }

  it should "carry title, originalTitle, year, runtime and director" in {
    val vaiana = page.films.find(_.title == "Vaiana").value
    vaiana.originalTitle.value shouldBe "Moana"      // international title → TMDB hint
    vaiana.year.value shouldBe 2026
    vaiana.runtimeMinutes.value shouldBe 116         // "1 Std. 56 Min."
    vaiana.director should contain("Thomas Kail")
  }

  it should "carry the German FSK certificate verbatim off the first release that has one" in {
    // Single-release films: the code is right there.
    page.films.find(_.title == "Vaiana").value.ageRating.value shouldBe "FSK 6"
    page.films.find(_.title == "Supergirl").value.ageRating.value shouldBe "FSK 12"
    // "Der Super Mario Galaxy Film" ships releases=[{no certificate}, {FSK 6}] —
    // the first release lacks a certificate, so we take the next one that has it.
    page.films.find(_.title == "Der Super Mario Galaxy Film").value.ageRating.value shouldBe "FSK 6"
    // Every film in this capture is FSK-classified; the "FSK" prefix is kept.
    all(page.films.flatMap(_.ageRating)) should startWith("FSK ")
  }

  it should "leave ageRating None when no release carries a certificate" in {
    val noCert = """{"results":[{"movie":{"internalId":42,"title":"Ungeprüft","releases":[{"name":"kino"}]}}]}"""
    WebediaShowtimesClient.parsePage(noCert, WebediaMarket.Germany).films.head.ageRating shouldBe None
  }

  it should "flatten version buckets into local-time showtimes with format tokens" in {
    val vaiana = page.films.find(_.title == "Vaiana").value

    // The 13:00 dubbed 3D screening — 3D is read from the `Format.Projection.3d`
    // tag, `DF` from the `dubbed` bucket the screening sits in.
    val threeD = vaiana.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 11, 13, 0)).value
    threeD.format shouldBe List("3D", "DF")

    // A plain digital screening carries no SCREEN format — `Format.Projection.Digital`
    // is the baseline every German slot shares — but is still named as the German
    // dubbed version, which is what tells it apart at a cinema that also runs OmU.
    val plain = vaiana.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 11, 11, 40)).value
    plain.format shouldBe List("DF")
  }

  it should "clean the trailing '; SSR' render-marker off booking links" in {
    val bookings = page.films.flatMap(_.showtimes).flatMap(_.bookingUrl)
    bookings should not be empty
    bookings.exists(_.startsWith("https://relay.mvtx.us/")) shouldBe true
    all(bookings) should not include " "        // no whitespace survived
    all(bookings) should not endWith ";"
  }

  // `synopsisFull` arrives as HTML. Left raw it reached the detail page as
  // visible `<p class="bo-p">` text, because SynopsisMarkdown escapes prose.
  it should "flatten the HTML synopsis to prose" in {
    val synopsis = page.films.find(_.title == "Vaiana").value.synopsis.value
    synopsis should startWith("Vaiana (Catherine Laga'aia) will unbedingt weit raus")
    synopsis should not include "<"
    synopsis should not include "bo-p"
    all(page.films.flatMap(_.synopsis)) should not include "<"
  }

  it should "keep a multi-paragraph synopsis' breaks as blank lines" in {
    // Evil Dead Burn ships two <p class="bo-p"> blocks: plot, then a
    // "new film by …" note. They must not run together into one wall of text.
    val synopsis = page.films.find(_.title == "Evil Dead Burn").value.synopsis.value
    val paragraphs = synopsis.split("\n\n")
    paragraphs.length shouldBe 2
    all(paragraphs.toSeq) should not be empty
  }

  // ── date discovery off the venue page's data-showtimes-dates ──────────────
  // The horizon lever: the venue page names exactly which days have showtimes
  // across Filmstarts' fixed ~28-day booking window, so the scrape fetches those
  // days (not a blind 7-day grid, and not every empty day out to 28).

  "parseShowtimeDates" should "read the populated days off the venue page, skipping gap days" in {
    val dates = WebediaShowtimesClient.parseShowtimeDates(venuePage).value
    dates.head shouldBe LocalDate.of(2026, 7, 19)
    dates.last shouldBe LocalDate.of(2026, 8, 15)   // ~4 weeks out — well past a 7-day grid
    dates.size shouldBe 24
    dates shouldBe dates.sorted
    // Days present in data-roller-dates (the full window) but with no showtimes
    // are absent from data-showtimes-dates — so we never fetch them.
    dates should contain noneOf (
      LocalDate.of(2026, 7, 27), LocalDate.of(2026, 7, 28),
      LocalDate.of(2026, 8, 10), LocalDate.of(2026, 8, 11))
  }

  it should "be None when the attribute is absent, so the caller fails the scrape" in {
    WebediaShowtimesClient.parseShowtimeDates("<html><body>no showtimes here</body></html>") shouldBe None
  }

  it should "be Some(empty) for a present-but-empty attribute — a legitimately empty venue" in {
    WebediaShowtimesClient.parseShowtimeDates("""<section data-showtimes-dates="[]"></section>""").value shouldBe empty
  }

  "planChunks" should "discover the venue's populated days beyond the fixed 7-day grid" in {
    val client = new WebediaShowtimesClient(
      new ScriptedByUrl(url =>
        if (url.contains("/kinoprogramm/kino/")) venuePage
        else throw new java.io.IOException("planChunks must not fetch per-day pages")),
      WebediaMarket.Germany, "A0263", venue,
      today = Some(LocalDate.of(2026, 7, 19)))

    val days = client.planChunks()
    days.size shouldBe 24
    days should contain("2026-08-15")        // reaches ~4 weeks, not today+6
    days.count(_ > "2026-07-25") should be > 0
    days should not contain "2026-07-27"     // gap day skipped
    days shouldBe days.sorted
  }

  // The bound is a SANITY valve, not a coverage cap — see `ScrapeHorizon`. Every day
  // the venue page advertises is planned, however far out, because a day we do not
  // fetch is a day whose films `MovieCache`'s scrape-prune then deletes as "no longer
  // screening". The old per-client 34-day cap dropped everything past today+34 even
  // though the source had named those days; that is the shape that cost the UK its
  // entire advance-sale programme, and Germany has no aggregator fallback to notice.
  it should "plan every advertised day, however far out, and bound only the absurd" in {
    // today set early enough that the page's last dates sit past the OLD 34-day cap.
    val today  = LocalDate.of(2026, 7, 10)
    val client = new WebediaShowtimesClient(
      new ScriptedByUrl(_ => venuePage),
      WebediaMarket.Germany, "A0263", venue,
      today = Some(today))

    val days = client.planChunks()
    days should contain("2026-08-13")      // was exactly at the old cap
    days should contain("2026-08-15")      // +36d — DROPPED by the old cap, kept now
    all(days) should be <= today.plusDays(WebediaShowtimesClient.MaxHorizonDays.toLong).toString
  }

  it should "still drop an absurd far-future date so one outlier can't balloon the fan-out" in {
    // A garbage date is what the bound is actually for: the source naming a day two
    // millennia out must not plan two millennia of chunk tasks.
    val today   = LocalDate.of(2026, 7, 10)
    val absurd  = today.plusYears(50).toString
    val page    = s"""<section data-showtimes-dates="[&quot;2026-07-11&quot;,&quot;$absurd&quot;]"></section>"""
    val client  = new WebediaShowtimesClient(
      new ScriptedByUrl(_ => page), WebediaMarket.Germany, "A0263", venue, today = Some(today))

    val days = client.planChunks()
    days should contain("2026-07-11")
    days should not contain absurd
  }

  it should "fail the scrape (no grid fallback) when the venue page can't be fetched" in {
    val pageDown = new ScriptedByUrl(_ => throw new java.io.IOException("HTTP 500"))
    a[java.io.IOException] should be thrownBy clientOver(pageDown).planChunks()
  }

  it should "fail the scrape when the venue page carries no data-showtimes-dates attribute" in {
    // A 200 that lost the attribute (Filmstarts markup change / block page) is a
    // discovery failure — throw rather than silently scrape zero days.
    val noAttr = new ScriptedByUrl(_ => "<html><body>unexpected page</body></html>")
    an[IllegalStateException] should be thrownBy clientOver(noAttr).planChunks()
  }

  // ── fetch() through the real /_/showtimes URL (fixture-replayed) ──────────
  // The venue page names one populated day (2026-07-11); that day's screenings
  // come from the recorded per-day fixture.
  "fetch" should "assemble films for the venue via the showtimes endpoint" in {
    val cinemaxxWuerzburg = new GermanCinema("CinemaxX Würzburg", "CinemaxX Würzburg")
    val client = new WebediaShowtimesClient(
      new ScriptedByUrl(url =>
        if (url.contains("/kinoprogramm/kino/")) OneDayVenuePage
        else if (url.contains("d-2026-07-11")) fixture
        else throw new java.io.IOException(s"unexpected fetch: $url")),
      WebediaMarket.Germany, "A0263", cinemaxxWuerzburg,
      today = Some(LocalDate.of(2026, 7, 11)))
    val movies = client.fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(cinemaxxWuerzburg)
    movies.map(_.movie.title) should contain("Vaiana")
    all(movies.map(_.externalIds.keySet)) should contain("webedia")
  }

  // ── a scrape that fetched NOTHING must fail, not report an empty venue ─────
  // A total host failure (every request 429/500) must surface as a throw. If it
  // were swallowed into a fast empty success, HostScrapeStats would record a
  // quick success, drag the host's median down until the adaptive budget pinned
  // at its 8s floor, and cut the venues that were still working (the 2026-07-18
  // Filmstarts 429 incident). Fetched-and-empty is a fact about the venue;
  // fetch-failed is a fact about us — only the latter is an error.
  private val venue = new GermanCinema("CinemaxX Würzburg", "CinemaxX Würzburg")

  private class ScriptedByUrl(respond: String => String) extends tools.GetOnlyHttpFetch {
    def get(url: String): String = respond(url)
  }

  /** A venue page advertising a single populated day — the per-day fixture then
   *  supplies that day's screenings. */
  private val OneDayVenuePage = """<section data-showtimes-dates="[&quot;2026-07-11&quot;]"></section>"""

  private def clientOver(http: tools.HttpFetch) =
    new WebediaShowtimesClient(http, WebediaMarket.Germany, "A0263", venue, today = Some(LocalDate.of(2026, 7, 11)))

  it should "FAIL when the whole scrape can't reach the host, rather than report zero films" in {
    val allFailing = new ScriptedByUrl(_ => throw new java.io.IOException("HTTP 429"))
    a[java.io.IOException] should be thrownBy clientOver(allFailing).fetch()
  }

  it should "report an empty venue when the page lists no showtime days" in {
    // data-showtimes-dates="[]" is a legitimately empty venue, not a failure.
    clientOver(new ScriptedByUrl(_ => """<section data-showtimes-dates="[]"></section>""")).fetch() shouldBe empty
  }

  // ── chunked: one ScrapeChunk task per day, reduced into the venue listing ──
  // fetch() (used only by the fixture harness + these tests) composes
  // reduceChunks ∘ fetchChunk ∘ planChunks; in prod each day is its own task.

  private def fakeClient() =
    new WebediaShowtimesClient(
      new FakeHttpFetch("webedia-de"), WebediaMarket.Germany, "A0263", venue, today = Some(LocalDate.of(2026, 7, 11)))

  "fetchChunk" should "parse one day's page into that day's films" in {
    val films = fakeClient().fetchChunk("2026-07-11")
    films.map(_.movie.title) should contain("Vaiana")
    all(films.map(_.externalIds.keySet)) should contain("webedia")
    all(films.flatMap(_.showtimes).map(_.dateTime.toLocalDate)) should be(LocalDate.of(2026, 7, 11))
  }

  it should "throw a fetch failure so only that day's chunk task reschedules" in {
    val failing = new ScriptedByUrl(_ => throw new java.io.IOException("HTTP 429"))
    a[java.io.IOException] should be thrownBy clientOver(failing).fetchChunk("2026-07-11")
  }

  "reduceChunks" should "merge the same film across days by webedia id, unioning showtimes" in {
    val client = fakeClient()
    val day1   = client.fetchChunk("2026-07-11")
    // Fabricate the next day's chunk: same films, showtimes shifted +1 day.
    val day2 = day1.map(m => m.copy(showtimes = m.showtimes.map(s => s.copy(dateTime = s.dateTime.plusDays(1)))))

    val reduced = client.reduceChunks(Map("2026-07-11" -> day1, "2026-07-12" -> day2))

    reduced.size shouldBe day1.size // one row per film, not doubled
    val vaiana = reduced.find(_.movie.title == "Vaiana").value
    vaiana.showtimes.map(_.dateTime.toLocalDate).distinct should contain allOf
      (LocalDate.of(2026, 7, 11), LocalDate.of(2026, 7, 12))
  }

  it should "keep a day that answered when reduced alongside a missing (failed) one" in {
    // In prod a failed day's chunk reschedules; the reduce runs on whatever
    // landed — so a single dead day degrades to a partial listing, not a loss.
    val day1 = fakeClient().fetchChunk("2026-07-11")
    fakeClient().reduceChunks(Map("2026-07-11" -> day1)).map(_.movie.title) should contain("Vaiana")
  }

  it should "throw from the in-process fetch when any day fails (the task path retries per-day instead)" in {
    // Venue page names two days; the second day's fetch fails.
    val partial = new ScriptedByUrl(url =>
      if (url.contains("/kinoprogramm/kino/")) """<section data-showtimes-dates="[&quot;2026-07-11&quot;,&quot;2026-07-12&quot;]"></section>"""
      else if (url.contains("d-2026-07-11")) fixture
      else throw new java.io.IOException("HTTP 429"))
    a[java.io.IOException] should be thrownBy clientOver(partial).fetch()
  }
}
