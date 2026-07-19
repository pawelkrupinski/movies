package clients.webedia

import models.GermanCinema
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.de.WebediaShowtimesClient

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/** Replays a recorded Webedia website-JSON capture (Germany, Filmstarts theater
 *  A0263, date 2026-07-11) through the pure `parsePage`. Pins that the surviving
 *  `www.filmstarts.de/_/showtimes/theater-<id>/d-<date>/p-<n>/` endpoint parses:
 *  one film per `results[]`, title + `originalTitle` (Moana → a TMDB hint),
 *  production year, "1 Std. 56 Min."-style runtime, director off `credits`, the
 *  `startsAt` local `LocalDateTime`s, a `3D` format token read from `tags`, and
 *  cleaned relay booking links. */
class WebediaShowtimesClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def fixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/webedia-de/www.filmstarts.de/_/showtimes/theater-A0263/d-2026-07-11/p-1.json")
    try src.mkString finally src.close()
  }

  private val page = WebediaShowtimesClient.parsePage(fixture)

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

  it should "flatten version buckets into local-time showtimes with format tokens" in {
    val vaiana = page.films.find(_.title == "Vaiana").value

    // The 13:00 dubbed 3D screening — 3D is read from the `Format.Projection.3d` tag.
    val threeD = vaiana.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 11, 13, 0)).value
    threeD.format should contain("3D")

    // A plain digital screening carries no format token (the German dubbed default).
    val plain = vaiana.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 11, 11, 40)).value
    plain.format shouldBe empty
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

  // ── fetch() through the real /_/showtimes URL (fixture-replayed) ──────────
  "fetch" should "assemble films for the venue via the showtimes endpoint" in {
    val cinemaxxWuerzburg = new GermanCinema("CinemaxX Würzburg", "CinemaxX Würzburg")
    val client = new WebediaShowtimesClient(
      new FakeHttpFetch("webedia-de"), "www.filmstarts.de", "A0263", cinemaxxWuerzburg,
      daysAhead = 0, today = LocalDate.of(2026, 7, 11))
    val movies = client.fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(cinemaxxWuerzburg)
    movies.map(_.movie.title) should contain("Vaiana")
    all(movies.map(_.externalIds.keySet)) should contain("webedia")
  }

  // ── a scrape that fetched NOTHING must fail, not report an empty venue ─────
  // Filmstarts 429'd every request on 2026-07-18. Each day's failure was
  // swallowed into None, so fetch() returned an empty Seq FAST and SUCCESSFULLY
  // — which HostScrapeStats recorded as a quick success, dragging the host's
  // median down until the adaptive budget pinned at its 8s floor and cut the
  // venues that were still working. A total fetch failure has to surface.
  private val venue = new GermanCinema("CinemaxX Würzburg", "CinemaxX Würzburg")

  private class ScriptedByUrl(respond: String => String) extends tools.GetOnlyHttpFetch {
    def get(url: String): String = respond(url)
  }

  /** What Webedia actually serves for a venue with nothing on: HTTP 200, a real
   *  body, `results: []`. Legitimately empty — NOT a failure. */
  private val NoShowtimesBody =
    """{"error":true,"message":"no.showtime.error","nextDate":null,"results":[],
      |"pagination":{"page":1,"totalPages":1,"itemsPerPage":20,"totalItems":0}}""".stripMargin

  private def clientOver(http: tools.HttpFetch, daysAhead: Int = 0) =
    new WebediaShowtimesClient(
      http, "www.filmstarts.de", "A0263", venue,
      daysAhead = daysAhead, today = LocalDate.of(2026, 7, 11))

  it should "FAIL when every day's request failed, rather than report zero films" in {
    val allFailing = new ScriptedByUrl(_ => throw new java.io.IOException("HTTP 429"))
    a[java.io.IOException] should be thrownBy clientOver(allFailing).fetch()
  }

  it should "still report an empty venue when the host ANSWERED with no showtimes" in {
    // The distinction that matters: fetched-and-empty is a fact about the venue,
    // fetch-failed is a fact about us. Only the latter is an error.
    clientOver(new ScriptedByUrl(_ => NoShowtimesBody)).fetch() shouldBe empty
  }

  // ── chunked: one ScrapeChunk task per day, reduced into the venue listing ──
  // fetch() (used only by the fixture harness + these tests) composes
  // reduceChunks ∘ fetchChunk ∘ planChunks; in prod each day is its own task.

  private def fakeClient(daysAhead: Int = 0) =
    new WebediaShowtimesClient(
      new FakeHttpFetch("webedia-de"), "www.filmstarts.de", "A0263", venue,
      daysAhead = daysAhead, today = LocalDate.of(2026, 7, 11))

  "planChunks" should "yield one chunk key per day across the window" in {
    fakeClient(daysAhead = 6).planChunks() shouldBe
      (0 to 6).map(d => LocalDate.of(2026, 7, 11).plusDays(d.toLong).toString)
  }

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
    val partial = new ScriptedByUrl(url =>
      if (url.contains("d-2026-07-11")) fixture
      else throw new java.io.IOException("HTTP 429"))
    a[java.io.IOException] should be thrownBy clientOver(partial, daysAhead = 1).fetch()
  }
}
