package clients.webedia

import clients.tools.FakeHttpFetch
import models.GermanCinema
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.WebediaShowtimesClient

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

  it should "tolerate a PARTIAL failure, keeping the days that did answer" in {
    val partial = new ScriptedByUrl(url =>
      if (url.contains("d-2026-07-11")) fixture
      else throw new java.io.IOException("HTTP 429"))
    val movies = clientOver(partial, daysAhead = 1).fetch()
    movies.map(_.movie.title) should contain("Vaiana")
  }
}
