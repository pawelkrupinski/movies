package clients.amc

import clients.tools.{FailingHttpFetch, UrlFragmentHttpFetch}
import models.{Cinema, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.us.{AmcClient, AmcParser}
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/**
 * Replays REAL recorded AMC responses (AMC Town Center 20, Kansas City, captured
 * 2026-08-30) through the pure parser and the client's chunk planning.
 *
 * Pins the three cases the venue's health hangs on:
 *  - a venue with a normal programme parses field-for-field;
 *  - a venue with NOTHING ON comes back EMPTY rather than throwing — an empty day
 *    is data, not an outage, and throwing on it is what left five UK venues
 *    permanently red on /uptime (2026-07-26);
 *  - a FETCH FAILURE propagates, so the scrape is recorded as failed and the
 *    venue keeps its last-known listing instead of being pruned to nothing.
 */
class AmcClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val cinema: Cinema =
    UsRoster.byDisplayName.get("AMC Town Center 20")
      .getOrElse(fail("AMC Town Center 20 is missing from the US roster"))

  private def fixture(path: String): String = {
    val src = Source.fromFile(s"test/resources/fixtures/amc/$path")
    try src.mkString finally src.close()
  }

  private val venuePage =
    fixture("www.amctheatres.com/movie-theatres/kansas-city/amc-town-center-20/showtimes.html")
  private val dayJson   = fixture("graph.amctheatres.com/amc-town-center-20-2026-09-05.json")
  private val emptyJson = fixture("graph.amctheatres.com/amc-empire-25-2027-09-18.json")

  // ── the day list ────────────────────────────────────────────────────────────

  "parseDates" should "read the venue's whole advertised day list off its date picker" in {
    val dates = AmcParser.parseDates(venuePage)
    dates should have size 124
    dates.head shouldBe LocalDate.of(2026, 8, 30)
    dates.last shouldBe LocalDate.of(2027, 9, 24)
  }

  it should "keep the sparse advance-sale tail rather than stopping at the first gap" in {
    val dates = AmcParser.parseDates(venuePage)
    // The tail is sparse — a consecutive-blank walk would cut here and drop
    // everything past it, which is the failure ScrapeHorizon exists to forbid.
    dates should contain(LocalDate.of(2027, 6, 9))
    val gaps = dates.sliding(2).collect { case Seq(a, b) => a.until(b).getDays }.toSeq
    gaps.max should be > 14
  }

  "hasDatePicker" should "tell a venue page from a page we failed to parse" in {
    AmcParser.hasDatePicker(venuePage) shouldBe true
    // A real AMC page that is not a venue page carries no picker at all.
    AmcParser.hasDatePicker(fixture("www.amctheatres.com/movie-theatres.html")) shouldBe false
  }

  // ── a normal programme ──────────────────────────────────────────────────────

  "parseDay" should "read every film on the day" in {
    AmcParser.parseDay(dayJson, cinema).map(_.filmUrl).distinct should have size 9
  }

  it should "carry a film's title, runtime, MPAA rating, genre, poster and AMC id" in {
    val odyssey = AmcParser.parseDay(dayJson, cinema)
      .find(_.movie.title == "The Odyssey").value
    odyssey.movie.runtimeMinutes.value shouldBe 172
    odyssey.ageRating.value             shouldBe "R"
    odyssey.movie.genres                shouldBe Seq("Action")
    odyssey.externalIds                 shouldBe Map("amc" -> "the-odyssey-76238")
    odyssey.filmUrl.value               shouldBe "https://www.amctheatres.com/movies/the-odyssey-76238"
    odyssey.posterUrl.value               should include("amc-theatres-res.cloudinary.com")
    odyssey.synopsis.value                should not be empty
  }

  it should "split the comma-joined director and cast strings" in {
    val cars = AmcParser.parseDay(dayJson, cinema)
      .find(_.movie.title == "Cars: 20th Anniversary").value
    cars.director shouldBe Seq("JOE RANFT", "John Lasseter")
    cars.cast     shouldBe Seq("Bonnie Hunt", "Owen Wilson", "Paul Newman")
  }

  it should "convert a screening's UTC start into the theatre's own wall clock" in {
    val dogStars = AmcParser.parseDay(dayJson, cinema)
      .find(_.movie.title == "The Dog Stars").value
    // "2026-09-05T15:00:00.000Z" at utcOffset "-05:00" is a 10:00 show.
    dogStars.showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 9, 5, 10, 0)
    dogStars.showtimes.map(_.dateTime) shouldBe sorted
  }

  it should "carry each screening's auditorium, format labels and booking link" in {
    val odyssey = AmcParser.parseDay(dayJson, cinema)
      .find(_.movie.title == "The Odyssey").value
    val first = odyssey.showtimes.head
    first.room.value shouldBe "11"
    first.format      should contain("IMAX at AMC")
    first.bookingUrl.value shouldBe "https://www.amctheatres.com/showtimes/146604687"
  }

  it should "NOT read AMC's release date as the film's production year" in {
    // AMC stamps the RE-release year on a revival, so reading it would poison
    // TMDB resolution the way Cineworld's would.
    AmcParser.parseDay(dayJson, cinema)
      .find(_.movie.title == "Cars: 20th Anniversary").value
      .movie.releaseYear shouldBe None
  }

  // ── a venue with nothing on ─────────────────────────────────────────────────

  it should "return EMPTY for a day the venue has nothing on, not throw" in {
    // A real AMC answer for an advertised-but-unsold day: `items` present, empty.
    AmcParser.parseDay(emptyJson, cinema) shouldBe empty
  }

  // ── a response we failed to parse ───────────────────────────────────────────

  it should "throw when the response carries no items array at all" in {
    an[IllegalStateException] should be thrownBy
      AmcParser.parseDay("""{"data":{"viewer":{}}}""", cinema)
    an[IllegalStateException] should be thrownBy AmcParser.parseDay("<html>nope</html>", cinema)
  }

  // ── the client's chunk planning ─────────────────────────────────────────────

  private def clientOn(http: tools.HttpFetch) =
    new AmcClient(http, "kansas-city", "amc-town-center-20", cinema,
      today = Some(LocalDate.of(2026, 8, 30)))

  "planChunks" should "plan one chunk per advertised day" in {
    val chunks = clientOn(UrlFragmentHttpFetch("amc-town-center-20/showtimes" -> venuePage)).planChunks()
    chunks should have size 124
    chunks.head shouldBe "2026-08-30"
  }

  it should "throw when the venue page carries no date picker" in {
    val http = UrlFragmentHttpFetch(
      "amc-town-center-20/showtimes" -> fixture("www.amctheatres.com/movie-theatres.html"))
    an[IllegalStateException] should be thrownBy clientOn(http).planChunks()
  }

  it should "propagate a fetch failure rather than reporting an empty listing" in {
    // A swallowed failure reads as a successful "0 showtimes" scrape, which
    // prunes the venue's whole listing. It must surface as a failed scrape.
    a[HttpStatusException] should be thrownBy clientOn(new FailingHttpFetch()).planChunks()
  }

  "fetchChunk" should "propagate a failure of the day's GraphQL POST" in {
    a[HttpStatusException] should be thrownBy clientOn(new FailingHttpFetch()).fetchChunk("2026-09-05")
  }

  // ── wiring ──────────────────────────────────────────────────────────────────

  "the venue map" should "address every mapped venue with slugs AMC's own roster gave us" in {
    val map = services.cinemas.us.AmcVenueMap.byFlicksSlug
    map should have size 519
    map("amc-town-center-20") shouldBe ("kansas-city", "amc-town-center-20")
    // Every AMC theatre slug is claimed by exactly one of our venues — a repeat
    // would silently point two cinemas at one programme.
    map.values.map(_._2).toSeq.distinct should have size map.size
  }

  it should "put the venue's own showtimes page behind its /uptime link" in {
    clientOn(new FailingHttpFetch()).sourceUrl.value shouldBe
      "https://www.amctheatres.com/movie-theatres/kansas-city/amc-town-center-20/showtimes"
  }

  it should "declare both hosts the scrape touches" in {
    clientOn(new FailingHttpFetch()).scrapeHosts shouldBe
      Set("www.amctheatres.com", "graph.amctheatres.com")
  }
}
