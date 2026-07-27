package clients.odeon

import clients.tools.FakeHttpFetch
import models.OdeonBirminghamNewStreet
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.uk.{OdeonClient, OdeonParser}

import java.time.LocalDate
import java.time.LocalDateTime
import scala.io.Source

/**
 * Replays REAL captured Odeon `WSVistaWebClient/ocapi/v1` payloads (site 017,
 * ODEON Birmingham New Street, harvested 2026-07-27) from disk — the roster,
 * one venue's `film-screening-dates`, and one date's `showtimes`. Pins the
 * three parsers, the per-day chunk plan/fetch, and the token contract. NEVER
 * hits the network.
 */
class OdeonClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val cinema = OdeonBirminghamNewStreet
  private val siteId = "017"
  private val date   = LocalDate.of(2026, 7, 27)

  private def read(path: String): String = {
    val src = Source.fromFile(path)
    try src.mkString finally src.close()
  }

  private val fixtureBase =
    "test/resources/fixtures/odeon/vwc.odeon.co.uk/WSVistaWebClient/ocapi/v1"

  // ── pure parsers over the recorded payloads ───────────────────────────────

  "parseSites" should "read the full venue roster with ids and names" in {
    val sites = OdeonParser.parseSites(read(s"$fixtureBase/sites.json"))
    sites.size shouldBe 113
    sites.map(_.id) should contain("017")
    sites.find(_.id == "017").value.name shouldBe "Birmingham New Street"
  }

  "parseScreeningDates" should "read the venue's business dates, deduped + sorted" in {
    val dates = OdeonParser.parseScreeningDates(read(s"$fixtureBase/film-screening-dates.json"))
    dates.size shouldBe 29
    dates.head shouldBe LocalDate.of(2026, 7, 27)
    dates.last shouldBe LocalDate.of(2026, 12, 13)   // ~4.5 months out — event/advance dates
    dates shouldBe dates.sorted
  }

  private val movies =
    OdeonParser.parseShowtimes(read(s"$fixtureBase/showtimes/by-business-date/2026-07-27.json"), cinema)

  "parseShowtimes" should "produce one row per film on the day" in {
    movies.map(_.movie.title).distinct.size shouldBe 7
    movies.map(_.cinema).toSet shouldBe Set(cinema)
    movies.map(_.movie.title) should contain("The Super Mario Galaxy Movie")
  }

  it should "carry a film's title, runtime, trailer, filmUrl and Odeon id" in {
    val mario = movies.find(_.movie.title == "The Super Mario Galaxy Movie").value
    mario.movie.runtimeMinutes.value shouldBe 98
    mario.trailerUrl.value should include("youtube.com")
    mario.filmUrl.value should (include("/films/") and include(s"siteId=$siteId"))
    mario.externalIds.get("odeon").value should startWith("HO")
  }

  it should "map a screening to a local date-time with a seat-picker booking URL, screen and format tag" in {
    val toyStory = movies.find(_.movie.title == "Toy Story 5").value
    val nineFifteen = toyStory.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 9, 15)).value
    nineFifteen.bookingUrl.value shouldBe "https://www.odeon.co.uk/ticketing/seat-picker/?showtimeId=017-68473"
    nineFifteen.room.value shouldBe "Screen 8"
    // The showtime's attributes are "Audio Described" + "Saver": the access tag
    // is surfaced, the pricing tier is dropped (not a screen format).
    nineFifteen.format should contain("Audio Described")
    nineFifteen.format should not contain "Saver"
  }

  it should "resolve the film's synopsis, cast, director and genres from relatedData" in {
    val mario = movies.find(_.movie.title == "The Super Mario Galaxy Movie").value
    mario.synopsis.value should include("Super Mario")
    // castAndCrew ids joined against relatedData: directors carry role "Director".
    mario.director should contain("Aaron Horvath")
    // Names are stitched from given/middle/family with blank parts dropped, so
    // no stray double-spaces survive the space-padded source fields.
    mario.director should not contain "Aaron  Horvath"
    mario.cast should contain("Charlie Day")
    mario.cast should contain("Jack Black")
    // genreIds resolved against relatedData.genres[].name.text.
    mario.movie.genres should contain("Animation")
  }

  it should "resolve the film's age rating from relatedData.censorRatings by id" in {
    // film.censorRatingId → censorRatings[].classification.text, kept verbatim.
    movies.find(_.movie.title == "The Super Mario Galaxy Movie").value.ageRating.value shouldBe "PG"
    // A different rating id resolves to its own label.
    movies.find(_.movie.title == "Obsession").value.ageRating.value shouldBe "18"
  }

  it should "NOT trust the API release year (re-releases are dated to the original year)" in {
    // We never read `film.releaseDate` — EmbeddedYear / TMDB backfill the year.
    movies.flatMap(_.movie.releaseYear) shouldBe empty
  }

  "formatToken" should "recognise screen formats and access features, dropping pricing/promo tags" in {
    OdeonParser.formatToken("IMAX with Laser").value shouldBe "IMAX"
    OdeonParser.formatToken("Dolby Atmos").value shouldBe "Dolby"
    OdeonParser.formatToken("Audio Described").value shouldBe "Audio Described"
    OdeonParser.formatToken("Open Captioned").value shouldBe "Subtitled"
    OdeonParser.formatToken("Saver") shouldBe None
    OdeonParser.formatToken("£5 web") shouldBe None
    OdeonParser.formatToken("Flash Warning") shouldBe None
  }

  // ── chunked scrape over the recorded corpus ───────────────────────────────

  private val fake  = new FakeHttpFetch("odeon")
  private val token = () => Some("test-bearer")

  private def client(today: LocalDate = date) =
    new OdeonClient(fake, siteId, cinema, token, today = today)

  "planChunks" should "discover the venue's business dates bounded to the horizon" in {
    val days = client().planChunks()
    days.size shouldBe 29
    days.head shouldBe "2026-07-27"
    days should contain("2026-12-13")   // far-out advance date, within the cap
    days shouldBe days.sorted
  }

  // A sanity valve, not a coverage cap — see `ScrapeHorizon`. Odeon advertises event and
  // advance dates months out; dropping them from the listing is what had scrape-prune
  // delete those films entirely.
  it should "plan every advertised day, however far out, and bound only the absurd" in {
    val days = client(today = LocalDate.of(2026, 6, 1)).planChunks()
    val cap  = LocalDate.of(2026, 6, 1).plusDays(OdeonClient.MaxHorizonDays.toLong).toString
    all(days) should be <= cap                 // ISO dates order lexicographically
    days should contain("2026-12-13")          // advance date — KEPT now
  }

  "fetchChunk" should "fetch + parse a single business date's showtimes into films" in {
    val day = client().fetchChunk("2026-07-27")
    day.map(_.movie.title) should contain("Toy Story 5")
    day.map(_.cinema).toSet shouldBe Set(cinema)
  }

  // ── token contract ────────────────────────────────────────────────────────
  // No harvested token ⇒ THROW (not return empty): an un-authed ocapi call would
  // 401, and an empty result would masquerade as "nothing on", silently dropping
  // the venue's whole listing. Throwing makes it a normal scrape FAILURE so the
  // last-known listing is kept and /uptime shows the problem.

  "planChunks" should "throw when no bearer token has been harvested" in {
    an[IllegalStateException] should be thrownBy
      new OdeonClient(fake, siteId, cinema, () => None, today = date).planChunks()
  }

  "fetchChunk" should "throw when the harvested token is empty" in {
    an[IllegalStateException] should be thrownBy
      new OdeonClient(fake, siteId, cinema, () => Some(""), today = date).fetchChunk("2026-07-27")
  }
}
