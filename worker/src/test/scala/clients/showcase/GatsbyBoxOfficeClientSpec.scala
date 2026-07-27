package clients.showcase

import clients.tools.FakeHttpFetch
import models.{CinemaMovie, ShowcaseDeLuxBluewater}
import org.scalatest.{LoneElement, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{GatsbyBoxOfficeClient, GatsbyBoxOfficeParser, WebediaBoxOffice}

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays Showcase Cinema de Lux Bluewater (`X06JR`) through
 * [[GatsbyBoxOfficeClient]] entirely from disk — the two real responses
 * recorded 2026-07-27:
 *
 *   - `page-data/sq/d/3836549025.json` — the chain-wide `allMovie` catalogue
 *   - `api/gatsby-source-boxofficeapi/schedule.<queryFingerprint>` — the ONE
 *     call covering the client's whole 210-day horizon
 *
 * The fixture's `from`/`to` are the ones `scheduleUrl` builds for
 * `today = 2026-07-27`, so pinning `today` here is what makes the replay
 * resolve; a horizon or URL-builder change moves the fingerprint and this spec
 * fails loudly rather than silently scraping a different window.
 *
 * Everyman runs the identical backend on its own host (same static-query
 * hashes, same schedule shape, verified live), so covering one brand covers
 * both — only `baseUrl` differs.
 */
class GatsbyBoxOfficeClientSpec extends AnyFlatSpec with Matchers with OptionValues with LoneElement {

  private val Today   = LocalDate.of(2026, 7, 27)
  private val Bluewater = "X06JR"

  private val films: Seq[CinemaMovie] =
    new GatsbyBoxOfficeClient(
      new FakeHttpFetch("showcase"),
      GatsbyBoxOfficeClient.ShowcaseBaseUrl,
      Bluewater,
      ShowcaseDeLuxBluewater,
      today = Today
    ).fetch()

  private def film(title: String): CinemaMovie = films.find(_.movie.title == title).value

  "fetch" should "join the venue's schedule against the chain catalogue into one row per film" in {
    films.size shouldBe 72
    films.map(_.cinema).toSet shouldBe Set(ShowcaseDeLuxBluewater)
    films.map(_.movie.title) shouldBe films.map(_.movie.title).sorted
    all(films.map(_.showtimes.size)) should be > 0
  }

  it should "carry the catalogue's title, poster, film page and genres" in {
    val jurassic = film("Jurassic Park")
    jurassic.posterUrl.value shouldBe
      "https://all.web.img.acsta.net/img/3b/44/3b44190963f93fab764748e07b5b554c.webp"
    jurassic.filmUrl.value shouldBe
      s"${GatsbyBoxOfficeClient.ShowcaseBaseUrl}/movies/8488-jurassic-park"
    // "ADVENTURE, SCIENCE_FICTION" un-shouted, not passed through verbatim.
    jurassic.movie.genres shouldBe Seq("Adventure", "Science fiction")
    jurassic.externalIds shouldBe Map("boxoffice" -> "8488")
    // originalTitle echoes the title for English-language films — dropped, so a
    // TMDB search doesn't get the same string twice.
    jurassic.movie.originalTitle shouldBe None
  }

  it should "parse each session to the venue's local date-time, screen and booking link" in {
    val first = film("Jurassic Park").showtimes.head
    first.dateTime shouldBe LocalDateTime.of(2026, 7, 27, 13, 5)
    first.room.value shouldBe "8"
    first.bookingUrl.value shouldBe
      "https://tickets.showcasecinemas.co.uk/launch/ticketing/cb83e445-7282-5bbc-9612-1bfba7b4bc1a"
    first.format shouldBe List("2D")   // Format.Projection.Digital, nothing else
  }

  it should "prefer the brand's own ticketing link over the relay redirector" in {
    val booking = films.flatMap(_.showtimes).flatMap(_.bookingUrl)
    booking should not be empty
    // The relay leg carries unencoded spaces/semicolons in its `code=` param; the
    // `default` provider is the clean URL and must win everywhere.
    all(booking) should startWith("https://tickets.showcasecinemas.co.uk/launch/ticketing/")
    booking.filter(_.contains(" ")) shouldBe empty
  }

  it should "surface the film's trailer when the catalogue has one" in {
    film("Back to The Future").trailerUrl.value shouldBe "https://www.youtube.com/watch?v=U71BvFM7Wpw"
    film("Jurassic Park").trailerUrl shouldBe None   // trailer.youtube is null for this node
  }

  // ── the dotted tag taxonomy → format tokens ───────────────────────────────

  it should "read 3D off either the projection tag or the auditorium system" in {
    val spidey = film("Spider-Man: Brand New Day").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 29, 18, 30)).value
    spidey.format shouldBe List("3D")   // Format.Projection.3d + Auditorium.Experience.RealD3D, once
    spidey.room.value shouldBe "17"
  }

  it should "translate the premium-format tags" in {
    // XPlus screen: Auditorium.Experience.PLF + Format.Projection.Laser, and NO
    // Format.Projection.Digital — so no dimension token, just the premium pair.
    film("Final Destination").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 28, 22, 10)).value
      .format shouldBe List("LASER", "PLF")
  }

  it should "translate the subtitled tag into a language token alongside the dimension" in {
    film("Animal Farm").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 28, 15, 0)).value
      .format shouldBe List("2D", "SUB")
  }

  it should "map IMAX from the vendor taxonomy even though no UK venue runs one" in {
    GatsbyBoxOfficeParser.formatTokens(Seq("Format.Projection.Imax", "Format.Projection.Digital")) shouldBe
      List("2D", "IMAX")
  }

  it should "badge the spoken language of a foreign-language screening" in {
    // "Jana Nayagan" is a Tamil release shown subtitled — the session carries
    // Localization.Language.Tamil alongside Showtime.Accessibility.Subtitled, so
    // the token reads "TAMIL" (what it's in) after "SUB" (how you read it).
    film("Jana Nayagan").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 27, 20, 50)).value
      .format shouldBe List("2D", "SUB", "TAMIL")
  }

  // ── the horizon: ONE call, no per-day fan-out ─────────────────────────────

  "the 210-day horizon" should "be spanned by a single schedule request" in {
    GatsbyBoxOfficeClient.scheduleUrl(
      GatsbyBoxOfficeClient.ShowcaseBaseUrl, Bluewater, GatsbyBoxOfficeClient.UkTimeZone,
      Today, Today.plusDays(GatsbyBoxOfficeClient.MaxHorizonDays.toLong)
    ) shouldBe
      s"${GatsbyBoxOfficeClient.ShowcaseBaseUrl}/api/gatsby-source-boxofficeapi/schedule" +
        "?theaters=%7B%22id%22%3A%22X06JR%22%2C%22timeZone%22%3A%22Europe%2FLondon%22%7D" +
        "&from=2026-07-27T00:00:00&to=2027-02-22T00:00:00"
  }

  it should "return days reaching far past a one-week grid, gap days simply absent" in {
    val days = films.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    days.size shouldBe 65
    days.head shouldBe Today
    days.last shouldBe LocalDate.of(2027, 2, 7)          // ~6 months out, in ONE request
    all(days.map(_.toString)) should be <= "2027-02-22"  // inside the horizon
  }

  // ── parser edge cases the live snapshot happens not to contain ────────────
  // The recorded payload has 1151 sessions and zero `isExpired` ones, and every
  // scheduled id resolves in the catalogue. Both branches are still real (the
  // platform sets isExpired on a day's already-started screenings), so they are
  // probed with a minimal hand-built payload — the golden path above is what the
  // real recorded fixture guards.

  private val catalogue =
    """{"data":{"allMovie":{"nodes":[
       {"id":"1","title":"Kept","originalTitle":"Kept","poster":null,"path":"/movies/1-kept","genres":"DRAMA","trailer":{"HD":null,"SD":null,"youtube":null}}
     ]}}}"""

  private def schedule(sessions: String) =
    s"""{"X06JR":{"schedule":{"1":{"2026-07-27":[$sessions]}}}}"""

  private val live    = """{"startsAt":"2026-07-27T10:00:00","tags":[],"isExpired":false,"data":{"ticketing":[{"urls":["https://x/live"],"provider":"default"}]}}"""
  private val expired = """{"startsAt":"2026-07-27T09:00:00","tags":[],"isExpired":true,"data":{"ticketing":[{"urls":["https://x/gone"],"provider":"default"}]}}"""

  private def parsed(scheduleJson: String, catalogueJson: String = catalogue) =
    GatsbyBoxOfficeParser.parse(scheduleJson, catalogueJson, Bluewater, ShowcaseDeLuxBluewater,
      GatsbyBoxOfficeClient.ShowcaseBaseUrl)

  "the parser" should "drop expired sessions and keep the live ones" in {
    val showtimes = parsed(schedule(s"$expired,$live")).loneElement.showtimes
    showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 7, 27, 10, 0))
  }

  it should "drop a film whose every session expired rather than emit a showtime-less row" in {
    parsed(schedule(expired)) shouldBe empty
  }

  it should "drop a scheduled id the catalogue can't name" in {
    // A bare "1000048157" is unenrichable and unshowable — better no row than that one.
    parsed(schedule(live), catalogueJson = """{"data":{"allMovie":{"nodes":[]}}}""") shouldBe empty
  }

  it should "strip the attribute marker the relay redirector glues onto its URL" in {
    WebediaBoxOffice.cleanBookingUrl(
      "https://relay.mvtx.us/ticketing/dbz?code_theater=X06JR&code=Gallery; Recliner; ReservedSeating"
    ) shouldBe "https://relay.mvtx.us/ticketing/dbz?code_theater=X06JR&code=Gallery"
  }
}
