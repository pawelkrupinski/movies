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

  "a details response naming none of the films asked" should "be asked once more, not taken as films without credits" in {
    // Landmark, 2026-09-29: three venues' whole `movies?ids=` batches came back 200 but unparseable,
    // so every film there was listed bare and "Nosferatu" (Eggers, 132 min) resolved as the 1922 film.
    // Each batch's first answer is the busy page; asked again, it names Jurassic Park.
    val asked = scala.collection.mutable.Map.empty[String, Int]
    val fetch = new tools.GetOnlyHttpFetch {
      private val recorded = new FakeHttpFetch("showcase")
      def get(url: String): String = synchronized {
        if (!url.contains("/movies?ids=")) recorded.get(url)
        else {
          asked(url) = asked.getOrElse(url, 0) + 1
          if (asked(url) == 1) "<html><body>Service busy</body></html>"
          else """[{"id":"8488","direction":["Steven Spielberg"],"runtime":7620}]"""
        }
      }
    }
    val jurassic = new GatsbyBoxOfficeClient(fetch, GatsbyBoxOfficeClient.ShowcaseBaseUrl, Bluewater, ShowcaseDeLuxBluewater, today = Today)
      .fetch().find(_.movie.title == "Jurassic Park").value
    jurassic.director shouldBe Seq("Steven Spielberg")
    jurassic.movie.runtimeMinutes shouldBe Some(127)
  }

  "fetch" should "join the venue's schedule against the chain catalogue into one row per film" in {
    films.size shouldBe 81
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
    first.dateTime shouldBe LocalDateTime.of(2026, 7, 28, 13, 0)
    first.room.value shouldBe "8"
    first.bookingUrl.value shouldBe
      "https://tickets.showcasecinemas.co.uk/launch/ticketing/4722a021-7132-5ea4-91c8-f9e19c22bf70"
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
        "&from=2026-07-27T00:00:00&to=2028-07-26T00:00:00"
  }

  it should "return days reaching far past a one-week grid, gap days simply absent" in {
    val days = films.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    days.size shouldBe 75
    days.head shouldBe Today
    // Re-recorded 2026-07-27 at the shared 2-year horizon: 10 more programme days and 9
    // more films than the old 210-day window returned, all of it advance-sale stock the
    // cap used to hide — and hiding it is what had scrape-prune delete those films.
    days.last shouldBe LocalDate.of(2027, 5, 30)
    all(days.map(_.toString)) should be <= "2028-07-26"  // inside the sanity bound
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

  it should "fail, not read as an empty programme, when the schedule or the catalogue is not the expected JSON" in {
    a[Exception] should be thrownBy parsed("<html>502 Bad Gateway</html>")
    a[Exception] should be thrownBy parsed(schedule(live), catalogueJson = "<html>502 Bad Gateway</html>")
    a[Exception] should be thrownBy parsed(schedule(live), catalogueJson = """{"errors":[{"message":"rate limited"}]}""")
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

  "the film details" should "carry each film's director, co-directors, running time, cast and synopsis" in {
    // Everyman's `movies?ids=` for three films, recorded 2026-09-29: the credits and runtimes the
    // catalogue leaves null. "Dracula (4K Restoration)" is Terence Fisher's 1958 Hammer film, which
    // a bare title could not tell from Browning's 1931 "Dracula"; runtime arrives in seconds.
    val details = GatsbyBoxOfficeParser.parseDetails(
      scala.io.Source.fromFile("test/resources/fixtures/everyman/film-details.json").mkString)
    val dracula = details("1000051270")
    dracula.directors shouldBe Seq("Terence Fisher")
    dracula.runtimeMinutes.value shouldBe 93
    dracula.cast should contain ("Christopher Lee")
    dracula.synopsis.value should startWith ("A landmark in Gothic horror")
    details("1000046138").runtimeMinutes.value shouldBe 195   // RBO 2026/27 Tosca, not 2025/26's 210
    details("1000046138").directors shouldBe Seq("Oliver Mears")
  }

  it should "carry the film's certificate, which the UK brands never published before" in {
    val details = GatsbyBoxOfficeParser.parseDetails(
      scala.io.Source.fromFile("test/resources/fixtures/everyman/film-details.json").mkString)
    details("1000036867").certificate.value shouldBe "12A"     // Avengers Endgame: Encore
    details("1000051270").certificate shouldBe None            // Dracula (4K Restoration): not yet rated
  }

  it should "rate a listing only in its brand's own system" in {
    val schedule  = scala.io.Source.fromFile(showcaseFixture("schedule")).mkString
    val catalogue = scala.io.Source.fromFile(showcaseFixture("page-data")).mkString
    def rated(certificate: String, ratings: Set[String]) = {
      val details = Map("8488" -> GatsbyBoxOfficeParser.FilmDetails(Nil, None, Nil, None, Some(certificate)))
      GatsbyBoxOfficeParser.parse(schedule, catalogue, Bluewater, ShowcaseDeLuxBluewater, GatsbyBoxOfficeClient.ShowcaseBaseUrl, details, ratings)
        .find(_.movie.title == "Jurassic Park").value.ageRating
    }
    rated("12A", GatsbyBoxOfficeParser.BbfcCertificates) shouldBe Some("12A")
    rated("PG-13", GatsbyBoxOfficeParser.BbfcCertificates) shouldBe None   // an MPA rating on a UK card
    rated("PG-13", GatsbyBoxOfficeParser.MpaCertificates) shouldBe Some("PG-13")
    rated("TBC", GatsbyBoxOfficeParser.BbfcCertificates) shouldBe None      // a placeholder
  }

  it should "fill the listing it belongs to, and leave the rest as the catalogue has them" in {
    val schedule  = scala.io.Source.fromFile(showcaseFixture("schedule")).mkString
    val catalogue = scala.io.Source.fromFile(showcaseFixture("page-data")).mkString
    val details   = Map("8488" -> GatsbyBoxOfficeParser.FilmDetails(Seq("Steven Spielberg"), Some(127), Seq("Sam Neill"), Some("Dinosaurs.")))
    val parsed    = GatsbyBoxOfficeParser.parse(schedule, catalogue, Bluewater, ShowcaseDeLuxBluewater, GatsbyBoxOfficeClient.ShowcaseBaseUrl, details)
    val jurassic  = parsed.find(_.movie.title == "Jurassic Park").value
    jurassic.director shouldBe Seq("Steven Spielberg")
    jurassic.movie.runtimeMinutes.value shouldBe 127
    parsed.filterNot(_.movie.title == "Jurassic Park").flatMap(_.director) shouldBe empty
  }

  it should "leave every film listed when the details request fails" in {
    // The Bluewater replay records no details call: the scrape is the schedule, credits are extra.
    films.size shouldBe 81
    films.flatMap(_.director) shouldBe empty
  }

  private def showcaseFixture(kind: String): java.io.File = {
    import scala.jdk.CollectionConverters.*
    val base = java.nio.file.Paths.get("test/resources/fixtures/showcase/www.showcasecinemas.co.uk")
    java.nio.file.Files.walk(base).iterator.asScala.map(_.toFile).filter(_.isFile)
      .find(f => if (kind == "schedule") f.getName.startsWith("schedule") else f.getName == "3836549025.json")
      .getOrElse(fail(s"no recorded $kind response under $base"))
  }
}
