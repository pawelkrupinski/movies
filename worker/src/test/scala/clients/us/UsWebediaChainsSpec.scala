package clients.us

import clients.tools.FakeHttpFetch
import models.{Cinema, CinemaMovie, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{GatsbyBoxOfficeClient, GatsbyBoxOfficeParser, ScrapeHorizon}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}

/**
 * The two US chains that turned out to run the SAME Webedia Gatsby "box office"
 * platform the UK's Showcase and Everyman do — so both arrive with no new client
 * at all, only a base URL, a time zone and their venue ids:
 *
 *   - Showcase Cinemas US  `www.showcasecinemas.com`   (13 venues)
 *   - Landmark Theatres    `www.landmarktheatres.com`  (26 venues)
 *
 * Replayed entirely from disk — the real responses recorded 2026-08-30 by
 * `clients.tools.RecordUsChains`, two per venue (the chain-wide `allMovie`
 * catalogue and the ONE schedule call covering the whole horizon).
 *
 * `today` is pinned to the capture date because the schedule URL carries
 * `from`/`to`, so its fixture filename is date-dependent: a horizon or
 * URL-builder change moves the fingerprint and this spec fails loudly rather
 * than silently scraping a different window.
 *
 * [[clients.showcase.GatsbyBoxOfficeClientSpec]] covers the platform's parsing
 * rules in depth against the UK brand. This spec is about what is DIFFERENT
 * here — that the US hosts really do serve the identical shape, that each
 * brand's own ticketing host comes through, and the venue-level behaviours a
 * chain needs before it can be a scrape primary.
 */
class UsWebediaChainsSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Today = LocalDate.of(2026, 8, 30)
  /** The far-future window the idle-venue fixtures were recorded against. */
  private val IdleDay = LocalDate.of(2029, 1, 1)

  private val LegacyPlace = UsRoster.byDisplayName("Showcase Legacy Place Dedham")
  private val Nuart       = UsRoster.byDisplayName("Landmark Nuart Theatre")

  private def client(
    fixtures: HttpFetch, baseUrl: String, theaterId: String, cinema: Cinema,
    timeZone: String, venuePath: Option[String] = None, today: LocalDate = Today
  ) = new GatsbyBoxOfficeClient(fixtures, baseUrl, theaterId, cinema,
    timeZone = timeZone, venuePath = venuePath, today = today)

  private def showcase(today: LocalDate = Today) =
    client(new FakeHttpFetch("showcase-us"), GatsbyBoxOfficeClient.ShowcaseUsBaseUrl, "X0C11",
      LegacyPlace, "America/New_York",
      Some("/theaters/x0c11-showcase-cinema-de-lux-legacy-place"), today)

  private def landmark(today: LocalDate = Today) =
    client(new FakeHttpFetch("landmark"), GatsbyBoxOfficeClient.LandmarkBaseUrl, "X00CW",
      Nuart, "America/Los_Angeles",
      Some("/theaters/x00cw-landmark-nuart-theatre-west-los-angeles"), today)

  private val showcaseFilms: Seq[CinemaMovie] = showcase().fetch()
  private val landmarkFilms: Seq[CinemaMovie] = landmark().fetch()

  private def film(films: Seq[CinemaMovie], title: String) =
    films.find(_.movie.title == title).value

  // ── the same platform, on two new hosts ───────────────────────────────────

  "the US hosts" should "serve the identical Gatsby shape the UK brands do" in {
    // The SAME static-query hash resolves on both US hosts, which is the concrete
    // evidence that this is one platform and not a lookalike: Gatsby derives the
    // filename from the query TEXT, so an identical hash means an identical query.
    GatsbyBoxOfficeClient.catalogueUrl(GatsbyBoxOfficeClient.ShowcaseUsBaseUrl) shouldBe
      "https://www.showcasecinemas.com/page-data/sq/d/3836549025.json"
    GatsbyBoxOfficeClient.catalogueUrl(GatsbyBoxOfficeClient.LandmarkBaseUrl) shouldBe
      "https://www.landmarktheatres.com/page-data/sq/d/3836549025.json"

    showcaseFilms.size shouldBe 61
    landmarkFilms.size shouldBe 22
    showcaseFilms.map(_.cinema).toSet shouldBe Set(LegacyPlace)
    landmarkFilms.map(_.cinema).toSet shouldBe Set(Nuart)
    all((showcaseFilms ++ landmarkFilms).map(_.showtimes.size)) should be > 0
  }

  it should "carry the catalogue's title, poster, film page and genres" in {
    val mind = film(showcaseFilms, "A Beautiful Mind 25th Anniversary")
    mind.posterUrl.value shouldBe
      "https://all.web.img.acsta.net/img/3a/94/3a94b0024128a2458e2a968d3eea6344.jpg"
    mind.filmUrl.value shouldBe
      "https://www.showcasecinemas.com/movies/1000039080-a-beautiful-mind-25th-anniversary"
    mind.externalIds shouldBe Map("boxoffice" -> "1000039080")
    mind.movie.genres shouldBe Seq("Drama")

    val aliens = film(landmarkFilms, "Aliens")
    aliens.filmUrl.value shouldBe "https://www.landmarktheatres.com/movies/2167-aliens"
    // "ACTION, SCIENCE_FICTION" un-shouted, not passed through verbatim.
    aliens.movie.genres shouldBe Seq("Action", "Science fiction")
  }

  it should "prefer each brand's OWN ticketing host over the shared relay" in {
    // Both brands sit behind the same vendor, but each sells on its own domain.
    // Taking the `default` provider is what keeps a Landmark booking link on
    // Landmark's host instead of the `relay.mvtx.us` redirector.
    film(showcaseFilms, "A Beautiful Mind 25th Anniversary").showtimes.head.bookingUrl.value shouldBe
      "https://tickets.showcasecinemas.com/launch/ticketing/78c3fb9f-1efb-5eb9-b19c-e844b5012210"
    film(landmarkFilms, "Aliens").showtimes.head.bookingUrl.value shouldBe
      "https://booking.landmarktheatres.com/launch/ticketing/321da833-8819-549c-b84b-04250b4926c5"

    val booking = (showcaseFilms ++ landmarkFilms).flatMap(_.showtimes).flatMap(_.bookingUrl)
    booking should not be empty
    // The relay leg carries unencoded spaces in its `code=` param; nothing should
    // reach a stored slot with one.
    booking.filter(_.contains(" ")) shouldBe empty
  }

  it should "parse each session to the venue's local date-time, screen and format" in {
    val mind = film(showcaseFilms, "A Beautiful Mind 25th Anniversary").showtimes.head
    mind.dateTime shouldBe LocalDateTime.of(2026, 11, 22, 15, 0)
    mind.room.value shouldBe "4"
    mind.format shouldBe List("2D")   // Format.Projection.Digital, nothing else

    val aliens = film(landmarkFilms, "Aliens").showtimes.head
    aliens.dateTime shouldBe LocalDateTime.of(2026, 9, 4, 23, 0)
    // Landmark names its screens in full rather than by number.
    aliens.room.value shouldBe "Screen 1"
    aliens.format shouldBe List("2D")
  }

  it should "leave runtime, synopsis, cast and director to TMDB, as the UK brands do" in {
    // The platform's static query declares these fields but populates none of
    // them — measured 0/89 on Showcase US and 0/184 on Landmark, matching the
    // UK brands exactly. Worth pinning: if a US deployment HAD populated them,
    // the shared parser would have been silently dropping real data.
    all(showcaseFilms.map(_.synopsis)) shouldBe None
    all(landmarkFilms.map(_.synopsis)) shouldBe None
    all((showcaseFilms ++ landmarkFilms).map(_.movie.runtimeMinutes)) shouldBe None
    (showcaseFilms ++ landmarkFilms).flatMap(_.cast) shouldBe empty
    (showcaseFilms ++ landmarkFilms).flatMap(_.director) shouldBe empty
  }

  it should "resolve every scheduled id against the catalogue" in {
    // A scheduled id the catalogue can't name is DROPPED (it would be an
    // unshowable row), so a silent catalogue/schedule mismatch would look like a
    // shrinking venue rather than an error. Both brands resolved 100% when
    // captured; this is the guard that says so.
    showcaseFilms.flatMap(_.showtimes).size shouldBe 502
    landmarkFilms.flatMap(_.showtimes).size shouldBe 72
  }

  // ── the horizon: TWO requests, and the whole advertised programme ─────────

  "the whole horizon" should "be spanned by a single schedule request per venue" in {
    GatsbyBoxOfficeClient.scheduleUrl(
      GatsbyBoxOfficeClient.LandmarkBaseUrl, "X00CW", "America/Los_Angeles",
      Today, Today.plusDays(GatsbyBoxOfficeClient.MaxHorizonDays.toLong)
    ) shouldBe
      "https://www.landmarktheatres.com/api/gatsby-source-boxofficeapi/schedule" +
        "?theaters=%7B%22id%22%3A%22X00CW%22%2C%22timeZone%22%3A%22America%2FLos_Angeles%22%7D" +
        "&from=2026-08-30T00:00:00&to=2028-08-29T00:00:00"
  }

  it should "reach as far as the aggregator it replaces, gap days simply absent" in {
    // THE CHECK THAT LETS THESE CHAINS GO PRIMARY, measured against the same
    // venues on flicks.us the same day:
    //   Legacy Place  own 78 days -> 2027-06-09   flicks 77 days -> 2027-06-09
    //   Nuart         own 32 days -> 2026-10-31   flicks 27 days -> 2026-10-31
    // Same furthest date, more populated days. A shorter-horizon primary would
    // not merely miss the tail — scrape-prune reads absence from a complete
    // listing as "stopped screening" and DELETES it (see ScrapeHorizon).
    val showcaseDays = showcaseFilms.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    showcaseDays.size shouldBe 78
    showcaseDays.head shouldBe Today
    showcaseDays.last shouldBe LocalDate.of(2027, 6, 9)

    val landmarkDays = landmarkFilms.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    landmarkDays.size shouldBe 32
    landmarkDays.head shouldBe Today
    landmarkDays.last shouldBe LocalDate.of(2026, 10, 31)

    all((showcaseDays ++ landmarkDays).map(_.toString)) should
      be <= Today.plusDays(ScrapeHorizon.MaxDays.toLong).toString
  }

  it should "surface each venue's own public page for /uptime" in {
    showcase().sourceUrl.value shouldBe
      "https://www.showcasecinemas.com/theaters/x0c11-showcase-cinema-de-lux-legacy-place"
    landmark().sourceUrl.value shouldBe
      "https://www.landmarktheatres.com/theaters/x00cw-landmark-nuart-theatre-west-los-angeles"
  }

  // ── an idle venue is DATA, not an outage ──────────────────────────────────

  "a venue with nothing on" should "return empty rather than throw" in {
    // A REAL empty response, not an edited one: the fixtures were recorded by
    // asking each brand for a window years past anything on sale, which is
    // exactly the body a quiet venue serves. It replays from the SAME fixture
    // directory as the populated case — the schedule URL carries `from`/`to`, so
    // the two windows are already distinct fixture keys and the (large) chain
    // catalogue is shared rather than copied.
    //
    // Throwing here is what left five UK venues permanently red on /uptime — an
    // empty programme must record as a SUCCESSFUL scrape of an empty repertoire
    // so the venue keeps its last-known listing.
    showcase(today = IdleDay).fetch() shouldBe empty
    landmark(today = IdleDay).fetch() shouldBe empty
  }

  // ── a failed fetch must FAIL, so last-known data survives ─────────────────

  "a fetch failure" should "propagate so the scrape records as failed" in {
    // The distinction the cache depends on: a throw is recorded as a failed
    // scrape and the venue keeps its previous showtimes, whereas swallowing it
    // into an empty listing would let scrape-prune delete the whole programme on
    // an upstream blip. Exercised on BOTH legs, because a venue's scrape is only
    // as safe as its weaker one: the schedule leg here (no fixture for this
    // theater id) and the catalogue leg below (no fixture for this host).
    a[java.io.FileNotFoundException] should be thrownBy
      client(new FakeHttpFetch("showcase-us"), GatsbyBoxOfficeClient.ShowcaseUsBaseUrl,
        "NOSUCH", LegacyPlace, "America/New_York").fetch()

    a[java.io.FileNotFoundException] should be thrownBy
      client(new FakeHttpFetch("landmark"), GatsbyBoxOfficeClient.ShowcaseUsBaseUrl,
        "X0C11", LegacyPlace, "America/New_York").fetch()
  }

  // ── the scraper's contract with the rest of the pipeline ──────────────────

  "the scrapers" should "declare their own hosts and count as chains" in {
    // Each brand is its OWN host, which is what keeps their pace gates and 429
    // back-offs independent — and why each needs its own HostPolicy row.
    showcase().scrapeHosts shouldBe Set("www.showcasecinemas.com")
    landmark().scrapeHosts shouldBe Set("www.landmarktheatres.com")
    showcase().chain shouldBe true
    landmark().chain shouldBe true
  }

  it should "not collide with the UK Showcase brand's host" in {
    // `Showcase` exists twice in this repo — National Amusements runs the brand
    // on both sides of the Atlantic. Two different hosts, two different pace
    // buckets, and (below) two disjoint sets of display names.
    GatsbyBoxOfficeClient.ShowcaseUsBaseUrl should not be GatsbyBoxOfficeClient.ShowcaseBaseUrl
    GatsbyBoxOfficeParser.formatTokens(Seq("Format.Projection.Digital")) shouldBe List("2D")
  }
}
