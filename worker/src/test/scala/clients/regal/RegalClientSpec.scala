package clients.regal

import clients.tools.{FailingHttpFetch, UrlFragmentHttpFetch}
import models.{Cinema, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.us.{RegalClient, RegalParser, RegalVenues}

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/**
 * Replays REAL recorded Regal `getShowtimes` / `Movies` responses (captured
 * 2026-08-30) through the pure parsers and the client's URL building.
 *
 * The fixtures, and why each exists:
 *   - `getShowtimes-batch-2026-09-12.json` — the 80-theatre batch that venue 1438
 *     rides in, on one date. The normal case, and the one that pins that a venue
 *     reads its OWN slice out of a shared response.
 *   - `getShowtimes-index.json` — a dateless call, which answers with
 *     `datesWithShows`: the day list `planChunks` is built from.
 *   - `getShowtimes-idle-2027-05-14.json` — a real venue on a real date it has
 *     NOTHING on. HTTP 200, `shows: []`. Must parse to EMPTY, never throw.
 *   - `Movies-HO00021207.json` — one film's detail.
 */
class RegalClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def fixture(name: String): String = {
    val src = Source.fromFile(s"test/resources/fixtures/regal/www.regmovies.com/api/$name")
    try src.mkString finally src.close()
  }

  private val batchBody = fixture("getShowtimes-batch-2026-09-12.json")
  private val indexBody = fixture("getShowtimes-index.json")
  private val idleBody  = fixture("getShowtimes-idle-2027-05-14.json")
  private val detailBody = fixture("Movies-HO00021207.json")

  private val NorthHollywood = "1438"
  private val AikenMall      = "0252"
  private val today          = LocalDate.of(2026, 8, 30)

  private def cinemaFor(slug: String): Cinema =
    UsRoster.flicksSlugByCinema.collectFirst { case (cinema, `slug`) => cinema }.value

  private val northHollywood = cinemaFor("regal-north-hollywood")

  // ── the venue map ────────────────────────────────────────────────────────

  "RegalVenues" should "map 401 of our 408 Regal venues to a theatre code" in {
    RegalVenues.theatreCodeBySlug should have size 401
    RegalVenues.theatreCodeBySlug("regal-north-hollywood") shouldBe NorthHollywood
    // Every code is distinct — two venues sharing one would silently merge their
    // listings, since a venue reads its slice of a shared response BY code.
    RegalVenues.theatreCodeBySlug.values.toSeq.distinct should have size 401
  }

  it should "cut the codes into balanced batches that partition the estate" in {
    RegalVenues.batches.flatten.sorted shouldBe RegalVenues.allCodes
    RegalVenues.batches.flatten.distinct should have size RegalVenues.allCodes.size
    // Balanced, not `grouped`: no batch spends a whole request on one venue.
    RegalVenues.batches.map(_.size).min should be > 1
    RegalVenues.batches.map(_.size).max should be <= RegalVenues.BatchSize
  }

  // ── the property the whole design rests on ───────────────────────────────

  it should "give every venue in a batch the IDENTICAL url, so one fetch serves them all" in {
    val batch = RegalVenues.batchFor(NorthHollywood)
    batch.size should be > 1
    val urls = batch.map(code => RegalClient.dayUrl(RegalVenues.batchFor(code), LocalDate.of(2026, 9, 12)))
    // Byte-identical, because the shared Mongo cache keys on the URL: if these
    // differed, each venue would pay its own upstream (Zyte-billed) fetch and the
    // ~24,000-requests-per-sweep shape would be back.
    urls.distinct should have size 1
  }

  // ── url shape ────────────────────────────────────────────────────────────

  "dayUrl" should "send Regal's unpadded M-d-yyyy date and a comma-joined theatre list" in {
    val url = RegalClient.dayUrl(Seq("0252", "1438"), LocalDate.of(2026, 9, 12))
    url shouldBe "https://www.regmovies.com/api/getShowtimes?theatres=0252,1438&date=9-12-2026" +
      "&hoCode=&ignoreCache=false&moviesOnly=false"
    // Not "09-12-2026" — Regal's own front end sends the unpadded form.
    url should include("date=9-12-2026")
  }

  "indexUrl" should "omit the date so the response carries datesWithShows" in {
    RegalClient.indexUrl(Seq("1438")) shouldBe
      "https://www.regmovies.com/api/getShowtimes?theatres=1438&date=&hoCode=&ignoreCache=false&moviesOnly=false"
  }

  // ── parseDates ───────────────────────────────────────────────────────────

  "parseDates" should "read the advertised day list" in {
    val dates = RegalParser.parseDates(indexBody)
    dates should have size 50
    dates.head shouldBe LocalDate.of(2026, 8, 30)
    // Reaches months out — the advance-sale tail no fixed window would cover.
    dates.last shouldBe LocalDate.of(2026, 12, 31)
    dates shouldBe dates.sorted
  }

  it should "throw when the response carries no datesWithShows at all" in {
    // Not "no days" — a body we failed to parse. Returning empty here would
    // narrow every venue in the batch to an empty listing.
    a[IllegalStateException] should be thrownBy RegalParser.parseDates("""{"shows":[]}""")
  }

  // ── parseDay: the populated case ─────────────────────────────────────────

  "parseDay" should "read only this theatre's slice of a shared batch response" in {
    val movies = RegalParser.parseDay(batchBody, NorthHollywood, northHollywood)
    movies should not be empty
    movies.foreach(_.cinema shouldBe northHollywood)
    // The batch carries 80 theatres; this venue must not inherit their films.
    val everyone = RegalParser.parseDay(batchBody, "1025", northHollywood)
    movies.flatMap(_.showtimes).toSet should not be everyone.flatMap(_.showtimes).toSet
  }

  it should "carry each film's title, chain id and detail reference" in {
    val movies = RegalParser.parseDay(batchBody, NorthHollywood, northHollywood)
    val film   = movies.head
    film.movie.title should not be empty
    val code = film.externalIds("regal")
    code should startWith("HO")
    // filmUrl doubles as the detail ref the enricher fetches — see fetchFilmDetail.
    film.filmUrl.value shouldBe s"https://www.regmovies.com/api/Movies?hoCode=$code"
  }

  it should "parse showtimes as local wall-clock times with their auditorium" in {
    val movies = RegalParser.parseDay(batchBody, NorthHollywood, northHollywood)
    val showtimes = movies.flatMap(_.showtimes)
    showtimes should not be empty
    // Every screening lands on the requested calendar date, read off
    // CalendarShowTime (the venue-local field) rather than the UTC instant.
    showtimes.map(_.dateTime.toLocalDate).distinct shouldBe Seq(LocalDate.of(2026, 9, 12))
    // Ordering is per FILM (the parser sorts each film's own showtimes); the
    // flattened cross-film list is deliberately not globally sorted.
    movies.foreach(m => m.showtimes.map(_.dateTime) shouldBe m.showtimes.map(_.dateTime).sorted)
    showtimes.flatMap(_.room) should not be empty
  }

  it should "keep presentation formats and drop the per-screening noise" in {
    val formats = RegalParser.parseDay(batchBody, NorthHollywood, northHollywood)
      .flatMap(_.showtimes).flatMap(_.format).toSet
    // Accessibility / merchandising flags every row carries are not badges.
    formats should not contain "CC"
    formats should not contain "2D"
    formats should not contain "Reserved-Selected"
    formats should not contain "No Passes"
  }

  // ── parseDay: the empty case (the one that left five UK venues red) ──────

  it should "return EMPTY for a venue with nothing on, rather than throwing" in {
    // A real venue on a real date it has no programme: HTTP 200, `shows: []`.
    // An idle venue is DATA, not an outage — throwing here records a failed
    // scrape and lights the venue red on /uptime forever.
    noException should be thrownBy RegalParser.parseDay(idleBody, AikenMall, northHollywood)
    RegalParser.parseDay(idleBody, AikenMall, northHollywood) shouldBe empty
  }

  it should "return EMPTY for a theatre absent from a populated batch response" in {
    // Same rule, different shape: other venues had a programme that day, this one
    // simply has no `shows` row.
    RegalParser.parseDay(batchBody, "9999", northHollywood) shouldBe empty
  }

  it should "throw on a response carrying no shows array at all" in {
    // A broken body must still fail loudly — "empty" is only for a real 200 that
    // says the venue is idle.
    a[IllegalStateException] should be thrownBy
      RegalParser.parseDay("""{"datesWithShows":[]}""", NorthHollywood, northHollywood)
    a[IllegalStateException] should be thrownBy
      RegalParser.parseDay("<html>Attention Required! | Cloudflare</html>", NorthHollywood, northHollywood)
  }

  // ── parseDetail ──────────────────────────────────────────────────────────

  "parseDetail" should "read the fields Regal's listing does not carry" in {
    val detail = RegalParser.parseDetail(detailBody)
    detail.runtimeMinutes.value shouldBe 145
    detail.ageRating.value shouldBe "PG13"
    detail.director shouldBe Seq("Destin Daniel Cretton")
    detail.cast should contain("Tom Holland")
    detail.synopsis.value should include("Peter Parker")
    detail.genres should contain("Action")
  }

  it should "drop the empty strings Regal sends for an absent poster or trailer" in {
    val detail = RegalParser.parseDetail(detailBody)
    // GraphicUrl / TrailerUrl are "" rather than absent on this film; "" is not a URL.
    detail.posterUrl shouldBe None
    detail.trailerUrl shouldBe None
  }

  // ── the client over a stubbed transport ──────────────────────────────────

  private def client(http: tools.HttpFetch) =
    new RegalClient(http, NorthHollywood, northHollywood, today)

  "the client" should "plan its chunks off the batch index and fetch one date's slice" in {
    val http = UrlFragmentHttpFetch(
      "date=&"          -> indexBody,
      "date=9-12-2026"  -> batchBody)
    val regal = client(http)

    val chunks = regal.planChunks()
    chunks.head shouldBe "2026-08-30"
    chunks should contain("2026-09-12")

    regal.fetchChunk("2026-09-12") should not be empty
  }

  it should "fetch its film detail from the ref the listing left on the movie" in {
    val film = RegalParser.parseDay(batchBody, NorthHollywood, northHollywood).head
    val http = UrlFragmentHttpFetch("/api/Movies?hoCode=" -> detailBody)
    client(http).fetchFilmDetail(film.filmUrl.value).value.runtimeMinutes.value shouldBe 145
  }

  it should "PROPAGATE a fetch failure rather than reporting an empty listing" in {
    // Both legs: a failed index must fail the whole scrape, and a failed date must
    // fail that chunk. Swallowing either into an empty list reads as a successful
    // "0 showtimes" scrape and would prune the venue's real listing.
    val regal = client(new FailingHttpFetch())
    a[tools.HttpStatusException] should be thrownBy regal.planChunks()
    a[tools.HttpStatusException] should be thrownBy regal.fetchChunk("2026-09-12")
  }

  it should "declare the host it scrapes, so it is paced and uptime-suppressed" in {
    client(new FailingHttpFetch()).scrapeHosts shouldBe Set("www.regmovies.com")
  }
}
