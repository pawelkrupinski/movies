package clients.cineworld

import clients.tools.FakeHttpFetch
import models.CineworldSheffield
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.uk.{CineworldClient, CineworldParser}

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded Cineworld `quickbook` responses for Sheffield (site
 *  code 031, captured 2026-07-27) — the chain roster, the venue's day list, and
 *  two days of `film-events` — and pins what the client makes of them: which
 *  days it plans, the films a day's `events[]`→`films[]` join produces, their
 *  showtimes / booking deep-links / screen / premium-format badges, and the
 *  cross-day merge. No live HTTP: everything resolves out of
 *  `test/resources/fixtures/cineworld/`. */
class CineworldClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 7, 27)
  private val Sheffield = "031"
  private val fake   = new FakeHttpFetch("cineworld")

  private def client(http: tools.HttpFetch = fake) =
    new CineworldClient(http, Sheffield, CineworldSheffield, today)

  // ── the chain roster CINEWORLD-VENUE-MAP.tsv is built from ────────────────
  "parseVenues" should "read every venue in the chain roster with its id and name" in {
    val venues = CineworldParser.parseVenues(fake.get(CineworldClient.venuesUrl(LocalDate.of(2026, 8, 31))))
    venues.size shouldBe 87
    val sheffield = venues.find(_.id == Sheffield).value
    sheffield.displayName shouldBe "Sheffield"
    sheffield.link.value shouldBe "https://www.cineworld.co.uk/cinemas/sheffield"
    venues.map(_.id).distinct.size shouldBe venues.size
  }

  // ── the horizon: days come off the venue's own `dates` endpoint ───────────
  // The venue's day list is the horizon — we plan EVERY day it advertises, not a
  // window of it. Sheffield (recorded 2026-07-27) publishes a dense five-week block
  // and then a sparse advance-sale tail of one-off event days running to 2027-04-22:
  // Met Opera, RBO season, NT Live, anniversary re-releases.
  //
  // Capping this at 35 days is what deleted them from prod. The cap kept the tail out
  // of the listing, and `MovieCache`'s scrape-prune reads "absent from the listing" as
  // "stopped screening" — so every complete scrape dropped the venue's whole advance
  // programme. Measured on 2026-07-27: ZERO Cineworld showtimes survived beyond 36 days.
  "planChunks" should "plan every day the venue advertises, including the advance-sale tail" in {
    val days = client().planChunks()
    days.size shouldBe 55
    days.head shouldBe "2026-07-27"
    days.last shouldBe "2027-04-22"
    days shouldBe days.sorted
    // the tail specifically — the part a 35-day cap would have cut
    days.count(_ > "2026-08-31") shouldBe 19
  }

  it should "keep a far-out advance-sale day, and drop only days already past" in {
    val strays = scripted(_ =>
      """{"body":{"dates":["2026-07-20","2026-07-27","2026-08-31","2027-01-04"]}}""")
    // 2026-07-20 is yesterday's programme — nothing upcoming there. 2027-01-04 is a real
    // advance-sale day and must be planned.
    client(strays).planChunks() shouldBe Seq("2026-07-27", "2026-08-31", "2027-01-04")
  }

  it should "ignore a date beyond the sanity horizon (a garbage far date can't fan out forever)" in {
    val silly = scripted(_ => """{"body":{"dates":["2026-07-27","2099-01-01"]}}""")
    client(silly).planChunks() shouldBe Seq("2026-07-27")
  }

  it should "return empty (not throw) for a venue with nothing on" in {
    client(scripted(_ => """{"body":{"dates":[]}}""")).planChunks() shouldBe empty
  }

  it should "THROW when the response carries no day list at all" in {
    // Not "no programme" — a body we failed to parse (an error page, a shape
    // change). Failing the scrape keeps the venue's last-known listing.
    an[IllegalStateException] should be thrownBy
      client(scripted(_ => """{"body":{}}""")).planChunks()
  }

  // ── one day: events[] joined to films[] ──────────────────────────────────
  private val day = client().fetchChunk("2026-07-27")

  "fetchChunk" should "produce one film per id that has screenings that day" in {
    day.size shouldBe 20
    day.map(_.cinema).toSet shouldBe Set(CineworldSheffield)
    day.map(_.movie.title) should contain("Toy Story 5")
  }

  it should "carry the film's title, runtime, page, poster, trailer and Cineworld id" in {
    val toyStory = day.find(_.movie.title == "Toy Story 5").value
    toyStory.movie.runtimeMinutes.value shouldBe 102
    toyStory.filmUrl.value shouldBe "https://www.cineworld.co.uk/films/toy-story-5/ho00014533"
    toyStory.posterUrl.value should include("regalcdn.azureedge.net")
    toyStory.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=UV7Ht-R3ICY"
    toyStory.externalIds shouldBe Map("cineworld" -> "ho00014533")
    // Genres are one slice of the film's kitchen-sink attributeIds
    // ('2d','3d','4dx','action','animation','audio-described','laser','pg',
    // 'reserved-selected'); the BBFC rating is lifted out separately (below) and
    // formats / seating drop.
    toyStory.movie.genres shouldBe Seq("Action", "Animation")
  }

  it should "lift the BBFC certificate out of attributeIds into ageRating" in {
    day.find(_.movie.title == "Toy Story 5").value.ageRating.value shouldBe "PG"
    day.find(_.movie.title == "£2 Family Films : Chicken Run (2026 Re-Release)").value.ageRating.value shouldBe "U"
    day.find(_.movie.title == "The Odyssey (2026)").value.ageRating.value shouldBe "15"
    day.find(_.movie.title == "Evil Dead Burn").value.ageRating.value shouldBe "18"
    day.find(_.movie.title == "Brunello: The Gracious Visionary").value.ageRating.value shouldBe "12A"
    // 'tbc' (rating pending) is not a certificate → no ageRating.
    day.find(_.movie.title == "Animal Farm (2025)").value.ageRating shouldBe None
  }

  it should "leave releaseYear unset — the API's is the UK re-release year" in {
    // "Scorsese Season: Raging Bull (1980)" ships releaseYear 2026 here.
    day.find(_.movie.title == "Scorsese Season: Raging Bull (1980)").value.movie.releaseYear shouldBe None
    day.flatMap(_.movie.releaseYear) shouldBe empty
  }

  it should "map each event to a showtime with its booking deep-link and screen" in {
    val toyStory = day.find(_.movie.title == "Toy Story 5").value
    toyStory.showtimes.size shouldBe 13
    toyStory.showtimes.map(_.dateTime) shouldBe toyStory.showtimes.map(_.dateTime).sorted
    val tenOClock = toyStory.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 10, 0)).value
    tenOClock.bookingUrl.value shouldBe
      "https://experience.cineworld.co.uk/select-tickets?sitecode=031&site=031&id=456974&lang=en"
    tenOClock.room.value shouldBe "Screen 9"
  }

  it should "badge the premium formats and drop the noise attributes" in {
    val toyStory = day.find(_.movie.title == "Toy Story 5").value
    // 10:00 is the 4DX 3D screening; 09:00 is a plain 2D one in the same film.
    toyStory.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 10, 0)).value.format shouldBe
      List("4DX", "3D")
    toyStory.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 9, 0)).value.format shouldBe
      List("2D")
    // IMAX rides the Odyssey's Screen 7 sessions; 'laser' / 'recliner' /
    // 'audio-described' / 'reserved-selected' never become badges.
    val odyssey = day.find(_.movie.title == "The Odyssey (2026)").value
    odyssey.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 11, 20)).value.format shouldBe
      List("IMAX", "2D")
    // TAMIL / TELUGU join the set off the day's foreign-language screenings.
    day.flatMap(_.showtimes).flatMap(_.format).toSet shouldBe
      Set("2D", "3D", "IMAX", "4DX", "NAP", "TAMIL", "TELUGU")
  }

  it should "mark a subtitled screening" in {
    val odyssey = day.find(_.movie.title == "The Odyssey (2026)").value
    odyssey.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 18, 10)).value.format shouldBe
      List("2D", "NAP")
  }

  it should "badge the spoken language of a foreign-language screening" in {
    // "Jana Nayagan (Tamil)" is subtitled Tamil — its attributeIds carry 'tamil'
    // alongside 'subbed', so the badge reads "TAMIL" (what it's in) after the
    // "NAP" subtitle marker, with the language ids never leaking as genres.
    val janaNayagan = day.find(_.movie.title == "Jana Nayagan (Tamil)").value
    janaNayagan.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 27, 10, 40)).value.format shouldBe
      List("2D", "NAP", "TAMIL")
  }

  // ── fetch(): planChunks → fetchChunk → reduceChunks, in process ───────────
  "fetch" should "merge every planned day into one row per film" in {
    // Only the two recorded days are scripted into the day list; the per-day
    // film-events calls resolve out of the fixture corpus.
    val twoDays = scripted(url =>
      if (url.contains("/dates/")) """{"body":{"dates":["2026-07-27","2026-07-28"]}}"""
      else fake.get(url))

    val movies   = client(twoDays).fetch()
    val toyStory = movies.find(_.movie.title == "Toy Story 5").value
    movies.size shouldBe 22                                   // 20 films a day, 22 across the two
    toyStory.showtimes.size shouldBe 26                       // 13 per day, unioned
    toyStory.showtimes.map(_.dateTime.toLocalDate).distinct shouldBe
      Seq(LocalDate.of(2026, 7, 27), LocalDate.of(2026, 7, 28))
    toyStory.showtimes.map(_.dateTime) shouldBe toyStory.showtimes.map(_.dateTime).sorted
  }

  "sourceUrl" should "link the venue's public page off the site code alone" in {
    client().sourceUrl.value shouldBe "https://www.cineworld.co.uk/cinemas/031/031"
  }

  it should "declare the chain and the host it scrapes" in {
    client().chain shouldBe true
    client().scrapeHosts shouldBe Set("www.cineworld.co.uk")
  }

  // ── the deferred DetailEnricher: the film's own /films/<slug>/<id> page ──────
  "fetchFilmDetail" should "parse synopsis, cast, director, runtime and cert off the film page" in {
    // The `filmUrl` the listing scrape leaves on the movie IS the detail ref.
    val ref    = day.find(_.movie.title == "Toy Story 5").value.filmUrl.value
    val detail = client().fetchFilmDetail(ref).value
    detail.synopsis.value should include("Toy Story 5")
    detail.director should contain("Andrew Stanton")
    detail.cast should contain allOf ("Tom Hanks", "Tim Allen")
    detail.runtimeMinutes.value shouldBe 102
    detail.ageRating.value shouldBe "PG"
  }

  it should "advertise the chain-wide detail group" in {
    client().detailGroup shouldBe "cineworld"
  }

  private def scripted(respond: String => String): tools.HttpFetch = new tools.GetOnlyHttpFetch {
    def get(url: String): String = respond(url)
  }
}
