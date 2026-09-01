package clients.webedia

import clients.tools.FakeHttpFetch
import models.SpanishCinema
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{WebediaMarket, WebediaShowtimesClient}

import java.time.{LocalDate, LocalDateTime}
import scala.io.Source

/**
 * Replays a recorded SensaCine capture (Spain, theater E0291 — Yelmo Cines
 * Premium Parque Corredor, Madrid — date 2026-09-02) through the SAME client
 * Germany runs on, parameterised by `WebediaMarket.Spain`.
 *
 * The point of the spec is the DIFFERENCES, not the shared envelope: the Spanish
 * site returns the identical `results[]` JSON, so everything structural is
 * already covered by `WebediaShowtimesClientSpec`. What is NOT shared is
 * language-shaped, and each piece below fails on the German market:
 *
 *   - the runtime string is "1h 46min", not "1 Std. 46 Min.";
 *   - the certificate arrives as a bare "16", which is an age only with the "+"
 *     a Spanish listing prints in front of it;
 *   - the venue page lives at `/cines/cine/<id>/`, not `/kinoprogramm/kino/<id>/`;
 *   - an original-version screening is `VO`/`VOSE`, not `OV`/`OmU`.
 */
class WebediaSpainShowtimesClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def fixture: String = read(
    "test/resources/fixtures/webedia-es/www.sensacine.com/_/showtimes/theater-E0291/d-2026-09-02/p-1.json")

  /** A real SensaCine venue page (theater E0291, captured 2026-09-01). Read
   *  directly rather than through `FakeHttpFetch`'s URL mapping, exactly as the
   *  German spec does, so the failure tests below still find no venue page under
   *  `webedia-es`. Source URL: https://www.sensacine.com/cines/cine/E0291/ */
  private def venuePage: String = read("test/resources/fixtures/webedia-es/theater-E0291-venue-page.html")

  private def read(path: String): String = {
    val src = Source.fromFile(path)
    try src.mkString finally src.close()
  }

  private val venue = new SpanishCinema("Yelmo Cines Premium Parque Corredor", "Yelmo Parque Corredor")
  private val page  = WebediaShowtimesClient.parsePage(fixture, WebediaMarket.Spain)

  "parsePage" should "read every film and the page count" in {
    page.totalPages shouldBe 1
    page.films.map(_.internalId).distinct.size shouldBe 10
  }

  // THE RUNTIME FORMAT IS THE MARKET SPLIT'S REASON TO EXIST. Spain writes
  // "1h 46min" where Germany writes "1 Std. 46 Min.", so the German pattern reads
  // Spanish runtimes as `None` — every Spanish film would have shipped with no
  // runtime at all, silently, since a missing runtime is a normal thing for a
  // film row to carry.
  it should "read the compact Spanish runtime format" in {
    val insidious = page.films.find(_.title == "Insidious: Fuera del más allá").value
    insidious.runtimeMinutes.value shouldBe 106      // "1h 46min"
    page.films.find(_.title == "Spider-Man: Brand New Day").value
      .runtimeMinutes.value shouldBe 145             // "2h 25min"
    page.films.find(_.title == "La Odisea").value
      .runtimeMinutes.value shouldBe 173             // "2h 53min"
    // Every film in the capture carries one, so a regression cannot hide behind
    // the one title that happens to be missing it.
    all(page.films.map(_.runtimeMinutes)) shouldBe defined
  }

  // The bite, stated as an assertion: run the SAME payload through the German
  // market and every runtime is gone. That is what shipped before the markets
  // were split, and it is why the unit match is case-sensitive — a loose match
  // would have made this read 46 minutes instead of nothing, which is far worse
  // than missing.
  it should "yield no runtime at all when parsed on the German market" in {
    WebediaShowtimesClient.parsePage(fixture, WebediaMarket.Germany)
      .films.flatMap(_.runtimeMinutes) shouldBe empty
  }

  it should "print a numeric Spanish certificate as an age, and leave a worded one alone" in {
    page.films.find(_.title == "Insidious: Fuera del más allá").value.ageRating.value shouldBe "+16"
    page.films.find(_.title == "Spider-Man: Brand New Day").value.ageRating.value shouldBe "+12"
    // "APTA(i)" is already a word ("apta para todos los públicos, especialmente
    // recomendada para la infancia") — prefixing it with "+" would read as an age.
    page.films.find(_.title == "La Patrulla Canina: La Dino película").value
      .ageRating.value shouldBe "APTA(I)"
  }

  it should "carry the international title only when it differs from the Spanish one" in {
    // Asserted on the emitted `CinemaMovie`, not on the raw parse: `parsePage`
    // reports the site's value verbatim, and the drop-when-identical rule is
    // applied when the film row is built.
    val films = fakeClient().fetchChunk("2026-09-02")
    films.find(_.movie.title == "Insidious: Fuera del más allá").value
      .movie.originalTitle.value shouldBe "Insidious: Out of the Further"   // → a TMDB hint
    // A film released under its English title in Spain echoes the same string
    // back, which is not a hint about anything.
    films.find(_.movie.title == "Spider-Man: Brand New Day").value.movie.originalTitle shouldBe None
  }

  it should "carry year, genres, director, poster and synopsis" in {
    val insidious = page.films.find(_.title == "Insidious: Fuera del más allá").value
    insidious.year.value shouldBe 2026
    insidious.genres should contain("Terror")
    insidious.director should contain("Jacob Chase")
    insidious.posterUrl.value should startWith("https://")
    insidious.synopsis.value.length should be > 100
    insidious.synopsis.value should not include "<p"     // HTML flattened, not rendered
  }

  it should "flatten every version bucket into local-time screenings with cleaned booking links" in {
    val insidious = page.films.find(_.title == "Insidious: Fuera del más allá").value
    insidious.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 2, 15, 45),
      LocalDateTime.of(2026, 9, 2, 18, 0),
      LocalDateTime.of(2026, 9, 2, 20, 10),
      LocalDateTime.of(2026, 9, 2, 22, 35),
    )
    // The relay link arrives with a trailing render marker glued on
    // ("…&code=PREMIUM; ESPANOL") — Spain's spelling of the "; SSR" Germany sends.
    val booking = insidious.showtimes.head.bookingUrl.value
    booking should startWith("https://relay.mvtx.us/ticketing/")
    booking should not include " "
    booking should not endWith ";"
  }

  // A dubbed digital screening — which is all this venue served on the captured
  // day — is the UNMARKED case in both markets, so the version tokens below are
  // pinned from the tag combinations directly.
  "formatTokens" should "use the Spanish version abbreviations, not the German ones" in {
    val subtitled = Seq("Localization.Version.Original", "Localization.Subtitle.Spanish")
    val original  = Seq("Localization.Version.Original", "Format.Projection.Digital")
    val dubbed    = Seq("Localization.Version.Spanish", "Format.Projection.Digital")

    WebediaShowtimesClient.formatTokens(subtitled, WebediaMarket.Spain)   shouldBe List("VOSE")
    WebediaShowtimesClient.formatTokens(original,  WebediaMarket.Spain)   shouldBe List("VO")
    WebediaShowtimesClient.formatTokens(dubbed,    WebediaMarket.Spain)   shouldBe Nil
    // The same tags on the German market still read OmU/OV — the tokens are the
    // market's, not the tag vocabulary's.
    WebediaShowtimesClient.formatTokens(subtitled, WebediaMarket.Germany) shouldBe List("OmU")
    WebediaShowtimesClient.formatTokens(original,  WebediaMarket.Germany) shouldBe List("OV")

    WebediaShowtimesClient.formatTokens(
      Seq("Format.Projection.3d", "Localization.Subtitle.Spanish"), WebediaMarket.Spain) shouldBe List("3D", "VOSE")
  }

  "sourceUrl" should "point at the Spanish venue-page path" in {
    clientOver(new ScriptedByUrl(_ => venuePage)).sourceUrl.value shouldBe
      "https://www.sensacine.com/cines/cine/E0291/"
  }

  "planChunks" should "read the venue page's advertised days" in {
    val days = clientOver(new ScriptedByUrl(url =>
      if (url.contains("/cines/cine/")) venuePage
      else throw new java.io.IOException("planChunks must not fetch per-day pages"))).planChunks()

    days should contain("2026-09-02")
    days.size should be >= 14                  // the capture advertises a three-week window
    days shouldBe days.sorted
    all(days.map(LocalDate.parse)) should be >= LocalDate.of(2026, 9, 1)
  }

  private def fakeClient() =
    new WebediaShowtimesClient(
      new FakeHttpFetch("webedia-es"), WebediaMarket.Spain, "E0291", venue,
      today = Some(LocalDate.of(2026, 9, 2)))

  "fetchChunk" should "parse one day's page into that day's films" in {
    val films = fakeClient().fetchChunk("2026-09-02")

    films.map(_.movie.title) should contain("Insidious: Fuera del más allá")
    films.map(_.cinema).toSet shouldBe Set(venue)
    all(films.map(_.externalIds.keySet)) should contain("webedia")
    all(films.flatMap(_.showtimes).map(_.dateTime.toLocalDate)) should be(LocalDate.of(2026, 9, 2))
  }

  private class ScriptedByUrl(respond: String => String) extends tools.GetOnlyHttpFetch {
    def get(url: String): String = respond(url)
  }

  private def clientOver(http: tools.HttpFetch) =
    new WebediaShowtimesClient(http, WebediaMarket.Spain, "E0291", venue,
      today = Some(LocalDate.of(2026, 9, 1)))
}
