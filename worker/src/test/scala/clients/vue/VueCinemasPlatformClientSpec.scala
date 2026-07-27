package clients.vue

import clients.tools.FakeHttpFetch
import models.VueCinemasIslington
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{VueCinemasPlatformClient, VueCinemasPlatformParser}
import tools.HttpFetch

import java.time.LocalDateTime
import scala.io.Source

/**
 * Replays the real Vue Cinemas platform films payload recorded live on
 * 2026-07-27 from
 * `https://www.myvue.com/api/microservice/showings/cinemas/10032/films`
 * (London — Islington / Angel Central), plus the real
 * `POST /api/microservice/auth/token` response that mints the session cookie.
 * Fixtures under `test/resources/fixtures/vue/www.myvue.com/...`; no live HTTP.
 */
class VueCinemasPlatformClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val IslingtonCinemaId = "10032"
  private val MyVue             = VueCinemasPlatformClient.MyVueBaseUrl

  private def fixture: String = {
    val src = Source.fromFile(
      "test/resources/fixtures/vue/www.myvue.com/api/microservice/showings/cinemas/10032/films.json")
    try src.mkString finally src.close()
  }

  private val films = VueCinemasPlatformParser.parse(fixture, VueCinemasIslington, MyVue)

  private def showtimes = films.flatMap(_.showtimes)

  // ── the pure parser over the recorded payload ────────────────────────────

  "parse" should "read every film the venue is showing" in {
    films.size shouldBe 37
    films.map(_.cinema).toSet shouldBe Set(VueCinemasIslington)
  }

  it should "carry a film's title, runtime, director, cast, poster, synopsis and page" in {
    val odyssey = films.find(_.movie.title == "The Odyssey").value
    odyssey.movie.runtimeMinutes.value shouldBe 173
    odyssey.director                   shouldBe Seq("Christopher Nolan")
    odyssey.cast                       should contain("Matt Damon")
    odyssey.filmUrl.value              shouldBe "https://www.myvue.com/film/the-odyssey"
    odyssey.posterUrl.value            should startWith("https://www.myvue.com/-/media/vuecinemas/img/")
    odyssey.synopsis.value             should startWith("A new Christopher Nolan film")
    // filmId is the venue-facing booking id; movieXchangeCode the cross-brand uuid.
    odyssey.externalIds("vue")         shouldBe "HO00020282"
    odyssey.externalIds("mxc")         shouldBe "b63dbbaf-6c0e-4622-b9b1-698037e6247e"
  }

  it should "leave releaseYear unset — the API's releaseDate is the local re-release, not the production year" in {
    // "Chicken Run (2000)" is re-run this summer and carries a 2026 releaseDate.
    films.find(_.movie.title == "Chicken Run (2000)").value.movie.releaseYear shouldBe None
  }

  it should "surface originalTitle only when it differs from the decorated run's title" in {
    films.find(_.movie.title == "Chicken Run (2000)").value.movie.originalTitle.value shouldBe "Chicken Run"
    films.find(_.movie.title == "The Odyssey").value.movie.originalTitle              shouldBe None
  }

  it should "parse a session's start time, screen and absolutised booking URL" in {
    val opening = films.find(_.movie.title == "The Odyssey").value.showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 27, 11, 45)).value
    opening.room.value       shouldBe "EPIC"
    // bookingUrl is relative in the payload: /book-tickets/summary/10032/HO00020282/398489
    opening.bookingUrl.value shouldBe "https://www.myvue.com/book-tickets/summary/10032/HO00020282/398489"
  }

  it should "reach months past today in one call" in {
    showtimes.map(_.dateTime).min shouldBe LocalDateTime.of(2026, 7, 27, 10, 0)
    showtimes.map(_.dateTime).max shouldBe LocalDateTime.of(2026, 11, 22, 14, 0)
    showtimes.size                shouldBe 414
  }

  // ── attributes[] → Showtime.format ──────────────────────────────────────

  "format tokens" should "carry the premium-format attributes of the screening" in {
    val epic = films.find(_.movie.title == "The Odyssey").value.showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 27, 11, 45)).value
    epic.format shouldBe List("EPIC", "HDR")
    showtimes.map(_.format) should contain(List("EPIC", "ATMOS", "HDR"))
  }

  it should "tag 3D, open-captioned and audio-described screenings" in {
    val threeD = films.find(_.movie.title == "Spider-Man: Brand New Day").value.showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 29, 18, 15)).value
    threeD.format shouldBe List("3D", "LASER", "AD")

    val openCaptioned = films.find(_.movie.title == "The Odyssey").value.showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 7, 28, 17, 40)).value
    openCaptioned.format shouldBe List("LASER", "OC", "AD")
  }

  it should "drop film-level (Movie) and default-language attributes" in {
    val tokens = showtimes.flatMap(_.format).toSet
    // "Strobe Lighting" / "Family" are Movie attributes — same on every row.
    tokens should contain noElementsOf Set("STROBE FX", "FAMILY", "storbe-fx", "family")
    // Every UK row is in the venue's default language, so no language token at all.
    tokens should not contain "OmU"
    tokens shouldBe Set("EPIC", "3D", "ATMOS", "HDR", "LASER", "OC", "AD")
    // Tokens are individual, never glued with a separator (FormatTokensSpec's rule).
    tokens.foreach(_ should not include "/")
  }

  // ── the same class, a second brand: base URL is the only difference ──────

  "the base URL parameter" should "absolutise booking links against the brand being scraped" in {
    // CinemaxX Germany runs the identical backend on a different host; the same
    // parser must hang the same relative booking path off cinemaxx.de.
    val german = VueCinemasPlatformParser.parse(fixture, VueCinemasIslington,
      VueCinemasPlatformClient.CinemaxXBaseUrl)
    german.flatMap(_.showtimes).flatMap(_.bookingUrl).head should startWith("https://www.cinemaxx.de/")
  }

  "the endpoint builders" should "hang both microservice paths off the brand's base URL" in {
    VueCinemasPlatformClient.filmsUrlFor(MyVue, IslingtonCinemaId) shouldBe
      "https://www.myvue.com/api/microservice/showings/cinemas/10032/films"
    VueCinemasPlatformClient.tokenUrlFor(VueCinemasPlatformClient.CinemaxXBaseUrl) shouldBe
      "https://www.cinemaxx.de/api/microservice/auth/token"
    VueCinemasPlatformClient.cinemasUrlFor(MyVue) shouldBe
      "https://www.myvue.com/api/microservice/showings/cinemas"
  }

  // ── fetch(): token bootstrap + parse ────────────────────────────────────

  private def client(http: HttpFetch) =
    new VueCinemasPlatformClient(http, MyVue, IslingtonCinemaId, VueCinemasIslington)

  "fetch" should "return the venue's films when the session cookie is already warm" in {
    val warm   = new CountingFetch(new FakeHttpFetch("vue"), failFirstGet = false)
    val movies = client(warm).fetch()

    movies.map(_.movie.title) should contain("The Odyssey")
    movies.size shouldBe 37
    warm.gets shouldBe 1
    warm.posts shouldBe 0   // no pointless token POST when the jar already has the cookie
  }

  it should "mint a token via POST and retry once when the films call is rejected" in {
    // Cold connection: the films GET 401s until POST /auth/token sets the cookie.
    val cold   = new CountingFetch(new FakeHttpFetch("vue"), failFirstGet = true)
    val movies = client(cold).fetch()

    movies.size shouldBe 37
    cold.posts       shouldBe 1                       // once per fetch(), never per film
    cold.postedUrls  shouldBe Seq("https://www.myvue.com/api/microservice/auth/token")
    cold.gets        shouldBe 2                       // rejected attempt + the retry
  }

  it should "declare its scrape host and count as a chain" in {
    val scraper = client(new FakeHttpFetch("vue"))
    scraper.scrapeHosts shouldBe Set("www.myvue.com")
    scraper.chain       shouldBe true
    scraper.cinema      shouldBe VueCinemasIslington
  }

  /** Counts calls and optionally rejects the first GET the way the live API does
   *  without a `microservicesToken` cookie (HTTP 401 → the fetch throws). */
  private class CountingFetch(delegate: HttpFetch, failFirstGet: Boolean) extends HttpFetch {
    var gets  = 0
    var posts = 0
    var postedUrls: Seq[String] = Seq.empty

    def get(url: String): String = {
      gets += 1
      if (failFirstGet && gets == 1) throw new java.io.IOException(s"HTTP 401 for $url")
      delegate.get(url)
    }

    def post(url: String, body: String, contentType: String): String = {
      posts += 1
      postedUrls = postedUrls :+ url
      delegate.post(url, body, contentType)
    }
  }
}
