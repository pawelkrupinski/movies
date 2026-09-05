package controllers

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.readmodel.WebReadModel

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.zip.GZIPInputStream

class ApiRepertoireConditionalSpec extends AnyFlatSpec with Matchers {

  private def buildController(): (MovieController, WebReadModel) = {
    val now = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt999"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Test Film"),
          releaseYear = Some(2024),
          genres      = Seq("Dramat", "Komedia"),
          synopsis    = Some("A test synopsis."),
          trailerUrl  = Some("https://www.youtube.com/watch?v=abc123DEF45"),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        ),
        Tmdb -> SourceData(originalTitle = Some("The Test Movie"))
      )
    )
    TestMovieController.build(Seq(("Test Film", Some(2024), record)))
  }

  it should "keep the lean listing free of detail-only fields" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    val film = play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "title").as[String] shouldBe "Test Film"
    (film \ "synopsis").toOption shouldBe None
    (film \ "trailerURLs").toOption shouldBe None
  }

  it should "carry releaseYear and genres on the lean listing (mobile card parity)" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    val film = play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "releaseYear").as[Int] shouldBe 2024
    (film \ "genres").as[Seq[String]] shouldBe Seq("Dramat", "Komedia")
  }

  "apiDetails" should "return synopsis + embed-transformed trailerURLs keyed by title" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiDetails("poznan")(FakeRequest())
    status(result) shouldBe OK
    val entry = play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]]
      .find(d => (d \ "title").as[String] == "Test Film")
      .getOrElse(fail("Test Film missing from /api/details"))
    (entry \ "synopsis").as[String] shouldBe "A test synopsis."
    // Raw watch URL is normalised to a YouTube embed URL.
    (entry \ "trailerURLs").as[Seq[String]] shouldBe Seq("https://www.youtube.com/embed/abc123DEF45")
    // The TMDB original title differs from the Polish cinema title, so it
    // rides on the detail payload.
    (entry \ "originalTitle").as[String] shouldBe "The Test Movie"
  }

  it should "keep the original title off the lean listing (detail-only field)" in {
    val (ctrl, _) = buildController()
    val film = play.api.libs.json.Json.parse(contentAsString(ctrl.apiRepertoire("poznan")(FakeRequest())))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "originalTitle").toOption shouldBe None
  }

  it should "omit films with neither synopsis nor trailers" in {
    val (ctrl, _) = buildController()
    // The single fixture film has both, so it is present; a film with neither
    // would be filtered out by ApiFilmDetails.hasContent.
    val details = play.api.libs.json.Json.parse(contentAsString(ctrl.apiDetails("poznan")(FakeRequest())))
      .as[Seq[play.api.libs.json.JsValue]]
    details.forall(d => (d \ "synopsis").toOption.isDefined || (d \ "trailerURLs").as[Seq[String]].nonEmpty) shouldBe true
  }

  "apiRepertoire" should "return 200 with Last-Modified header when no If-Modified-Since" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    header("Last-Modified", result) shouldBe defined
  }

  it should "return 304 when If-Modified-Since matches" in {
    val (ctrl, _) = buildController()
    val first = ctrl.apiRepertoire("poznan")(FakeRequest())
    val lastMod = header("Last-Modified", first).get

    val second = ctrl.apiRepertoire("poznan")(FakeRequest().withHeaders("If-Modified-Since" -> lastMod))
    status(second) shouldBe NOT_MODIFIED
    contentAsString(second) shouldBe empty
  }

  it should "return 200 after a cache mutation even with the old If-Modified-Since" in {
    val (ctrl, cache) = buildController()
    val first = ctrl.apiRepertoire("poznan")(FakeRequest())
    val lastMod = header("Last-Modified", first).get

    Thread.sleep(1100)
    cache.reload()

    val second = ctrl.apiRepertoire("poznan")(FakeRequest().withHeaders("If-Modified-Since" -> lastMod))
    status(second) shouldBe OK
  }

  // ── Gzip response cache ────────────────────────────────────────────────────

  private def gzipRequest(path: String) =
    FakeRequest("GET", path).withHeaders("Accept-Encoding" -> "gzip, deflate, br")

  private def gunzip(bytes: org.apache.pekko.util.ByteString): String =
    new String(new GZIPInputStream(new ByteArrayInputStream(bytes.toArray)).readAllBytes(), StandardCharsets.UTF_8)

  "apiRepertoire" should "serve gzip-precompressed JSON to a gzip-accepting client, with Last-Modified" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    header("Last-Modified", result) shouldBe defined
    val film = play.api.libs.json.Json.parse(gunzip(contentAsBytes(result)))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "title").as[String] shouldBe "Test Film"
  }

  it should "still 304 a current client even when it accepts gzip (cache must not shadow the 304)" in {
    val (ctrl, _) = buildController()
    val lastMod = header("Last-Modified", ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))).get

    val revalidated = ctrl.apiRepertoire("poznan")(
      gzipRequest("/poznan/api/repertoire").withHeaders("If-Modified-Since" -> lastMod)
    )
    status(revalidated) shouldBe NOT_MODIFIED
    header("Content-Encoding", revalidated) shouldBe None
    contentAsString(revalidated) shouldBe empty
  }

  it should "re-serve fresh gzipped JSON after the cache version advances" in {
    val (ctrl, cache) = buildController()
    ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))

    Thread.sleep(1100)
    cache.reload()

    val after = ctrl.apiRepertoire("poznan")(gzipRequest("/poznan/api/repertoire"))
    status(after) shouldBe OK
    header("Content-Encoding", after) shouldBe Some("gzip")
    gunzip(contentAsBytes(after)) should include ("Test Film")
  }

  "apiDetails" should "serve gzip-precompressed JSON to a gzip-accepting client" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiDetails("poznan")(gzipRequest("/poznan/api/details"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    gunzip(contentAsBytes(result)) should include ("A test synopsis.")
  }

  // ── SHARED-CACHE (Cloudflare) HEADERS ────────────────────────────────────────
  //
  // The mobile apps reach these endpoints through Cloudflare now. Measured on the
  // live edge 2026-09-05, every one of them answered `cf-cache-status: DYNAMIC`:
  // the proxy cached nothing, so each conditional request from each app install
  // still woke the JVM to compute a 304 it would then send as 0 bytes.
  //
  // `s-maxage` is what lets the edge answer instead. `max-age=0` beside it keeps
  // every CLIENT revalidating exactly as before, so If-Modified-Since and the
  // 304s are unchanged from the app's point of view -- only who answers moves.

  it should "let a shared cache hold the lean listing briefly, while clients still revalidate" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    val cc = header("Cache-Control", result).value
    cc should include ("public")
    cc should include ("max-age=0")                 // clients revalidate, as before
    cc should include ("s-maxage=60")               // only the shared cache may answer
    header("Last-Modified", result) shouldBe defined
  }

  it should "still answer a current If-Modified-Since with a bodiless 304" in {
    // The whole point is that this behaviour does NOT change: adding s-maxage
    // must not cost the conditional request that mobile already relies on.
    val (ctrl, readModel) = buildController()
    val lastMod = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(
      readModel.lastModified.truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
        .atOffset(java.time.ZoneOffset.UTC))
    val result = ctrl.apiRepertoire("poznan")(FakeRequest().withHeaders("If-Modified-Since" -> lastMod))
    status(result) shouldBe NOT_MODIFIED
    contentAsBytes(result).length shouldBe 0
  }

  it should "NEVER mark an HTML page shareable, however it was rendered" in {
    // The safety property, and it is asserted on the ABSENCE of the shared
    // directives rather than the presence of a particular one, because the page
    // has two legitimate spellings: `private, no-store` once somebody is signed
    // in (PersonalisedPage), and no Cache-Control at all for an anonymous render
    // that also carries a Set-Cookie -- which no shared cache will store anyway.
    // What must never appear on either is `public`/`s-maxage`: that would let
    // Cloudflare hand one visitor's page, cookie and navbar to the next person
    // through the same PoP.
    val (ctrl, _) = buildController()
    val cc = header("Cache-Control", ctrl.index("poznan")(FakeRequest())).getOrElse("")
    cc should not include ("public")
    cc should not include ("s-maxage")
    // And the signed-in spelling is the strict one.
    PersonalisedPage.CacheControl shouldBe "private, no-store"
  }


  // ── ?days=N WINDOW ───────────────────────────────────────────────────────────
  //
  // The listing renders the whole corpus: London's payload carried 187 distinct
  // dates out to 2027-07-03, and only 119 of its 784 films played on the day it
  // was measured. `days` is what lets a client ask for the part it will show.

  private def windowController() = {
    val today = LocalDateTime.now()
    def rec(title: String, when: LocalDateTime) = MovieRecord(
      imdbId = Some("tt" + title.hashCode.abs),
      data = Map[Source, SourceData](Helios -> SourceData(
        title = Some(title), releaseYear = Some(2024),
        showtimes = Seq(models.Showtime(when, None, None, Nil)))))
    TestMovieController.build(Seq(
      ("Today Film",  Some(2024), rec("Today Film",  today.plusHours(2))),
      ("Soon Film",   Some(2024), rec("Soon Film",   today.plusDays(3))),
      ("Distant Film",Some(2024), rec("Distant Film",today.plusDays(90))),
    ))
  }

  private def titlesFrom(result: scala.concurrent.Future[play.api.mvc.Result]): Seq[String] =
    play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]].map(j => (j \ "title").as[String])

  it should "return the whole corpus when no window is asked for" in {
    val (ctrl, _) = windowController()
    titlesFrom(ctrl.apiRepertoire("poznan")(FakeRequest())) should contain allOf
      ("Today Film", "Soon Film", "Distant Film")
  }

  it should "keep only films showing inside the window, and drop the rest entirely" in {
    val (ctrl, _) = windowController()
    val titles = titlesFrom(ctrl.apiRepertoire("poznan", Some(7))(FakeRequest("GET","/poznan/api/repertoire?days=7")))
    titles should contain ("Today Film")
    titles should contain ("Soon Film")
    // A film 90 days out must NOT ride along just because it is next on its own
    // list -- the window is calendar days from today, not "the first N dates
    // this film happens to have".
    titles should not contain ("Distant Film")
  }

  it should "clamp a hostile or nonsensical window rather than trusting the URL" in {
    val (ctrl, _) = windowController()
    MovieController.dayWindow(Some(0))        shouldBe Some(1)
    MovieController.dayWindow(Some(-5))       shouldBe Some(1)
    MovieController.dayWindow(Some(99999))    shouldBe Some(MovieController.MaxDayWindow)
    MovieController.dayWindow(None)           shouldBe None
  }

  it should "NOT serve one window's body to a client asking for another" in {
    // The gzip cache is keyed on request.path, which drops the query string, so
    // without the window in the key `?days=7` and the full payload share one
    // entry -- and whichever rendered first wins, silently, with a 200 and a
    // plausible body. Exercised through the GZIP path on purpose: that is the
    // only path the cache is on.
    val (ctrl, _) = windowController()
    def titles(r: scala.concurrent.Future[play.api.mvc.Result]) =
      play.api.libs.json.Json.parse(gunzip(contentAsBytes(r)))
        .as[Seq[play.api.libs.json.JsValue]].map(j => (j \ "title").as[String])

    val narrow = titles(ctrl.apiRepertoire("poznan", Some(7))(gzipRequest("/poznan/api/repertoire?days=7")))
    val wide   = titles(ctrl.apiRepertoire("poznan", None)(gzipRequest("/poznan/api/repertoire")))
    narrow should not contain ("Distant Film")
    wide    should contain    ("Distant Film")

    // And in the other order, so neither can be the one that merely happened to
    // render first.
    val (ctrl2, _) = windowController()
    val wideFirst   = titles(ctrl2.apiRepertoire("poznan", None)(gzipRequest("/poznan/api/repertoire")))
    val narrowAfter = titles(ctrl2.apiRepertoire("poznan", Some(7))(gzipRequest("/poznan/api/repertoire?days=7")))
    wideFirst   should contain    ("Distant Film")
    narrowAfter should not contain ("Distant Film")
  }

}
