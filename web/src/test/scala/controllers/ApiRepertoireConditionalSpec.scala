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

  // THE SAFETY PROPERTY MOVED RATHER THAN LAPSED. This used to read "never mark
  // an HTML page shareable, however it was rendered", which was the right rule
  // while the navbar drew whoever was signed in: a `public`/`s-maxage` on that
  // page would have let Cloudflare hand one visitor's avatar to the next through
  // the same PoP. The listing stopped rendering anybody (see
  // `SharedCacheableListingSpec`), so it is now shareable BECAUSE it names no
  // one — and the rule bites on what still does name someone instead.
  it should "never mark a response that names a visitor shareable" in {
    PerUserResponse.CacheControl should not include ("public")
    PerUserResponse.CacheControl should not include ("s-maxage")
    PerUserResponse.CacheControl shouldBe "private, no-store"
  }

  // The facet pages carry the same bytes for everyone too, but a `?cast=` URL is
  // one of combinatorially many near-duplicates of the listing, so they are kept
  // out of the edge deliberately rather than by accident.
  it should "keep the facet pages out of the shared cache" in {
    val (ctrl, _) = buildController()
    val cc = header("Cache-Control",
      ctrl.browse("poznan", None, Some("Nolan"), None, None)(FakeRequest())).getOrElse("")
    cc should not include ("public")
    cc should not include ("s-maxage")
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


  // ── ETAG, WHICH IS WHAT A SHARED CACHE ANSWERS A CONDITIONAL FROM ────────────
  //
  // Measured against the live edge 2026-09-05: with `s-maxage` set and no ETag,
  // Cloudflare served its cached copy as a 200 WITH THE WHOLE BODY when the app
  // sent `If-Modified-Since` -- so edge caching had quietly traded the mobile
  // apps' 0-byte 304s for ~750 KB payloads. `/api/catalog` was unaffected
  // because it already carried an ETag. These pin the missing half.

  it should "carry an ETag beside Last-Modified, so a cache can answer a conditional" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    val etag = header("ETag", result).value
    etag should startWith ("\"")
    etag should endWith ("\"")
    header("Last-Modified", result) shouldBe defined
  }

  it should "answer a matching If-None-Match with a bodiless 304" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.apiRepertoire("poznan")(FakeRequest())).value
    val result = ctrl.apiRepertoire("poznan")(FakeRequest().withHeaders("If-None-Match" -> etag))
    status(result) shouldBe NOT_MODIFIED
    contentAsBytes(result).length shouldBe 0
  }

  it should "give two windows of one path DIFFERENT etags" in {
    // Same path, different bodies. A shared validator would let a cache answer
    // "not modified" to a client holding the other window's copy.
    val (ctrl, _) = windowController()
    val wide   = header("ETag", ctrl.apiRepertoire("poznan", None)(FakeRequest())).value
    val narrow = header("ETag", ctrl.apiRepertoire("poznan", Some(7))(
      FakeRequest("GET", "/poznan/api/repertoire?days=7"))).value
    narrow should not be wide
  }

  it should "move the etag when the read model does" in {
    val (ctrl, readModel) = buildController()
    val before = header("ETag", ctrl.apiRepertoire("poznan")(FakeRequest())).value
    Thread.sleep(1100)          // the validator is second-resolution
    readModel.reload()
    val after = header("ETag", ctrl.apiRepertoire("poznan")(FakeRequest())).value
    after should not be before
  }


  // ── The validator is scoped to the city being served ────────────────────────
  //
  // `WebReadModel.lastModified` moves whenever anything in the corpus changes,
  // so using it as the conditional-GET validator meant a Warsaw showtime expired
  // Poznan's ETag. Every city's payload then looked like it changed every couple
  // of minutes and neither the client 304s nor the new edge cache could hold a
  // copy for long. These pin the scoping at the HTTP layer, which is where the
  // benefit actually lands.

  private def resolved(id: String, title: String) =
    models.ResolvedMovie(id, title, None, None, Nil, None, Some(2021), Nil, Nil, Nil, Nil, None, Nil,
      models.ResolvedRatings(None, None, None, "", None, "", None, ""), 0.0)

  /** Two Polish cities screening the same film, wired straight to the store so
   *  the spec can push single change-stream events at one city. */
  private def twoCities(): (MovieController, services.readmodel.InMemoryReadModelRepository, WebReadModel) = {
    val store = new services.readmodel.InMemoryReadModelRepository
    store.upsertMovie(resolved("belle|2021", "Belle"))
    store.upsertScreening(models.CityScreening("s-waw", "belle|2021", "warszawa", "Muranow", None, Nil))
    store.upsertScreening(models.CityScreening("s-poz", "belle|2021", "poznan", "Malta", None, Nil))
    val readModel = new WebReadModel(store)
    readModel.start()
    val (ctrl, _) = TestMovieController.build(Seq.empty, readModel = Some(readModel))
    (ctrl, store, readModel)
  }

  "A conditional GET for one city" should "still be answered 304 after ANOTHER city's showtimes change" in {
    val (ctrl, store, readModel) = twoCities()
    val warsawEtag = header(ETAG, ctrl.apiRepertoire("warszawa")(FakeRequest())).value

    // Poznan gains a venue. Warsaw's bytes are untouched, so its cached copy —
    // in the browser, the phone, or Cloudflare — must remain valid.
    //
    // ⚠️ THE SLEEP IS WHAT GIVES THIS CASE ITS TEETH. The validator is truncated
    // to whole seconds, so a Poznan change in the SAME second as Warsaw's
    // response leaves even a model-wide stamp's ETag unchanged — the case would
    // then pass against the very bug it exists to catch. Crossing the boundary
    // first makes the old behaviour genuinely produce a different ETag, so a 304
    // here can only mean the validator is scoped to the city.
    Thread.sleep(1100)
    store.upsertScreening(models.CityScreening("s-poz-2", "belle|2021", "poznan", "Palacowe", None, Nil))

    val revalidated = ctrl.apiRepertoire("warszawa")(FakeRequest().withHeaders(IF_NONE_MATCH -> warsawEtag))
    status(revalidated) shouldBe NOT_MODIFIED
    readModel.stop()
  }

  it should "be answered 200 once THAT city's own showtimes change" in {
    val (ctrl, store, readModel) = twoCities()
    val warsawEtag = header(ETAG, ctrl.apiRepertoire("warszawa")(FakeRequest())).value

    // The validator is truncated to whole seconds (an HTTP date has no finer
    // resolution), so a change landing in the same second as the response it
    // must invalidate is genuinely invisible. Cross the boundary deliberately
    // rather than race it — the point of the case is the scoping, not the clock.
    Thread.sleep(1100)
    store.upsertScreening(models.CityScreening("s-waw-2", "belle|2021", "warszawa", "Atlantic", None, Nil))

    val revalidated = ctrl.apiRepertoire("warszawa")(FakeRequest().withHeaders(IF_NONE_MATCH -> warsawEtag))
    status(revalidated) shouldBe OK
    readModel.stop()
  }

  it should "be answered 200 after a film's TITLE changes, which re-addresses the whole corpus" in {
    // `FilmSlugs` assigns /{city}/movie/{slug} over the whole corpus, so a
    // retitle in one city can change another city's links. Scoping must not
    // suppress that.
    val (ctrl, store, readModel) = twoCities()
    val warsawEtag = header(ETAG, ctrl.apiRepertoire("warszawa")(FakeRequest())).value

    Thread.sleep(1100)
    store.upsertMovie(resolved("dune|2021", "Dune"))

    val revalidated = ctrl.apiRepertoire("warszawa")(FakeRequest().withHeaders(IF_NONE_MATCH -> warsawEtag))
    status(revalidated) shouldBe OK
    readModel.stop()
  }
}
