package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.zip.GZIPInputStream

/** The plain `/{city}/` page is served as a pre-rendered, pre-gzipped blob to
 *  anonymous, gzip-accepting visitors. These assert the controller wiring of
 *  [[PageResponseCache]]: the right responses carry `Content-Encoding: gzip`
 *  and decode to the real page, while a non-gzip client still renders
 *  correctly. */
class PageCacheControllerSpec extends AnyFlatSpec with Matchers {

  private def buildController(): (MovieController, services.readmodel.WebReadModel) = {
    val now = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt123"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Cache Test Film"),
          releaseYear = Some(2024),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        )
      )
    )
    TestMovieController.build(Seq(("Cache Test Film", Some(2024), record)))
  }

  private def gzipRequest(path: String) =
    FakeRequest("GET", path).withHeaders("Accept-Encoding" -> "gzip, deflate, br")

  private def gunzip(bytes: org.apache.pekko.util.ByteString): String = {
    val in = new GZIPInputStream(new ByteArrayInputStream(bytes.toArray))
    new String(in.readAllBytes(), StandardCharsets.UTF_8)
  }

  "the / index page" should "be served gzip-precompressed to a gzip-accepting anonymous visitor" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    gunzip(contentAsBytes(result)) should include ("Cache Test Film")
  }

  it should "serve byte-identical bytes on a repeat request at the same cache version" in {
    val (ctrl, _) = buildController()
    val first  = contentAsBytes(ctrl.index("poznan")(gzipRequest("/poznan/")))
    val second = contentAsBytes(ctrl.index("poznan")(gzipRequest("/poznan/")))
    second shouldBe first
  }

  it should "re-serve a fresh valid page after the cache version advances" in {
    val (ctrl, cache) = buildController()
    ctrl.index("poznan")(gzipRequest("/poznan/"))

    Thread.sleep(1100) // mtime is second-resolution; ensure the rehydrate advances it
    cache.reload()

    val after = ctrl.index("poznan")(gzipRequest("/poznan/"))
    status(after) shouldBe OK
    header("Content-Encoding", after) shouldBe Some("gzip")
    gunzip(contentAsBytes(after)) should include ("Cache Test Film")
  }

  "a client that does not accept gzip" should "get an uncompressed page, not the precompressed blob" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(FakeRequest("GET", "/poznan/"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe None
    contentAsString(result) should include ("Cache Test Film")
  }

  // ── Browser conditional-GET (304 on refresh) ───────────────────────────────

  "a cacheable page" should "carry Last-Modified + Cache-Control so the browser revalidates" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/"))

    header("Last-Modified", result) shouldBe defined
    header("Cache-Control", result) shouldBe Some("private, no-cache")
  }

  it should "304 a refresh whose If-Modified-Since is current, with no body" in {
    val (ctrl, _) = buildController()
    val first   = ctrl.index("poznan")(gzipRequest("/poznan/"))
    val lastMod = header("Last-Modified", first).get

    val refresh = ctrl.index("poznan")(gzipRequest("/poznan/").withHeaders("If-Modified-Since" -> lastMod))
    status(refresh) shouldBe NOT_MODIFIED
    header("Content-Encoding", refresh) shouldBe None
    contentAsBytes(refresh).isEmpty shouldBe true
  }

  it should "200 with a fresh body after the cache version advances despite an old If-Modified-Since" in {
    val (ctrl, cache) = buildController()
    val lastMod = header("Last-Modified", ctrl.index("poznan")(gzipRequest("/poznan/"))).get

    Thread.sleep(1100)
    cache.reload()

    val after = ctrl.index("poznan")(gzipRequest("/poznan/").withHeaders("If-Modified-Since" -> lastMod))
    status(after) shouldBe OK
    header("Content-Encoding", after) shouldBe Some("gzip")
    gunzip(contentAsBytes(after)) should include ("Cache Test Film")
  }
}
