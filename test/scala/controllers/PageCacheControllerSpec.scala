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

/** The plain `/{city}/` and `/{city}/kina` pages are served as pre-rendered,
 *  pre-gzipped blobs to anonymous, gzip-accepting visitors. These assert the
 *  controller wiring of [[PageResponseCache]]: the right responses carry
 *  `Content-Encoding: gzip` and decode to the real page, while the paths that
 *  must bypass the shared blob (no gzip, view-swap) still render correctly. */
class PageCacheControllerSpec extends AnyFlatSpec with Matchers {

  private def buildController(): (MovieController, services.movies.CaffeineMovieCache) = {
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

  private def gzipReq(path: String) =
    FakeRequest("GET", path).withHeaders("Accept-Encoding" -> "gzip, deflate, br")

  private def gunzip(bytes: org.apache.pekko.util.ByteString): String = {
    val in = new GZIPInputStream(new ByteArrayInputStream(bytes.toArray))
    new String(in.readAllBytes(), StandardCharsets.UTF_8)
  }

  "the / index page" should "be served gzip-precompressed to a gzip-accepting anonymous visitor" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipReq("/poznan/"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    gunzip(contentAsBytes(result)) should include ("Cache Test Film")
  }

  "the /kina page" should "be served gzip-precompressed to a gzip-accepting anonymous visitor" in {
    val (ctrl, _) = buildController()
    val result = ctrl.kina("poznan")(gzipReq("/poznan/kina"))

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    gunzip(contentAsBytes(result)) should include ("Cache Test Film")
  }

  it should "serve byte-identical bytes on a repeat request at the same cache version" in {
    val (ctrl, _) = buildController()
    val first  = contentAsBytes(ctrl.kina("poznan")(gzipReq("/poznan/kina")))
    val second = contentAsBytes(ctrl.kina("poznan")(gzipReq("/poznan/kina")))
    second shouldBe first
  }

  it should "re-serve a fresh valid page after the cache version advances" in {
    val (ctrl, cache) = buildController()
    ctrl.kina("poznan")(gzipReq("/poznan/kina"))

    Thread.sleep(1100) // mtime is second-resolution; ensure the rehydrate advances it
    cache.rehydrate()

    val after = ctrl.kina("poznan")(gzipReq("/poznan/kina"))
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

  "a view-swap request" should "get the bare fragment, never the cached full page" in {
    val (ctrl, _) = buildController()
    val result = ctrl.kina("poznan")(
      FakeRequest("GET", "/poznan/kina")
        .withHeaders("Accept-Encoding" -> "gzip", "X-Requested-With" -> "view-swap")
    )

    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe None
    val body = contentAsString(result)
    body should include ("Cache Test Film")
    // The swap fragment is just the #view-root inner content — no document shell.
    body.toLowerCase should not include "<!doctype"
  }
}
