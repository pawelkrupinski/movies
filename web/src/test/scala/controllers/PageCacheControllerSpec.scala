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

  // `max-age=0` is what keeps this test's name true: the browser stores the page
  // and revalidates before every re-use, exactly as `private, no-cache` made it.
  // What changed is who may ANSWER that revalidation — the `s-maxage` lets
  // Cloudflare hold a copy for a minute and 304 the client itself instead of
  // waking the JVM, which only became safe once the page stopped naming the
  // visitor (see `SharedCacheableListingSpec`).
  "a cacheable page" should "carry Last-Modified + Cache-Control so the browser revalidates" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/"))

    header("Last-Modified", result) shouldBe defined
    header("Cache-Control", result) shouldBe
      Some(s"public, max-age=0, s-maxage=${MovieController.SharedMaxAgeSeconds}")
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

  // ── Filter variants (`?date=`, `?q=`, …) ───────────────────────────────────
  //
  // These stay out of the shared gzip cache and out of the edge — they are
  // combinatorially many and would evict the bare city pages that earn their
  // place. What they DO get is a validator, so that `private, no-cache` means
  // "ask, then usually 304" rather than "ask, then always re-download". A
  // shared `?date=tomorrow` link was re-sending the whole listing on every
  // refresh because a revalidation had nothing to validate against.

  "a filtered page" should "still tell the browser to revalidate before re-use" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))

    status(result) shouldBe OK
    header("Cache-Control", result) shouldBe Some("private, no-cache")
  }

  it should "carry validators so that revalidation can come back empty" in {
    val (ctrl, _) = buildController()
    val result = ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))

    header("ETag", result) shouldBe defined
    header("Last-Modified", result) shouldBe defined
  }

  it should "304 a refresh carrying the ETag it was given" in {
    val (ctrl, _) = buildController()
    val etag = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))).get

    val refresh = ctrl.index("poznan")(
      gzipRequest("/poznan/?date=tomorrow").withHeaders("If-None-Match" -> etag))
    status(refresh) shouldBe NOT_MODIFIED
    contentAsBytes(refresh).isEmpty shouldBe true
  }

  // The page puts its own URL in `og:url`, so two filters really do render
  // different bytes — one must never validate the other.
  it should "not answer one filter with another filter's validator" in {
    val (ctrl, _) = buildController()
    val tomorrow = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))).get
    val week     = header("ETag", ctrl.index("poznan")(gzipRequest("/poznan/?date=week"))).get

    tomorrow should not be week
    val crossed = ctrl.index("poznan")(
      gzipRequest("/poznan/?date=week").withHeaders("If-None-Match" -> tomorrow))
    status(crossed) shouldBe OK
    contentAsString(crossed) should include ("Cache Test Film")
  }

  // The bare page keeps the precompressed blob; a filter variant must not take
  // an entry in that byte-bounded LRU.
  it should "not be served from the shared precompressed blob" in {
    val (ctrl, _) = buildController()
    header("Content-Encoding", ctrl.index("poznan")(gzipRequest("/poznan/"))) shouldBe Some("gzip")
    header("Content-Encoding", ctrl.index("poznan")(gzipRequest("/poznan/?date=tomorrow"))) shouldBe None
  }

  // ── The day the payload was cut for ────────────────────────────────────────

  "a zoned payload's validator" should "advance to the new day even when the model has not moved" in {
    val zone      = java.time.ZoneId.of("Europe/Warsaw")
    val beforeMid = java.time.ZonedDateTime.of(2026, 9, 5, 23, 40, 0, 0, zone).toInstant
    val afterMid  = java.time.ZonedDateTime.of(2026, 9, 6, 0, 5, 0, 0, zone).toInstant
    val dayStart  = java.time.ZonedDateTime.of(2026, 9, 6, 0, 0, 0, 0, zone).toInstant

    // Same model stamp on both sides of midnight — the day is what moved.
    MovieController.dayFlooredValidator(beforeMid, Some(zone), now = beforeMid) shouldBe beforeMid
    MovieController.dayFlooredValidator(beforeMid, Some(zone), now = afterMid)  shouldBe dayStart
  }

  it should "leave a stamp from later in the same day alone" in {
    val zone  = java.time.ZoneId.of("Europe/Warsaw")
    val noon  = java.time.ZonedDateTime.of(2026, 9, 6, 12, 0, 0, 0, zone).toInstant
    val later = java.time.ZonedDateTime.of(2026, 9, 6, 15, 0, 0, 0, zone).toInstant

    MovieController.dayFlooredValidator(noon, Some(zone), now = later) shouldBe noon
  }

  it should "leave a payload with no day in it on the model stamp alone" in {
    val stamp = java.time.Instant.parse("2020-01-01T00:00:00Z")
    MovieController.dayFlooredValidator(stamp, None) shouldBe stamp
  }
}
