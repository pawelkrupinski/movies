package controllers

import models.City
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.{Instant, ZoneId, ZonedDateTime}
import scala.concurrent.Future

/** [[ConditionalResponse]] on its own, with a fixed read-model stamp and clock —
 *  the headers, the 304, and the cache key pinned without a controller, a read
 *  model or a rendered page in the way. The controller-level wiring of the same
 *  mechanism is [[PageCacheControllerSpec]] and [[ApiRepertoireConditionalSpec]]. */
class ConditionalResponseSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val stamp = Instant.parse("2026-09-05T10:20:30Z")
  private val zone  = ZoneId.of("Europe/Warsaw")
  private val city: City = City.bySlug("poznan").getOrElse(fail("no city 'poznan'"))

  /** A response builder whose model stamp never moves and whose clock is `now`,
   *  answering as a `Future` so Play's result extractors read it like a
   *  controller's. */
  private class Responses(cache: EncodedResponseCache = TestResponseCache(), now: Instant = stamp) {
    private val underlying = new ConditionalResponse(cache, modelStamp = _ => stamp, now = () => now)
    def serve(request: play.api.mvc.RequestHeader, contentType: String, policy: CachePolicy,
              cacheKey: String = "", city: Option[City] = None,
              cacheBody: Boolean = true)(body: => String): Future[play.api.mvc.Result] =
      Future.successful(underlying.serve(request, contentType, policy, cacheKey, city, cacheBody)(ResponseBody.text(body)))
  }
  private def responses(cache: EncodedResponseCache = TestResponseCache(), now: Instant = stamp) =
    new Responses(cache, now)

  private def gzipRequest(path: String, host: String = "kinowo.net") =
    FakeRequest("GET", path).withHeaders("Accept-Encoding" -> "gzip", "Host" -> host)

  // ── The validator ───────────────────────────────────────────────────────────

  "the ETag" should "be weak: the key's hash and the stamp's epoch seconds, both in hex" in {
    val result  = responses().serve(gzipRequest("/poznan/"), "text/html", CachePolicy.RevalidatedAnywhere)("<p>")
    val bodyKey = "kinowo.net" + "/poznan/"
    header("ETag", result) shouldBe
      Some("W/\"" + Integer.toHexString(bodyKey.hashCode) + "-" + stamp.getEpochSecond.toHexString + "\"")
    header("Last-Modified", result) shouldBe Some("Sat, 5 Sep 2026 10:20:30 GMT")
  }

  it should "answer a matching If-None-Match with a bodiless 304 that still carries Vary and the validators" in {
    val serve = responses()
    val etag  = header("ETag", serve.serve(gzipRequest("/poznan/"), "text/html", CachePolicy.RevalidatedAnywhere)("<p>")).value

    var rendered = 0
    val result = serve.serve(gzipRequest("/poznan/").withHeaders("If-None-Match" -> etag), "text/html",
      CachePolicy.RevalidatedAnywhere) { rendered += 1; "<p>" }

    status(result) shouldBe NOT_MODIFIED
    contentAsBytes(result).isEmpty shouldBe true
    withClue("a 304 short-circuits before the body is rendered: ")(rendered shouldBe 0)
    header("ETag", result) shouldBe Some(etag)
    header("Vary", result) shouldBe Some("Accept-Encoding")
    header("Cache-Control", result) shouldBe Some("public, max-age=0, must-revalidate, no-transform")
  }

  it should "serve the body when the offered tag is not this response's" in {
    val result = responses().serve(gzipRequest("/poznan/").withHeaders("If-None-Match" -> "W/\"stale-1\""),
      "text/html", CachePolicy.RevalidatedAnywhere)("<p>")
    status(result) shouldBe OK
  }

  // ── Cache-Control, one string per policy ────────────────────────────────────

  "Cache-Control" should "be private, no-cache, no-transform under BrowserOnly" in {
    val result = responses().serve(gzipRequest("/poznan/?date=tomorrow"), "text/html", CachePolicy.BrowserOnly,
      cacheKey = "|q=date=tomorrow", cacheBody = false)("<p>")
    header("Cache-Control", result) shouldBe Some("private, no-cache, no-transform")
  }

  // An uncached body used to go out plain for the GzipFilter to compress: a second full
  // pass over the page, ~40 MB a render on New York. Gzipped here, from the same walk
  // that writes it, the filter sees an encoded response and leaves it alone — and the
  // shared cache still holds nothing for a filter variant.
  "an uncached body" should "go out already gzipped to a client that takes gzip, and stay out of the cache" in {
    val cache  = TestResponseCache()
    val result = responses(cache).serve(gzipRequest("/poznan/?date=tomorrow"), "text/html", CachePolicy.BrowserOnly,
      cacheKey = "|q=date=tomorrow", cacheBody = false)("<p>tomorrow</p>")
    header("Content-Encoding", result) shouldBe Some("gzip")
    val gzipped = scala.concurrent.Await.result(result, scala.concurrent.duration.Duration.Inf).body match {
      case play.api.http.HttpEntity.Strict(data, _) => data
      case other                                    => fail(s"expected a strict body, got $other")
    }
    new String(new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(gzipped.toArray)).readAllBytes(), "UTF-8") shouldBe "<p>tomorrow</p>"
    cache.heldEntries shouldBe 0
  }

  it should "go out plain to a client that refuses gzip" in {
    val result = responses().serve(FakeRequest("GET", "/poznan/?date=tomorrow").withHeaders("Host" -> "kinowo.net"),
      "text/html", CachePolicy.BrowserOnly, cacheKey = "|q=date=tomorrow", cacheBody = false)("<p>tomorrow</p>")
    header("Content-Encoding", result) shouldBe None
    contentAsString(result) shouldBe "<p>tomorrow</p>"
  }

  it should "be public, max-age=0, must-revalidate, no-transform under RevalidatedAnywhere" in {
    val result = responses().serve(gzipRequest("/poznan/"), "text/html", CachePolicy.RevalidatedAnywhere)("<p>")
    header("Cache-Control", result) shouldBe Some("public, max-age=0, must-revalidate, no-transform")
    header("Content-Encoding", result) shouldBe Some("gzip")
  }

  // ── The cache key ───────────────────────────────────────────────────────────

  "the blob" should "be keyed on the host as well as the path" in {
    val cache = TestResponseCache()
    val serve = responses(cache)
    var rendered = 0

    serve.serve(gzipRequest("/poznan/", host = "kinowo.net"),   "text/html", CachePolicy.RevalidatedAnywhere) { rendered += 1; "<p>" }
    serve.serve(gzipRequest("/poznan/", host = "showtimes.cc"), "text/html", CachePolicy.RevalidatedAnywhere) { rendered += 1; "<p>" }
    withClue("the second host must not be handed the first host's blob: ")(rendered shouldBe 2)
    cache.heldEntries shouldBe 2

    serve.serve(gzipRequest("/poznan/", host = "kinowo.net"),   "text/html", CachePolicy.RevalidatedAnywhere) { rendered += 1; "<p>" }
    withClue("the same host and path is a hit: ")(rendered shouldBe 2)
  }

  it should "give the two hosts different validators too" in {
    val serve = responses()
    val first  = header("ETag", serve.serve(gzipRequest("/poznan/", host = "kinowo.net"),   "text/html", CachePolicy.RevalidatedAnywhere)("<p>"))
    val second = header("ETag", serve.serve(gzipRequest("/poznan/", host = "showtimes.cc"), "text/html", CachePolicy.RevalidatedAnywhere)("<p>"))
    first should not be second
  }

  // ── The city's midnight ─────────────────────────────────────────────────────

  "a city-scoped response" should "retire a held copy at the city's midnight even when the model has not moved" in {
    val beforeMidnight = ZonedDateTime.of(2026, 9, 5, 23, 40, 0, 0, zone).toInstant
    val afterMidnight  = ZonedDateTime.of(2026, 9, 6, 0, 5, 0, 0, zone).toInstant

    val etag = header("ETag", responses(now = beforeMidnight)
      .serve(gzipRequest("/poznan/"), "text/html", CachePolicy.RevalidatedAnywhere, city = Some(city))("<p>")).value

    status(responses(now = beforeMidnight).serve(gzipRequest("/poznan/").withHeaders("If-None-Match" -> etag),
      "text/html", CachePolicy.RevalidatedAnywhere, city = Some(city))("<p>")) shouldBe NOT_MODIFIED
    status(responses(now = afterMidnight).serve(gzipRequest("/poznan/").withHeaders("If-None-Match" -> etag),
      "text/html", CachePolicy.RevalidatedAnywhere, city = Some(city))("<p>")) shouldBe OK
  }

  // ── Two changes inside one second ───────────────────────────────────────────

  /** A response builder whose model stamp the test moves, the clock `now` to match. */
  private final class MovingStamp(var stamp: Instant, var now: Instant) {
    private val cache      = TestResponseCache()
    private val underlying = new ConditionalResponse(cache, modelStamp = _ => stamp, now = () => now)
    def serve(request: play.api.mvc.RequestHeader)(body: => String): Future[play.api.mvc.Result] =
      Future.successful(underlying.serve(request, "text/html", CachePolicy.RevalidatedAnywhere)(ResponseBody.text(body)))
  }

  // A film's projection lands as a burst — its movie document, then its venue rows — well
  // inside one second. Truncated to the second, the stamp after the burst named the same
  // version as a page rendered mid-burst, so the cache kept serving that page and every
  // client holding it was told 304, until the city next changed in some other second.
  "a stamp that moves within one second" should "retire the copy rendered before the move" in {
    val serve = new MovingStamp(stamp.plusMillis(100), stamp.plusMillis(150))
    val first = serve.serve(gzipRequest("/poznan/"))("<p>before</p>")
    serve.stamp = stamp.plusMillis(700)
    serve.now   = stamp.plusMillis(750)
    serve.serve(gzipRequest("/poznan/"))("<p>after</p>")         // the held copy, while it re-renders
    val third = serve.serve(gzipRequest("/poznan/"))("<p>after</p>")
    gunzip(contentAsBytes(third).toArray) shouldBe "<p>after</p>"
    header("ETag", third) should not be header("ETag", first)
    status(serve.serve(gzipRequest("/poznan/").withHeaders("If-None-Match" -> header("ETag", first).value))("<p>after</p>")) shouldBe OK
  }

  it should "let If-None-Match decide alone, as RFC 9110 requires, when both are sent" in {
    val serve = new MovingStamp(stamp.plusMillis(700), stamp.plusSeconds(5))
    val both = FakeRequest("GET", "/poznan/").withHeaders("Accept-Encoding" -> "gzip", "Host" -> "kinowo.net",
      "If-None-Match" -> "W/\"stale-1\"", "If-Modified-Since" -> "Sat, 5 Sep 2026 10:20:31 GMT")
    status(serve.serve(both)("<p>")) shouldBe OK
  }

  private def gunzip(bytes: Array[Byte]): String =
    new String(new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(bytes)).readAllBytes(), "UTF-8")
}
