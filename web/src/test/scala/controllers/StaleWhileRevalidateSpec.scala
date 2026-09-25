package controllers

import models.City
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.{Duration, Instant, ZoneId, ZonedDateTime}
import java.util.zip.GZIPInputStream
import scala.concurrent.Future

/** Stale-while-revalidate on the cached, gzipped pages ([[ConditionalResponse]] over
 *  [[EncodedResponseCache]]).
 *
 *  Measured in production 2026-09-25: `/uk/london/` (1.28 MB of HTML) had its cached
 *  copy discarded every 1-2 minutes as London's showtimes moved, and the request
 *  that found it discarded rendered synchronously in 0.7-1.2 s against ~5 ms for a
 *  hit. These cases pin the replacement: the previous copy keeps being served —
 *  under ITS OWN validators — while exactly one background render builds the new one.
 *
 *  The read-model stamp, the clock and the refresh executor are all driven by hand,
 *  so every case observes the window between "a render was scheduled" and "it
 *  finished" without a thread or a sleep. */
class StaleWhileRevalidateSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val zone = ZoneId.of("Europe/Warsaw")
  private val city: City = City.bySlug("poznan").getOrElse(fail("no city 'poznan'"))
  private val t0 = ZonedDateTime.of(2026, 9, 25, 12, 0, 0, 0, zone).toInstant

  /** One process's worth of state: a city stamp that moves when told to, a clock
   *  that moves when told to, and a refresh executor that runs when told to. */
  private class Harness {
    var stamp: Instant = t0
    var now: Instant   = t0
    val refresh        = new ManualExecutionContext
    val cache          = new EncodedResponseCache(refresh, () => now)
    val responses      = new ConditionalResponse(cache, modelStamp = _ => stamp, now = () => now)

    var renders = 0
    var content = "old"
    var failNext = false

    def get(headers: (String, String)*): Future[Result] =
      Future.successful(responses.serve(
        FakeRequest("GET", "/poznan/").withHeaders((("Accept-Encoding" -> "gzip") +: ("Host" -> "kinowo.net") +: headers)*),
        "text/html", CachePolicy.RevalidatedAnywhere, city = Some(city)) {
        renders += 1
        if (failNext) { failNext = false; throw new IllegalStateException("render failed") }
        s"<p>$content</p>"
      })

    /** The city's content changes: the read model moves its stamp. */
    def change(to: String, after: Duration = Duration.ofSeconds(30)): Unit = {
      now = now.plus(after)
      stamp = now
      content = to
    }
  }

  private def body(result: Future[Result]): String =
    new String(new GZIPInputStream(new ByteArrayInputStream(contentAsBytes(result).toArray)).readAllBytes(),
      StandardCharsets.UTF_8)

  "a changed city" should "keep getting the previous body, under the previous body's validators, while ONE render runs in the background" in {
    val h = new Harness
    val first = h.get()
    body(first) shouldBe "<p>old</p>"
    h.renders shouldBe 1

    h.change(to = "new")
    val during = h.get()
    withClue("the request that finds the copy superseded is answered from it, not made to render: ")(
      body(during) shouldBe "<p>old</p>")
    withClue("the validators must describe the body actually sent, never the version being built: ")(
      (header("ETag", during), header("Last-Modified", during)) shouldBe
        ((header("ETag", first), header("Last-Modified", first))))
    withClue("nothing rendered on the request thread: ")(h.renders shouldBe 1)
    h.refresh.pending shouldBe 1

    body(h.get()) shouldBe "<p>old</p>"
    withClue("single-flight: a second request during the render schedules no second one: ")(
      h.refresh.pending shouldBe 1)

    h.refresh.runAll()
    h.renders shouldBe 2

    val after = h.get()
    body(after) shouldBe "<p>new</p>"
    header("ETag", after) should not be header("ETag", first)
    h.renders shouldBe 2
    h.refresh.pending shouldBe 0
  }

  it should "answer a client already holding the served copy with a 304 carrying that copy's validators" in {
    val h = new Harness
    val etag = header("ETag", h.get()).value
    h.change(to = "new")

    val revalidated = h.get("If-None-Match" -> etag)
    status(revalidated) shouldBe NOT_MODIFIED
    header("ETag", revalidated) shouldBe Some(etag)
    withClue("the 304 still starts the refresh the next plain request will need: ")(h.refresh.pending shouldBe 1)
  }

  it should "retry a failed background render on the next request, serving the previous copy meanwhile" in {
    val h = new Harness
    h.get()
    h.change(to = "new")
    h.failNext = true

    body(h.get()) shouldBe "<p>old</p>"
    h.refresh.runAll()
    h.renders shouldBe 2

    body(h.get()) shouldBe "<p>old</p>"
    withClue("the failure released the key, so this request schedules the retry: ")(h.refresh.pending shouldBe 1)
    h.refresh.runAll()
    body(h.get()) shouldBe "<p>new</p>"
  }

  "the staleness bound" should s"render synchronously rather than serve a copy rendered more than ${EncodedResponseCache.MaxStaleAge} ago" in {
    val h = new Harness
    h.get()
    h.change(to = "new", after = EncodedResponseCache.MaxStaleAge.plusSeconds(1))

    body(h.get()) shouldBe "<p>new</p>"
    h.renders shouldBe 2
    h.refresh.pending shouldBe 0
  }

  it should "still serve a copy exactly at the bound" in {
    val h = new Harness
    h.get()
    h.change(to = "new", after = EncodedResponseCache.MaxStaleAge)
    body(h.get()) shouldBe "<p>old</p>"
  }

  it should "never serve across the city's midnight a copy rendered for the day before" in {
    val h = new Harness
    h.now = ZonedDateTime.of(2026, 9, 25, 23, 58, 0, 0, zone).toInstant
    h.stamp = h.now
    h.get()
    // Two minutes later, well inside the age bound, but the copy names yesterday's
    // midnight as the moment it retires itself: served now, it would reload into itself.
    h.change(to = "new", after = Duration.ofMinutes(4))

    body(h.get()) shouldBe "<p>new</p>"
    h.refresh.pending shouldBe 0
  }

  "a cold page" should "render synchronously: there is no previous copy to serve" in {
    val h = new Harness
    body(h.get()) shouldBe "<p>old</p>"
    h.renders shouldBe 1
    h.refresh.pending shouldBe 0
  }
}
