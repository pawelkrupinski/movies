package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.http.HttpTimeoutException
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class HostCircuitBreakerHttpFetchSpec extends AnyFlatSpec with Matchers {

  private val urlA = "https://restapi.helios.pl/api/cinema/x/screen/y"
  private val urlB = "https://api.themoviedb.org/3/movie/1"
  private val hostA = "restapi.helios.pl"

  /** A delegate whose response is swappable, counting every call that reaches it. */
  private class FakeDelegate(var respond: String => String) extends GetOnlyHttpFetch {
    val calls = new AtomicInteger(0)
    override def get(url: String): String = { calls.incrementAndGet(); respond(url) }
  }

  private def timeout: String => String = _ => throw new HttpTimeoutException("timed out")
  private def serverError: String => String = url => throw new HttpStatusException(503, "GET", url, None)
  private def notFound: String => String = url => throw new HttpStatusException(404, "GET", url, None)
  private def tooManyRequests: String => String = url => throw new HttpStatusException(429, "GET", url, None)

  private def breaker(delegate: HttpFetch, now: () => Instant = () => Instant.parse("2026-06-23T00:00:00Z")) =
    new HostCircuitBreakerHttpFetch(delegate, failureThreshold = 4, openDuration = 60.seconds, now = now)

  "the breaker" should "open after the failure threshold and then fast-fail WITHOUT touching the wire" in {
    val delegate = new FakeDelegate(timeout)
    val cb = breaker(delegate)
    // The first 4 calls all reach the delegate and time out; the 4th trips it open.
    (1 to 4).foreach(_ => a[HttpTimeoutException] should be thrownBy cb.get(urlA))
    delegate.calls.get() shouldBe 4
    cb.openRemainingMillis(hostA) should be > 0L
    // Now open: the next call fails fast and the delegate is NOT called again —
    // this is the whole point (a dead host stops costing even its short timeout).
    a[CircuitOpenException] should be thrownBy cb.get(urlA)
    delegate.calls.get() shouldBe 4
  }

  it should "stay closed below the threshold" in {
    val delegate = new FakeDelegate(timeout)
    val cb = breaker(delegate)
    (1 to 3).foreach(_ => a[HttpTimeoutException] should be thrownBy cb.get(urlA))
    cb.openRemainingMillis(hostA) shouldBe 0L
  }

  it should "reset the failure count on a successful round-trip" in {
    val delegate = new FakeDelegate(timeout)
    val cb = breaker(delegate)
    (1 to 3).foreach(_ => a[HttpTimeoutException] should be thrownBy cb.get(urlA))
    delegate.respond = _ => "ok"
    cb.get(urlA) shouldBe "ok"              // success clears the 3 accrued failures
    delegate.respond = timeout
    (1 to 3).foreach(_ => a[HttpTimeoutException] should be thrownBy cb.get(urlA))
    cb.openRemainingMillis(hostA) shouldBe 0L // 3 fresh failures, still under threshold — proves no carry-over
  }

  it should "trip on 5xx but NOT on a 404 (the host answering, not host trouble)" in {
    val notFoundCb = breaker(new FakeDelegate(notFound))
    (1 to 8).foreach(_ => a[HttpStatusException] should be thrownBy notFoundCb.get(urlA))
    notFoundCb.openRemainingMillis(hostA) shouldBe 0L // 8 × 404 never opens

    val serverErrCb = breaker(new FakeDelegate(serverError))
    (1 to 4).foreach(_ => a[HttpStatusException] should be thrownBy serverErrCb.get(urlA))
    serverErrCb.openRemainingMillis(hostA) should be > 0L // 4 × 503 opens
  }

  it should "trip on SUSTAINED 429 — a host refusing us, unlike other 4xx" in {
    // Filmstarts (2026-07-18) answered every request 429 for hours. Without this
    // the breaker never opened (429 is a 4xx) and the worker kept firing ~14k
    // guaranteed-rejected requests an hour at a host that had said stop.
    val cb = breaker(new FakeDelegate(tooManyRequests))
    (1 to 4).foreach(_ => a[HttpStatusException] should be thrownBy cb.get(urlA))
    cb.openRemainingMillis(hostA) should be > 0L
  }

  it should "NOT trip on a 429 that recovers — transient backpressure isn't a refusal" in {
    // TMDB throttles a burst then serves the retry; ThrottledHttpFetch's gate
    // handles that, and the success must clear the accrued 429s so an ordinary
    // rate-limit blip never costs the host a 60s blackout.
    val delegate = new FakeDelegate(tooManyRequests)
    val cb = breaker(delegate)
    (1 to 3).foreach(_ => a[HttpStatusException] should be thrownBy cb.get(urlA))
    delegate.respond = _ => "ok"
    cb.get(urlA) shouldBe "ok"
    delegate.respond = tooManyRequests
    (1 to 3).foreach(_ => a[HttpStatusException] should be thrownBy cb.get(urlA))
    cb.openRemainingMillis(hostA) shouldBe 0L // no carry-over across the success
  }

  it should "go half-open after the cooldown and close again on a success" in {
    val delegate = new FakeDelegate(timeout)
    var clock = Instant.parse("2026-06-23T00:00:00Z")
    val cb = new HostCircuitBreakerHttpFetch(delegate, failureThreshold = 4, openDuration = 60.seconds, now = () => clock)
    (1 to 4).foreach(_ => a[HttpTimeoutException] should be thrownBy cb.get(urlA))
    cb.openRemainingMillis(hostA) should be > 0L
    clock = clock.plusSeconds(61) // cooldown elapsed → half-open
    cb.openRemainingMillis(hostA) shouldBe 0L
    delegate.respond = _ => "ok"
    val callsBefore = delegate.calls.get()
    cb.get(urlA) shouldBe "ok"               // the trial call reaches the wire and succeeds
    delegate.calls.get() shouldBe callsBefore + 1
    cb.openRemainingMillis(hostA) shouldBe 0L // closed
  }

  it should "isolate hosts — one open breaker never blocks a healthy host" in {
    val delegate = new FakeDelegate(url => if (url.contains("helios")) throw new HttpTimeoutException("t") else "ok")
    val cb = breaker(delegate)
    (1 to 4).foreach(_ => a[HttpTimeoutException] should be thrownBy cb.get(urlA))
    cb.openRemainingMillis(hostA) should be > 0L
    // Host A is open, but B is untouched and still served.
    cb.get(urlB) shouldBe "ok"
    a[CircuitOpenException] should be thrownBy cb.get(urlA)
  }
}
