package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.net.ConnectException
import java.net.http.{HttpConnectTimeoutException, HttpTimeoutException}
import java.util.concurrent.TimeoutException
import javax.net.ssl.SSLHandshakeException

class ShowtimeCacheSpec extends AnyFlatSpec with Matchers {

  private val cache = new ShowtimeCache(Nil, null, null)

  "isTransientHttpError" should "classify HttpTimeoutException as transient" in {
    cache.isTransientHttpError(new HttpTimeoutException("request timed out")) shouldBe true
  }

  it should "classify HttpConnectTimeoutException as transient" in {
    cache.isTransientHttpError(new HttpConnectTimeoutException("HTTP connect timed out")) shouldBe true
  }

  it should "classify SSLHandshakeException as transient" in {
    cache.isTransientHttpError(new SSLHandshakeException("Remote host terminated the handshake")) shouldBe true
  }

  it should "classify ConnectException as transient" in {
    cache.isTransientHttpError(new ConnectException("Connection refused")) shouldBe true
  }

  it should "classify generic IOException as transient" in {
    cache.isTransientHttpError(new IOException("network unreachable")) shouldBe true
  }

  it should "classify Future TimeoutException as transient" in {
    cache.isTransientHttpError(new TimeoutException("Future timed out after [1 minute]")) shouldBe true
  }

  it should "classify RuntimeException with HTTP status as transient" in {
    cache.isTransientHttpError(new RuntimeException("HTTP 500 for GET https://example.com")) shouldBe true
    cache.isTransientHttpError(new RuntimeException("HTTP 503 for GET https://example.com")) shouldBe true
    cache.isTransientHttpError(new RuntimeException("HTTP 429 for GET https://example.com")) shouldBe true
  }

  // Regression: Sentry was getting Multikino's `All 2 backends failed`
  // exhaustion through, because the predicate only matched messages
  // starting with `HTTP `. Both Zyte and the direct fallback timing out
  // in the same refresh tick is still a transient external blip — log
  // WARN, let the next scrape recover.
  it should "classify FallbackHttpFetch's 'All N backends failed' exhaustion as transient" in {
    cache.isTransientHttpError(new RuntimeException(
      "All 2 backends failed for get https://www.multikino.pl/api/microservice/showings/cinemas/0011/films:\n  zyte: ..."
    )) shouldBe true
    cache.isTransientHttpError(new RuntimeException(
      "All 3 backends failed for post https://example.com:\n  ..."
    )) shouldBe true
  }

  it should "not be fooled by an unrelated message that happens to start with 'All '" in {
    cache.isTransientHttpError(new RuntimeException("All quiet on the western front")) shouldBe false
  }

  it should "not classify parser errors as transient" in {
    cache.isTransientHttpError(new NoSuchElementException("head of empty list")) shouldBe false
    cache.isTransientHttpError(new NullPointerException()) shouldBe false
    cache.isTransientHttpError(new NumberFormatException("For input string: \"abc\"")) shouldBe false
    cache.isTransientHttpError(new MatchError("unexpected value")) shouldBe false
  }

  it should "not classify generic RuntimeException as transient" in {
    cache.isTransientHttpError(new RuntimeException("something went wrong")) shouldBe false
    cache.isTransientHttpError(new RuntimeException(null: String)) shouldBe false
  }

  // The continuous-loop invariant: `runPass` is driven by scheduleWithFixedDelay,
  // which CANCELS the recurring task if the run throws. So a pass must never
  // propagate — not even when it overruns its timeout — or scraping silently
  // stops. A scraper slower than the (here tiny) pass timeout must leave runPass
  // returning cleanly.
  "runPass" should "not propagate when a pass exceeds its timeout (so the loop keeps running)" in {
    import scala.concurrent.duration._
    val slow = new services.cinemas.CinemaScraper {
      val cinema = models.KinoApollo
      def scrapeHosts: Set[String] = Set.empty
      def fetch() = { Thread.sleep(500); throw new java.io.IOException("slow upstream") }
    }
    val sc = new ShowtimeCache(Seq(slow), null, null, passTimeout = 100.millis)
    noException should be thrownBy sc.runPass()
  }

  // Boot-cost guard: the first scrape pass must not fire at boot. On a fresh
  // worker, firing the ~48-cinema sweep immediately piled it onto the cold-JVM
  // cache hydrate + read-model reconcile and drained the CPU-credit balance to
  // zero (82% steal). `start()` must hold the first pass back by `initialDelay`.
  // (Before this change the initial delay was a hardcoded `0L`, so the pass ran
  // right away and the `count shouldBe 0` assertion below would fail.)
  "start" should "hold the first pass until the initial delay elapses" in {
    import scala.concurrent.duration._
    val count = new java.util.concurrent.atomic.AtomicInteger(0)
    val scraper = new services.cinemas.CinemaScraper {
      val cinema = models.KinoApollo
      def scrapeHosts: Set[String] = Set.empty
      // Count the invocation, then bail transiently so the pass settles without
      // touching the (null) runner/cache — we only care that the pass ran.
      def fetch() = { count.incrementAndGet(); throw new java.io.IOException("probe") }
    }
    val sc = new ShowtimeCache(Seq(scraper), null, null, initialDelay = 300.millis)
    sc.start()
    Thread.sleep(100)
    count.get shouldBe 0          // still within the initial delay → no pass yet
    Thread.sleep(600)
    count.get should be >= 1      // delay elapsed → first pass ran
    sc.stop()
  }
}
