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
}
