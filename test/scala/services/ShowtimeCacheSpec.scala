package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.net.ConnectException
import java.net.http.{HttpConnectTimeoutException, HttpTimeoutException}
import java.util.concurrent.TimeoutException
import javax.net.ssl.SSLHandshakeException

class ShowtimeCacheSpec extends AnyFlatSpec with Matchers {

  private val cache = new ShowtimeCache(Nil, null, null, new UptimeMonitor())

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
