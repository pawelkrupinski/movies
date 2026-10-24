package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.http.HttpTimeoutException
import java.time.Instant
import scala.concurrent.duration._

/** A recoverable failure is asked again on the spot, a few times; what a retry cannot change fails at once. */
class RetryingHttpFetchSpec extends AnyFlatSpec with Matchers {

  private val url = "https://www.imdb.com/title/tt0063208/"
  private val sleeps = scala.collection.mutable.Buffer.empty[Long]
  private def retrying(delegate: HttpFetch) = new RetryingHttpFetch(delegate, sleeps += _)

  /** Fails with `failure` the first `times` requests, then answers. */
  private def failing(times: Int, failure: String => Throwable): RecordingHttpFetch = {
    lazy val delegate: RecordingHttpFetch = new RecordingHttpFetch(u => if (delegate.calls <= times) throw failure(u) else "answer")
    delegate
  }

  "a request that timed out, lost its connection or met a server error" should "be asked again on the spot, and answer" in {
    Seq[String => Throwable](
      _ => new HttpTimeoutException("timed out"),
      _ => new java.net.ConnectException("refused"),
      u => new HttpStatusException(502, "GET", u, None)
    ).foreach { failure =>
      val delegate = failing(2, failure)
      retrying(delegate).get(url) shouldBe "answer"
      delegate.calls shouldBe 3
    }
  }

  it should "fail to its caller after its attempts, for the caller's own backoff" in {
    val delegate = failing(10, u => new HttpStatusException(503, "GET", u, None))
    an[HttpStatusException] should be thrownBy retrying(delegate).get(url)
    delegate.calls shouldBe RetryingHttpFetch.Attempts
  }

  "what a retry cannot change" should "fail at once: the source's 404, a rate limit (the gate's), an open breaker" in {
    Seq[String => Throwable](
      u => new HttpStatusException(404, "GET", u, None),
      u => new HttpStatusException(429, "GET", u, None),
      u => new HttpStatusException(503, "GET", u, Some(30.seconds)),
      _ => new CircuitOpenException("www.imdb.com", 60000L)
    ).foreach { failure =>
      val delegate = failing(1, failure)
      an[Exception] should be thrownBy retrying(delegate).get(url)
      delegate.calls shouldBe 1
    }
  }

  it should "be asked through the breaker each time, so a host down for good trips it and is asked no more" in {
    val wire    = new RecordingHttpFetch(_ => throw new HttpTimeoutException("timed out"))
    val breaker = new HostCircuitBreakerHttpFetch(wire, failureThreshold = 4, openDuration = 60.seconds,
      clock = new MutableClock(Instant.parse("2026-10-24T00:00:00Z")))
    val fetch   = retrying(breaker)
    an[HttpTimeoutException] should be thrownBy fetch.get(url)        // 3 tries
    a[CircuitOpenException] should be thrownBy fetch.get(url)         // the 4th trips it; the open breaker is not retried
    wire.calls shouldBe 4
  }
}
