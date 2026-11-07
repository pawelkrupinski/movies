package tools

import scala.concurrent.duration._

/**
 * A recoverable failure asked again on the spot: a request that timed out, lost its connection or met a server error is
 * tried up to [[RetryingHttpFetch.Attempts]] times, a short backoff between ([[RetryWithBackoff]]), before it fails to
 * its caller — who remembers it and asks again later, on a backoff of its own (the identity questions' memory). What a
 * retry cannot change fails at once: a 404/410 (the source's answer), another 4xx, a body the parser refused.
 *
 * Between the 429 gate and the host breaker ([[modules.wiring.HttpWiring.pacedWire]]): a rate limit is the gate's (it
 * waits the host's `Retry-After` for every caller), and each attempt goes through the breaker, so a host down for good
 * trips it and is asked no more — its [[CircuitOpenException]] is never retried. GETs only: a POST may not be safe to
 * send twice.
 */
final class RetryingHttpFetch(underlying: HttpFetch, sleep: Long => Unit, attempts: Int = RetryingHttpFetch.Attempts,
                              initialBackoff: FiniteDuration = RetryingHttpFetch.InitialBackoff)
    extends ForwardingHttpFetch {
  protected def around[T](url: String)(call: HttpFetch => T): T =
    RetryWithBackoff(s"GET ${RetryingHttpFetch.hostOf(url)}", attempts, initialBackoff, sleep, retryOn = RetryingHttpFetch.recoverable)(call(underlying))

  override def post(url: String, body: String, contentType: String): String = underlying.post(url, body, contentType)
}

object RetryingHttpFetch {
  /** Every try of one request, the first included. */
  val Attempts: Int = 3
  /** The first wait between tries, doubling, with full jitter. */
  val InitialBackoff: FiniteDuration = 500.millis

  /** A failure another try may not repeat: a timeout, a lost or refused connection, a server error that is no rate limit. */
  def recoverable(e: Throwable): Boolean = e match {
    case s: HttpStatusException  => s.code >= 500 && !HttpStatusException.isRateLimit(s)
    // an open breaker is the host down, asked again once it is not; a replayed fixture's miss is permanent
    case io: java.io.IOException => !io.isInstanceOf[CircuitOpenException] && !io.isInstanceOf[java.io.FileNotFoundException]
    case _                       => false
  }

  /** The host only: a URL may carry a key. */
  private def hostOf(url: String): String =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).getOrElse("?")
}
