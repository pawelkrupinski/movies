package tools

import scala.concurrent.duration._

/**
 * A non-2xx HTTP response surfaced as a typed exception so callers and
 * decorators can react to the STATUS — notably 429 rate-limiting — and the
 * server's `Retry-After` hint, instead of pattern-matching a bare message.
 *
 * Extends `RuntimeException` with the SAME message shape the code threw before
 * (`HTTP <code> for <method> <url>`), so existing `catch`/regex callers keep
 * working unchanged — in particular `MonitoringHttpFetch`'s `HTTP 5\d\d .*`
 * connection-failure classifier.
 */
class HttpStatusException(
  val code:       Int,
  val method:     String,
  val url:        String,
  val retryAfter: Option[FiniteDuration]
) extends RuntimeException(s"HTTP $code for $method $url")

object HttpStatusException {
  /** Parse a `Retry-After` header value. Honors the delta-seconds form ("120")
   *  that TMDB and most APIs send; the rarer HTTP-date form falls back to `None`
   *  (callers apply their own default pause). Pure — takes no clock. */
  def parseRetryAfter(raw: Option[String]): Option[FiniteDuration] =
    raw.map(_.trim).filter(_.nonEmpty)
      .flatMap(s => scala.util.Try(s.toLong).toOption)
      .filter(_ >= 0)
      .map(_.seconds)
}
