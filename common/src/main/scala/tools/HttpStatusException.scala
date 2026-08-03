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
 *
 * The message carries the url [[RedactedUrl]]-masked, because this message is
 * what every caller logs — `MovieService`'s "TMDB resolve failed … retry" line
 * among them — and TMDB/OMDb authenticate in the query string. `url` itself
 * stays raw for callers that re-issue or inspect the request.
 */
class HttpStatusException(
  val code:       Int,
  val method:     String,
  val url:        String,
  val retryAfter: Option[FiniteDuration]
) extends RuntimeException(s"HTTP $code for $method ${RedactedUrl(url)}")

object HttpStatusException {
  /** Statuses that describe the URL rather than the moment: asking again buys
   *  the same answer, however long you wait, so a caller should remember the
   *  verdict instead of retrying. Everything else (timeout, 5xx, 429) says
   *  something about right now and stays retryable.
   *
   *  One definition, because several places draw this exact line and must not
   *  drift apart: both detail-page caches ([[CachingDetailFetch]],
   *  `MongoCachingDetailFetch`) remember a durable failure for their TTL, and
   *  `EnrichDetailsHandler` stamps a durably-gone detail as fetched so its
   *  reaper backs off to the refresh window instead of retrying every tick. */
  def isDurable(code: Int): Boolean = code == 404 || code == 410

  /** Parse a `Retry-After` header value. Honors the delta-seconds form ("120")
   *  that TMDB and most APIs send; the rarer HTTP-date form falls back to `None`
   *  (callers apply their own default pause). Pure — takes no clock. */
  def parseRetryAfter(raw: Option[String]): Option[FiniteDuration] =
    raw.map(_.trim).filter(_.nonEmpty)
      .flatMap(s => scala.util.Try(s.toLong).toOption)
      .filter(_ >= 0)
      .map(_.seconds)
}
