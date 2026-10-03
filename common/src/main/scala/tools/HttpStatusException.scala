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

  /** Parse a `Retry-After` header value: the delta-seconds form ("120") that TMDB and most
   *  APIs send, or the HTTP-date form measured against the SAME response's `Date` header —
   *  the server's clock on both sides, so no local clock is read and no skew between the two
   *  creeps in. A date already past is a wait of zero; a date with no `Date` to measure from,
   *  or anything unreadable, is `None` (callers apply their own pause). Pure. */
  def parseRetryAfter(raw: Option[String], responseDate: Option[String] = None): Option[FiniteDuration] =
    raw.map(_.trim).filter(_.nonEmpty).flatMap { value =>
      value.toLongOption match {
        case Some(seconds) => Option.when(seconds >= 0)(seconds.seconds)
        case None =>
          for {
            at  <- httpDate(value)
            now <- responseDate.flatMap(httpDate)
          } yield java.time.Duration.between(now, at).toMillis.max(0L).millis
      }
    }

  private def httpDate(value: String): Option[java.time.Instant] =
    scala.util.Try(java.time.ZonedDateTime.parse(value.trim, java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME).toInstant).toOption
}
