package tools

/**
 * An [[HttpFetch]] decorator that remembers what a detail page's GET answered — its body, or a PERMANENT failure — and
 * answers a repeat from memory: the one rule every detail cache keeps, in-process ([[CachingDetailFetch]]) or shared
 * across the fleet (`services.MongoCachingDetailFetch`), which differ only in where they keep it.
 *
 * A 404/410 says something permanent about the URL, so re-asking every pass buys nothing: it is the same answer, and
 * the film whose detail this is stays gated on a fetch that will not start succeeding. A remembered one is thrown
 * again, so callers see the same `HttpStatusException` either way. Every OTHER failure — timeouts, 5xx, rate limits —
 * says something about right now, and stays unremembered so the next pass retries it. The {404, 410} line lives in
 * `HttpStatusException.isDurable` — one definition, several callers.
 *
 * Detail fetches don't vary by request header, so the URL alone is the key. Raw bytes and posts pass straight through
 * (unremembered): a detail cache holds UTF-8 bodies, and re-encoding one would mojibake a legacy single-byte page.
 */
abstract class RememberingDetailFetch(underlying: HttpFetch) extends ForwardingHttpFetch {

  /** What is remembered of `url`: `Right(body)`, `Left(status)` of a permanent failure, or None for nothing. */
  protected def remembered(url: String): Option[Either[Int, String]]

  /** Keep what `url` answered: `Right(body)`, or `Left(status)` of a permanent failure. */
  protected def remember(url: String, outcome: Either[Int, String]): Unit

  override final def get(url: String): String = remembered(url) match {
    case Some(Right(body)) => body
    case Some(Left(code))  => throw new HttpStatusException(code, "GET", url, None)
    case None =>
      try {
        val body = underlying.get(url)
        remember(url, Right(body))
        body
      } catch {
        case failure: HttpStatusException if HttpStatusException.isDurable(failure.code) =>
          remember(url, Left(failure.code))
          throw failure
      }
  }

  override final def get(url: String, headers: Map[String, String]): String = get(url)

  protected final def around[T](url: String)(call: HttpFetch => T): T = call(underlying)
}
