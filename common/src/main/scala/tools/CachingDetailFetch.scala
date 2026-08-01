package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, Ticker}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/**
 * [[HttpFetch]] decorator that caches successful GET bodies for a TTL, keyed by
 * URL. For the cinema scrapers' STATIC detail pages only.
 *
 * Why: each scrape pass re-fetched every film's detail page (runtime, director,
 * synopsis, trailer) even though that content doesn't change between the worker's
 * minutes-apart passes — which dominated the slow cinemas' scrape time (Kinoteka
 * re-fetched ~40 unchanged detail pages every pass). Wrapping just the
 * detail-fetch calls lets a film's detail page be fetched once per [[ttl]]
 * instead of hundreds of times a day. It must NOT wrap the listing/day fetches:
 * those carry volatile showtimes and have to stay live.
 *
 * Successes are cached, and so are PERMANENT failures — a 404/410 describes the
 * URL, not the moment, so re-asking every pass buys the same answer while the film
 * whose detail it is stays gated on a fetch that will not start succeeding. Every
 * other failure (timeout, 5xx, rate limit) says something about right now and stays
 * uncached, so a transient blip is never pinned for the whole TTL. `get` still
 * throws either way, so callers are unchanged. Caching the body (not a parsed result)
 * keeps this uniform across clients and sidesteps each client's own "empty"
 * sentinel; re-parsing the cached HTML is cheap next to the network round-trip.
 */
class CachingDetailFetch(
  underlying: HttpFetch,
  ttl:        FiniteDuration = CachingDetailFetch.DefaultTtl,
  maxEntries: Long           = 10000,
  ticker:     Ticker         = Ticker.systemTicker()
) extends HttpFetch {

  // One cache for both outcomes: a URL is either a body or a remembered permanent
  // failure, never both, and sharing the entry means the TTL and the size bound
  // apply to them uniformly.
  private val cache: Cache[String, CachingDetailFetch.Outcome] =
    Caffeine.newBuilder()
      .expireAfterWrite(ttl.toMillis, TimeUnit.MILLISECONDS)
      .maximumSize(maxEntries)
      .ticker(ticker)
      .build()

  override def get(url: String): String = cache.getIfPresent(url) match {
    case CachingDetailFetch.Body(body) => body
    case CachingDetailFetch.Gone(code) => throw new HttpStatusException(code, "GET", url, None)
    case null =>
      try {
        val body = underlying.get(url)
        cache.put(url, CachingDetailFetch.Body(body))
        body
      } catch {
        // A 404/410 says something permanent about the URL, so re-asking every pass
        // buys nothing: it is the same answer, and the film whose detail this is stays
        // gated on a fetch that will not start succeeding. Every OTHER failure —
        // timeouts, 5xx, rate limits — says something about right now, and stays
        // uncached so the next pass retries it. Same {404, 410} line
        // `EnrichmentCache.isDurable` draws, for the same reason.
        case failure: HttpStatusException if CachingDetailFetch.DurableFailureStatuses(failure.code) =>
          cache.put(url, CachingDetailFetch.Gone(failure.code))
          throw failure
      }
  }

  // Detail fetches don't vary by request header; key on the URL alone.
  override def get(url: String, headers: Map[String, String]): String = get(url)

  // Raw bytes pass straight through to the underlying fetch (uncached): the
  // string cache holds UTF-8 bodies, and re-encoding one would mojibake a
  // legacy single-byte page. Don't inherit the lossy base default.
  override def getBytes(url: String): Array[Byte] = underlying.getBytes(url)

  override def post(url: String, body: String, contentType: String): String =
    underlying.post(url, body, contentType)
}

object CachingDetailFetch {

  /** What the cache holds for a URL: the body, or the permanent failure that URL gives. */
  private sealed trait Outcome
  private final case class Body(body: String) extends Outcome
  private final case class Gone(code: Int)    extends Outcome

  /** Statuses that describe the URL rather than the moment. Kept identical to
   *  `EnrichmentCache.isDurable`'s set — the two answer the same question. */
  private val DurableFailureStatuses = Set(404, 410)
  /** Detail metadata is effectively static per film; refreshing twice a day is
   *  plenty fresh while cutting ~all of the redundant per-pass detail fetches. */
  val DefaultTtl: FiniteDuration = 12.hours
}
