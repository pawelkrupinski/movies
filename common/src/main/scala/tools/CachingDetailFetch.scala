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
 * Only SUCCESSFUL responses are cached. `get` throws on HTTP/transport failure
 * exactly like the underlying fetch, so a transient blip is never pinned for the
 * whole TTL — the next pass retries it. Caching the body (not a parsed result)
 * keeps this uniform across clients and sidesteps each client's own "empty"
 * sentinel; re-parsing the cached HTML is cheap next to the network round-trip.
 */
class CachingDetailFetch(
  underlying: HttpFetch,
  ttl:        FiniteDuration = CachingDetailFetch.DefaultTtl,
  maxEntries: Long           = 10000,
  ticker:     Ticker         = Ticker.systemTicker()
) extends HttpFetch {

  private val cache: Cache[String, String] =
    Caffeine.newBuilder()
      .expireAfterWrite(ttl.toMillis, TimeUnit.MILLISECONDS)
      .maximumSize(maxEntries)
      .ticker(ticker)
      .build()

  override def get(url: String): String = {
    val hit = cache.getIfPresent(url)
    if (hit != null) hit
    else {
      val body = underlying.get(url) // throws on failure → falls through uncached
      cache.put(url, body)
      body
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
  /** Detail metadata is effectively static per film; refreshing twice a day is
   *  plenty fresh while cutting ~all of the redundant per-pass detail fetches. */
  val DefaultTtl: FiniteDuration = 12.hours
}
