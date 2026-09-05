package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, Ticker, Weigher}

import java.util.concurrent.{Executor, ForkJoinPool, TimeUnit}
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
 * The cache is bounded by the BYTES its bodies retain, not by how many there are.
 * Detail pages run from a few KB to 1.4 MB, so an entry count says nothing about
 * memory: with the old `maximumSize(10000)` bound, worker-pl was found holding
 * 1,015 bodies worth 228 MiB — 73% of its 313 MiB old gen, and its tenured floor
 * pinned at the cap doing two full GCs a minute — while still nine thousand
 * entries short of the bound that was supposed to stop it. See [[maxBytes]].
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
  maxBytes:   Long           = CachingDetailFetch.DefaultMaxBytes,
  ticker:     Ticker         = Ticker.systemTicker(),
  // WHERE EVICTION RUNS, and the only reason it is a parameter — same trade-off
  // `OgCardCache` documents at length: Caffeine defers maintenance here, so a
  // spec asserting on the bound has to run it on the calling thread to know it
  // has happened. Production keeps the common pool.
  maintenance: Executor      = ForkJoinPool.commonPool()
) extends HttpFetch {

  // One cache for both outcomes: a URL is either a body or a remembered permanent
  // failure, never both, and sharing the entry means the TTL and the size bound
  // apply to them uniformly.
  private val cache: Cache[String, CachingDetailFetch.Outcome] =
    Caffeine.newBuilder()
      .expireAfterWrite(ttl.toMillis, TimeUnit.MILLISECONDS)
      .maximumWeight(maxBytes)
      .weigher(CachingDetailFetch.RetainedBytes)
      .executor(maintenance)
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
        // uncached so the next pass retries it. The {404, 410} line lives in
        // `HttpStatusException.isDurable` — one definition, several callers.
        case failure: HttpStatusException if HttpStatusException.isDurable(failure.code) =>
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
  private[tools] sealed trait Outcome
  private[tools] final case class Body(body: String) extends Outcome
  private[tools] final case class Gone(code: Int)    extends Outcome

  /**
   * SHORTER THAN THE DETAIL REFRESH WINDOW, and that relationship is the whole
   * setting — `CachingDetailFetchSpec` pins it against
   * `Freshness.ttlFor(DetailEnrich)`.
   *
   * This started at 12h, for a world that lasted two days. The cache was added
   * 2026-06-06 because the slow scrapers (Kinoteka ~57s, DCF, Pałacowe, Muranów,
   * Apollo, Cytadela) pulled every film's detail page inline in `fetch()`, on
   * every minutes-apart pass. By 2026-06-08 all of them had been converted to
   * deferred queue detail, and a film's detail became one reaper task per
   * `DetailEnrich` window. Nothing re-reads a detail URL inside a pass any more,
   * so the redundancy this TTL was sized for does not exist.
   *
   * What a TTL LONGER than the refresh window bought instead was a refresh that
   * is not one: at 12h over a 6h window, every second scheduled refresh was
   * served the cached body, re-parsed to the identical detail, and then stamped
   * `lastFetchedAt = now`. It could not see a change by construction, so detail
   * ran 12h stale while the stamp claimed 6h.
   *
   * An hour was chosen while this cache was still absorbing a livelock: a page
   * that returned 200 and parsed to nothing became `DetailFetchOutcome.Failed`,
   * which never stamps, so `DetailReaper` re-enqueued that film every tick
   * indefinitely (Kino Bulgarska: 1,438 failures to 56 successes in 24h on one
   * trailer-less film) and the cache was what kept those retries off a small
   * cinema's site. That is fixed at the clients now — a page that LOADED is a
   * detail, `DetailEnricherDurableFailureSpec` holds every one of them to it — and
   * the venue caches this defended were deleted once nothing read them.
   *
   * So an hour is now just a sane default for whoever inherits it: the two
   * production chains set their own (2h each, on the Mongo-backed cache), and
   * this is what a NEW chain or a diagnostic gets. Keep it well inside the refresh
   * window, and well above the reaper's tick — a cache that expires faster than
   * the work arrives is not one.
   */
  val DefaultTtl: FiniteDuration = 1.hour

  /**
   * What one cache may retain, in bytes of body.
   *
   * MEASURED, not guessed, from worker-pl's heap on 2026-09-05: 1,015 live
   * bodies, median 142 KiB, mean 236 KiB, largest 1.39 MiB. The widest single
   * venue in that dump held 54 pages, so 8 MiB — ~56 median pages — covers the
   * largest working set actually observed while the pathological megabyte pages
   * get evicted instead of pinned.
   *
   * The bound is PER INSTANCE, which is what made it dangerous: the bespoke venue
   * clients each built their own, 59 of them, so the real ceiling was this budget
   * times the roster and grew whenever Poland gained a venue. Those caches are
   * gone — nothing read them once the TTL was right — and the only instances left
   * come from the `CinemaScraperCatalog.chainDetailCache` seam, one per chain, of
   * which production has two and both are Mongo-backed. So this number now bounds
   * a diagnostic's in-process cache rather than the worker's heap; keep it
   * measured anyway, because the seam's default is what a new chain gets.
   */
  val DefaultMaxBytes: Long = 8L * 1024 * 1024

  /**
   * What an entry actually costs the heap.
   *
   * A `String` holds one byte per char only while every char fits in Latin-1;
   * one that doesn't is stored UTF-16, at two. That is not a rounding error
   * here — it is half of why the POLISH worker blew its heap where the larger UK
   * corpus did not: `ł`, `ż` and `ś` are all outside Latin-1, so essentially
   * every Polish detail page costs double what its length suggests. A remembered
   * failure retains nothing but its own small record, so it weighs 1 — never 0,
   * which Caffeine reads as "may never be evicted".
   */
  private[tools] val RetainedBytes: Weigher[String, Outcome] =
    (_: String, outcome: Outcome) =>
      outcome match {
        case Body(body) =>
          val perChar = if (body.chars().anyMatch(_ > 0xFF)) 2 else 1
          math.max(1, math.min(Int.MaxValue.toLong, body.length.toLong * perChar).toInt)
        case Gone(_) => 1
      }
}
