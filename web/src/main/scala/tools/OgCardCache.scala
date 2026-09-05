package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine, Weigher}

import java.util.concurrent.{Executor, ForkJoinPool, TimeUnit}

/**
 * Shared memoisation for the rendered Open Graph cards (the film card and the
 * per-city page card). Holds a 12h, BYTE-BOUNDED Caffeine cache plus the
 * "render once, but only cache a *complete* card" rule both card services rely
 * on: a render whose poster failed to load is still returned, but NOT frozen,
 * so the next share retries instead of serving a permanently poster-less card.
 *
 * ⚠️ BOUNDED BY BYTES AND NOT BY ENTRY COUNT. It used to be `maximumSize(1000)`,
 * which is a heap bomb dressed as a cache: the values are rendered images whose
 * size is set by the poster, so "1000 entries" was anywhere between 30 MB and
 * half a gigabyte depending on what got shared. On 2026-09-04 a crawler swept
 * the share cards and found out which -- the old-gen floor on web-uk went from
 * 29% to 71% of its cap in two hours, web-de from 22% to 57%, and web-us to 82%
 * on a JVM that had OOMed the day before. A weight in bytes is the only bound
 * that means the same thing on every card.
 */
private[tools] class OgCardCache(maxBytes: Long, maintenance: Executor = ForkJoinPool.commonPool()) {
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder()
      .maximumWeight(maxBytes)
      .weigher((_: String, card: Array[Byte]) => card.length: Int)
      .expireAfterWrite(12, TimeUnit.HOURS)
      // A hit here is a whole card not composited again — worth reading as a ratio.
      .recordStats()
      // WHERE EVICTION RUNS, and the only reason it is a parameter. Caffeine
      // defers maintenance to this executor and `cleanUp()` only does the work
      // itself if it can take the eviction lock -- so on a machine whose common
      // pool is saturated (every spec in `testUnit` running at once) the pending
      // eviction is neither done by then nor done yet, and a test that asserts on
      // the bound fails on load rather than on behaviour. Production keeps the
      // common pool; the specs pass a same-thread executor and get a cache that
      // has finished evicting when `cleanUp()` returns.
      .executor(maintenance)
      .build()

  /** Return the cached card for `key`, or run `render`. `render` yields the
   *  bytes paired with whether the render is complete enough to cache. */
  def getOrRender(key: String)(render: => (Array[Byte], Boolean)): Array[Byte] =
    Option(cache.getIfPresent(key)).getOrElse {
      val (bytes, cacheable) = render
      if (cacheable) cache.put(key, bytes)
      bytes
    }

  /** Bytes currently held. Caffeine evicts asynchronously, so a caller asserting
   *  on the bound has to settle first -- see `cleanUp`. */
  private[tools] def weight: Long = cache.policy().eviction().get().weightedSize().getAsLong

  /** What this cache holds against its byte budget, for `kinowo_web_cache_*`. The
   *  reading that was missing on 2026-09-04, when a crawler swept the share cards
   *  and took web-uk's old-gen floor from 29% to 71% in two hours behind a bound
   *  that counted ENTRIES. */
  def occupancy: services.metrics.CacheOccupancy =
    services.metrics.CacheOccupancy.of(cache, weighted = true)

  /** Run pending maintenance (eviction included) now rather than on the next
   *  write. Only the specs need this; production never waits on the bound. */
  private[tools] def cleanUp(): Unit = cache.cleanUp()
}

private[tools] object OgCard {
  /** Bound on how many candidate poster URLs a card fetches before giving up on
   *  a text-only/empty render. The fallback list can hold 25+ URLs; the first
   *  reachable origin almost always wins, but the cap keeps a pathological
   *  all-dead chain from stacking dozens of slow connect timeouts on one
   *  request. (A film's *primary* poster is often a Multikino origin whose
   *  Cloudflare 403s our datacenter IP, so the fallbacks carry the load.) */
  val MaxPosterCandidates = 6

  /** How much heap the rendered cards may hold. 64 MB is ~300 cards at the size
   *  they encode to today, which covers the share traffic a human audience
   *  generates many times over -- and, unlike a count, it is the SAME 64 MB when
   *  a card turns out to be three times its usual size. The number that matters
   *  is not how many cards fit but that a sweep cannot take the tier's heap:
   *  the web pods run on a 768 MB-1 GB cap whose old-gen floor already sits
   *  between 15% and 60% before any card is rendered. */
  val MaxCacheBytes = 64L * 1024 * 1024
}
