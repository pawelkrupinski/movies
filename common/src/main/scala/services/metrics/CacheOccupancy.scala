package services.metrics

import com.github.benmanes.caffeine.cache.Cache

/**
 * What one in-process cache is holding, against what it is allowed to hold.
 *
 * A cache's own numbers are the only ones that answer "is this thing working",
 * and until 2026-09-05 none of them were exported. Two incidents a day apart ran
 * on exactly that blindness: `OgCardCache` and [[tools.CachingDetailFetch]] were
 * each bounded by ENTRY COUNT over values whose size varies by four orders of
 * magnitude, so both bounds sat far from engaging while the caches filled a
 * tier's old generation. A held-bytes reading beside its maximum would have shown
 * that on a panel; instead it took a heap dump.
 *
 * Every field is optional except [[entries]] because caches differ in what they
 * can honestly answer. Caffeine only exposes a maximum through an eviction
 * policy, so an UNBOUNDED cache has no maximum to report (and must not report
 * zero, which reads as "full"); a count-bounded cache has no byte weight; and
 * hit/eviction counts exist only where `recordStats()` was enabled. A gauge that
 * is absent for a cache is the honest rendering of "this cache cannot say".
 */
final case class CacheOccupancy(
  entries:    Long,
  maxEntries: Option[Long]   = None,
  heldBytes:  Option[Long]   = None,
  maxBytes:   Option[Long]   = None,
  evictions:  Option[Long]   = None,
  hitRatio:   Option[Double] = None
)

object CacheOccupancy {

  /**
   * Read a Caffeine cache's occupancy, asking it only what it can answer.
   *
   * `weightedSize` is present iff the cache was built with a weigher, and
   * `getMaximum` means entries on a `maximumSize` cache and weight on a
   * `maximumWeight` one — the same call with two different units. `weighted`
   * says which, so a byte-weighted cache reports bytes and a count-bounded one
   * reports entries, rather than both landing in one gauge that silently mixes
   * units across caches.
   */
  def of(cache: Cache[?, ?], weighted: Boolean): CacheOccupancy = {
    val eviction = {
      val policy = cache.policy().eviction()
      if (policy.isPresent) Some(policy.get()) else None
    }
    val maximum = eviction.map(_.getMaximum)
    val stats   = cache.stats()
    CacheOccupancy(
      entries    = cache.estimatedSize(),
      maxEntries = if (weighted) None else maximum,
      heldBytes  = if (weighted) eviction.flatMap { e =>
                     val weight = e.weightedSize()
                     if (weight.isPresent) Some(weight.getAsLong) else None
                   } else None,
      maxBytes   = if (weighted) maximum else None,
      // `recordStats()` off leaves every counter at zero, which is indistinguishable
      // from a healthy cache that has evicted nothing — so report nothing instead.
      evictions  = Option.when(stats.requestCount() > 0 || stats.evictionCount() > 0)(stats.evictionCount()),
      hitRatio   = Option.when(stats.requestCount() > 0)(stats.hitRate())
    )
  }
}
