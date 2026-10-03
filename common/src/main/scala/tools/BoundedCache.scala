package tools

import com.github.benmanes.caffeine.cache.Caffeine

/**
 * The one way main code builds a Caffeine cache: every builder starts BOUNDED.
 *
 * The class of bug this closes: a cache that only grows. A builder with no `maximumSize` (an
 * `expireAfterWrite` alone keeps every key written inside the window — a burst of distinct keys is
 * all held), or a `maximumWeight` whose weigher answers 0 for some entries — Caffeine never evicts a
 * zero-weight entry, so an empty fragment or a `None` result pinned its key (and whatever the key
 * holds) for the life of the process. Each was found and fixed in its own place; the bound now lives
 * here, and `NoUnboundedConcurrencyPrimitiveSpec` fails the build on a raw `Caffeine.newBuilder`.
 *
 *  - [[ofSize]]: at most `maxEntries` entries.
 *  - [[ofWeight]]: at most `maxWeight` total, every entry weighing at least `entryOverhead` (itself
 *    at least [[MinEntryOverhead]]) on top of what `weigh` says — so no entry is ever weightless,
 *    and an entry's key and node are charged even when its value is empty.
 *
 * The returned builder is a plain Caffeine builder: add `expireAfterWrite`, `recordStats`, a ticker
 * or an executor as before.
 */
object BoundedCache {

  /** The least an entry of a weighed cache is charged: a Caffeine node, its key and its value
   *  reference are this many bytes before the value holds anything. */
  val MinEntryOverhead: Int = 64

  /** A builder holding at most `maxEntries` entries. */
  def ofSize(maxEntries: Long): Caffeine[Any, Any] = {
    require(maxEntries > 0, s"a cache bound must be positive, got $maxEntries")
    Caffeine.newBuilder().maximumSize(maxEntries)
  }

  /** A builder holding at most `maxWeight`, each entry weighing `entryOverhead` plus `weigh`'s
   *  answer (a negative answer counts as 0; the sum saturates at `Int.MaxValue`). */
  def ofWeight[K, V](maxWeight: Long, entryOverhead: Int = MinEntryOverhead)(weigh: (K, V) => Int): Caffeine[K, V] = {
    require(maxWeight > 0, s"a cache bound must be positive, got $maxWeight")
    require(entryOverhead >= MinEntryOverhead, s"an entry overhead below $MinEntryOverhead lets entries weigh almost nothing, got $entryOverhead")
    Caffeine.newBuilder().maximumWeight(maxWeight).weigher[K, V]((key: K, value: V) => weight(entryOverhead, weigh(key, value)))
  }

  private[tools] def weight(entryOverhead: Int, weighed: Int): Int =
    math.min(Int.MaxValue.toLong, entryOverhead.toLong + math.max(0, weighed)).toInt
}
