package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import io.prometheus.metrics.model.snapshots.Unit as PrometheusUnit

/**
 * `kinowo_web_cache_*` — what each of the web tier's in-heap caches holds,
 * against what it is allowed to hold.
 *
 * WHY THIS EXISTS, twice over. The gzipped-response cache was unbounded, and
 * nothing in the process could say what that cost: on the US, where a city is a
 * STATE and the largest listing is 1.06 MB gzipped, a crawler walking the sitemap
 * pinned every one of 55 states in a 768m heap that also holds the read model,
 * and `web-us` OOM-crash-looped roughly hourly. Then on 2026-09-04 the SHARE-CARD
 * cache did the same thing behind a bound that counted ENTRIES over rendered
 * images — web-uk's old-gen floor went 29% → 71% in two hours — and it had no
 * gauge at all. Both times diagnosis meant guessing at the split between the
 * caches and the read model, because the JRE image carries no `jcmd`.
 *
 * ONE family with a `cache` label rather than a family per cache, so a new cache
 * is a registration rather than a new metric name and a new panel — the shape
 * [[WorkerCacheMetrics]] uses on the other tier. It replaces the older
 * `kinowo_web_response_cache_*`, which covered only one of these caches and never
 * published the budget its own help text told you to read against.
 *
 * A cache that cannot honestly answer a question publishes NO SERIES for it (see
 * [[CacheOccupancy]]): the response cache is a `LinkedHashMap` with no hit
 * counters, so it reports bytes and entries and no ratio. A zero would read as a
 * cache serving nothing.
 *
 * Read at SCRAPE time through callback gauges, like [[WebHostMetrics]]: a field
 * read behind one lock, far cheaper than the JVM collectors on the same scrape,
 * and a timer would only add staleness.
 */
class WebCacheMetrics(registry: PrometheusRegistry, country: String,
                      caches: Seq[(String, () => CacheOccupancy)]) {

  // This tier runs one country per process, so every registration carries the same
  // one; the worker's runs a wiring per country and varies it.
  private val registrations: () => Seq[CacheGauge.Registration] =
    () => caches.map { case (cache, read) => CacheGauge.Registration(country, cache, read) }

  private def gauge(name: String, help: String, unit: Option[PrometheusUnit])
                   (field: CacheOccupancy => Option[Double]): Unit =
    CacheGauge.register(registry, name, help, unit)(field, registrations)

  gauge("kinowo_web_cache_held_bytes",
    "Bytes a cache is holding, as its own weigher charges them. Against `max_bytes` this is what " +
      "says whether a deployment is evicting at all: Poland's corpus fits several times over and " +
      "should sit flat well under the ceiling, while the US should ride at it.",
    Some(PrometheusUnit.BYTES))(_.heldBytes.map(_.toDouble))

  gauge("kinowo_web_cache_max_bytes",
    "The cache's byte budget, published so a dashboard reads the fraction rather than hard-coding " +
      "a number that goes stale the moment the budget is retuned.",
    Some(PrometheusUnit.BYTES))(_.maxBytes.map(_.toDouble))

  gauge("kinowo_web_cache_entries",
    "How many distinct keys the cache is holding — context for `held_bytes`, not the bound itself. " +
      "Entries flat while held_bytes rides the budget is the long tail falling out.",
    None)(o => Some(o.entries.toDouble))

  gauge("kinowo_web_cache_hit_ratio",
    "Share of lookups served from the cache, where the cache counts them. On the share-card caches " +
      "a hit is a whole card not composited again, so a fall alongside a full budget is the sweep " +
      "that cost web-uk its heap.",
    None)(_.hitRatio)

  gauge("kinowo_web_cache_evictions_total",
    "Entries evicted since boot, where the cache counts them. A card cache evicting steadily is one " +
      "whose working set no longer fits its budget.",
    None)(_.evictions.map(_.toDouble))
}
