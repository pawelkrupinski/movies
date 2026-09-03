package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.movies.StringPool

/**
 * `kinowo_worker_string_pool_*` — whether [[StringPool]] is still interning, or
 * has quietly stopped.
 *
 * WHY THIS EXISTS. The pool collapses the byte-identical `SourceData` strings a
 * film repeats across its per-cinema slots — one `"action"` instead of the 66,106
 * copies a US heap dump found. It is BOUNDED, and that bound is the failure mode
 * nothing could see: past the maximum, Caffeine evicts, the next lookup of an
 * evicted value allocates a fresh String, and interning degrades to a no-op that
 * still costs a hash per call. There is no error, no log line, and the only
 * symptom is a heap that grows — which is how `worker-us` came to OOM twice
 * (2026-09-01 13:56, 2026-09-03 03:40) with 66.7% of its String payload duplicate.
 *
 * The pool was SUSPECTED of that and cleared: measured 2026-09-03, the pooled
 * vocabulary is 28,695 distinct values on the US corpus and 26,415 on the UK's --
 * 22% and 20% of the cap. It saturates rather than growing with the corpus, so the
 * duplication came from the paths that never reach the pool, not from eviction.
 * These gauges exist so that stays a READING rather than a belief: a corpus that
 * one day does breach the cap will show it here first, and nobody has to re-run a
 * `mongosh` count to find out.
 *
 *   - `entries` against [[StringPool.MaxEntries]] — riding the ceiling means the
 *     vocabulary no longer fits and the cap is now the thing to raise.
 *   - `evictions_total` — flat at zero is a pool doing its job. Climbing is the
 *     thrash: strings being interned and thrown away.
 *   - `hit_ratio` — the share of lookups served an existing instance. High is
 *     healthy; a fall alongside rising evictions is the degradation itself.
 *
 * Process-level, like [[JvmVitalsSampler]] and unlike the census gauges: the pool
 * is one `object` shared by every country's wiring in the JVM, so a `country`
 * label would be a lie. Read at scrape time through callback gauges — three field
 * reads, cheaper than a timer and with no staleness.
 */
object StringPoolMetrics {

  def register(registry: PrometheusRegistry): Unit = {
    gauge(registry,
      "kinowo_worker_string_pool_entries",
      "Distinct strings the intern pool is holding, against its maximum. At the maximum the pool " +
        "is evicting and interning has stopped paying for itself.",
      () => StringPool.heldEntries.toDouble)

    gauge(registry,
      "kinowo_worker_string_pool_max_entries",
      "The intern pool's configured maximum, so `entries` can be read as a fraction without the " +
        "number being written into a dashboard that then goes stale when it is retuned.",
      () => StringPool.MaxEntries.toDouble)

    gauge(registry,
      "kinowo_worker_string_pool_evictions_total",
      "Strings evicted from the intern pool since boot. Zero is healthy; any sustained climb means " +
        "the corpus vocabulary no longer fits and duplicates are reaching the heap.",
      () => StringPool.evictions.toDouble)

    gauge(registry,
      "kinowo_worker_string_pool_hit_ratio",
      "Share of intern lookups served an instance the pool already held. Falling while evictions " +
        "climb is the pool thrashing rather than deduplicating.",
      () => StringPool.hitRate)
  }

  private def gauge(registry: PrometheusRegistry, name: String, help: String, read: () => Double): Unit =
    GaugeWithCallback.builder()
      .name(name)
      .help(help)
      .callback(callback => callback.call(read()))
      .register(registry)
}
