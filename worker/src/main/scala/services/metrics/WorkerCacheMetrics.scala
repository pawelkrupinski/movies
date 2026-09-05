package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry
import io.prometheus.metrics.model.snapshots.Unit as PrometheusUnit

/**
 * `kinowo_worker_cache_*` — what each in-process cache holds, against what it is
 * allowed to hold.
 *
 * WHY THIS EXISTS. On 2026-09-05 worker-pl paged `JvmOldGenNearCap` at 99.69% of
 * a 313 MiB old generation, and answering "which cache, and how full" needed a
 * heap dump taken by hand-rolling the JVM attach protocol against a JRE image
 * with no `jcmd`. The number that would have said it in a glance — bytes held
 * beside the maximum — existed inside Caffeine the whole time and was never
 * exported. The tier's own copy of the same bug (`OgCardCache`, 2026-09-04) was
 * equally invisible for equally long.
 *
 * ONE family with a `cache` label rather than a family per cache, so a new cache
 * is a registration rather than a new metric name and a new panel: the "held
 * against maximum" panel charts whatever is registered. Mirrors the shape
 * [[WorkerCorpusMetrics]] uses for its `subset` label.
 *
 * A cache that cannot honestly answer a question publishes NO SERIES for it (see
 * [[CacheOccupancy]]): an unbounded cache has no maximum, a count-bounded one no
 * byte weight. That is deliberate — a zero maximum renders as "100% full" on the
 * ratio panel, which is the wrong alarm to invent for a cache that simply has no
 * ceiling.
 *
 * Read at scrape time through callback gauges: Caffeine keeps these counters
 * itself, so a sample is a few field reads and there is no staleness window and
 * no sampler thread — the same call [[StringPoolMetrics]] makes.
 */
object WorkerCacheMetrics {

  /** One registered cache: which country's wiring owns it, what to call it, and
   *  how to read it. The reader is called on every scrape, so it must stay cheap
   *  and must not throw. */
  final case class Registration(country: String, cache: String, read: () => CacheOccupancy)

  def register(registry: PrometheusRegistry, registrations: () => Seq[Registration]): Unit = {
    gauge(registry, "kinowo_worker_cache_entries",
      "Entries a cache is holding. For a byte-bounded cache this is context for `held_bytes` " +
        "rather than the bound itself — the bound is bytes, and entry size varies hugely.",
      None)(o => Some(o.entries.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_max_entries",
      "A count-bounded cache's maximum entries. Absent for byte-bounded and unbounded caches, " +
        "which have no entry ceiling to report.",
      None)(_.maxEntries.map(_.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_held_bytes",
      "Bytes a byte-bounded cache is holding, as its own weigher charges them. Against " +
        "`max_bytes` this is the reading that says how close a cache is to its budget.",
      Some(PrometheusUnit.BYTES))(_.heldBytes.map(_.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_max_bytes",
      "A byte-bounded cache's budget, published so a dashboard reads the fraction rather than " +
        "hard-coding a number that goes stale the moment the budget is retuned.",
      Some(PrometheusUnit.BYTES))(_.maxBytes.map(_.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_evictions_total",
      "Entries evicted since boot. A cache evicting steadily is one whose working set no longer " +
        "fits its budget; whether that matters depends on what a miss costs it.",
      None)(_.evictions.map(_.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_hit_ratio",
      "Share of lookups served from the cache. Read it against what the cache is FOR: the detail " +
        "cache expires inside its refresh window, so near-zero is healthy and a high ratio means " +
        "it is absorbing a retry loop that should be fixed at its source.",
      None)(_.hitRatio, registrations)
  }

  private def gauge(registry: PrometheusRegistry, name: String, help: String, unit: Option[PrometheusUnit])
                   (read: CacheOccupancy => Option[Double], registrations: () => Seq[Registration]): Unit = {
    val builder = GaugeWithCallback.builder().name(name).help(help).labelNames("country", "cache")
    unit.foreach(builder.unit)
    builder
      .callback { callback =>
        registrations().foreach { registration =>
          read(registration.read()).foreach(value => callback.call(value, registration.country, registration.cache))
        }
      }
      .register(registry)
  }
}
