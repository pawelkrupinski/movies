package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry

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
 * NO BYTE SERIES HERE, for the same reason one level up. `held_bytes`/`max_bytes`
 * were published while the venue detail cache existed; deleting it (@8041c1fbf)
 * left no byte-bounded cache in this JVM, and a family that can never carry a
 * sample is a panel nobody can read. [[CacheOccupancy]] still MODELS both — it is
 * the general shape of "what a cache holds against its bound" — so restoring them
 * is this method plus a panel, the day something here is weighed in bytes again.
 * The web tier's `OgCardCache` IS byte-bounded and still unmeasured; it belongs on
 * a `kinowo_web_cache_*` family, not this one.
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
      "Entries a cache is holding, against `max_entries` where the cache is bounded by count. " +
        "For an unbounded one the SHAPE is the signal: a count that tracks its corpus is healthy, " +
        "one that climbs past it has stopped evicting.")(o => Some(o.entries.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_max_entries",
      "A count-bounded cache's maximum entries. Absent for byte-bounded and unbounded caches, " +
        "which have no entry ceiling to report.")(_.maxEntries.map(_.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_evictions_total",
      "Entries evicted since boot. A cache evicting steadily is one whose working set no longer " +
        "fits its budget; whether that matters depends on what a miss costs it.")(_.evictions.map(_.toDouble), registrations)

    gauge(registry, "kinowo_worker_cache_hit_ratio",
      "Share of lookups served from the cache. Read it against what each cache is FOR: on " +
        "`task_dedup` a high ratio is the point, since every hit is a Mongo enqueue round-trip " +
        "not made, and a fall means the dedup is being evicted.")(_.hitRatio, registrations)
  }

  private def gauge(registry: PrometheusRegistry, name: String, help: String)
                   (read: CacheOccupancy => Option[Double], registrations: () => Seq[Registration]): Unit =
    GaugeWithCallback.builder().name(name).help(help).labelNames("country", "cache")
      .callback { callback =>
        registrations().foreach { registration =>
          read(registration.read()).foreach(value => callback.call(value, registration.country, registration.cache))
        }
      }
      .register(registry)
}
