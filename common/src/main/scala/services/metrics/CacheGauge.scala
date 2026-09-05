package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry
import io.prometheus.metrics.model.snapshots.Unit as PrometheusUnit

/**
 * One `{country, cache}`-labelled callback gauge over a set of caches, reading a
 * single field of each one's [[CacheOccupancy]].
 *
 * Shared because both tiers publish the same SHAPE of cache series and differ
 * only in what they call them and which fields they have to publish: the worker's
 * caches are bounded by count, the web's by bytes, and one JVM runs a wiring per
 * country while the other runs one country. The names and help text stay with
 * each tier, since they are what a reader of that dashboard needs; the wiring of
 * "read this field off each registered cache, skip the ones that cannot answer"
 * is the same sentence twice and lives here.
 *
 * The skip is the load-bearing part. A cache that has no maximum, or counts no
 * hits, publishes NO SERIES for that field rather than a zero — see
 * [[CacheOccupancy]] for why a zero maximum is the wrong alarm to invent.
 *
 * Read at SCRAPE time: `read` runs inside the callback, so it must stay cheap and
 * must not throw.
 */
object CacheGauge {

  /** One registered cache: whose it is, what to call it, and how to read it. */
  final case class Registration(country: String, cache: String, read: () => CacheOccupancy)

  def register(registry: PrometheusRegistry, name: String, help: String,
               unit: Option[PrometheusUnit] = None)
              (field: CacheOccupancy => Option[Double], registrations: () => Seq[Registration]): Unit = {
    val builder = GaugeWithCallback.builder().name(name).help(help).labelNames("country", "cache")
    unit.foreach(builder.unit)
    builder
      .callback { callback =>
        registrations().foreach { registration =>
          field(registration.read()).foreach(value => callback.call(value, registration.country, registration.cache))
        }
      }
      .register(registry)
  }
}
