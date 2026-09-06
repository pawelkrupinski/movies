package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.MongoTtlIndex

/**
 * `kinowo_worker_ttl_index_mismatches` — TTL indexes whose expiry does not match
 * what the code asks for and which the app could not bring into line.
 *
 * WHY THIS EXISTS. `createIndex` cannot ALTER a TTL index, and `readWrite` does
 * not carry `collMod`, so for as long as this fleet has existed a change to a TTL
 * constant reached Mongo only if the collection had never been indexed. The
 * failure was logged at `logger.debug`, which is off in production, and the result
 * was invisible for months: `detailCache-cinema-city` was still reaping at 6h on
 * 2026-09-06 although `5096417e3` had set it to 2h the day before — and that
 * commit existed precisely because 6h against a 6h refresh window made half the
 * scheduled refreshes re-derive what they already had and stamp it as fresh.
 * Nothing anywhere said so; it surfaced only when [[MongoTtlIndex]] started
 * reading the expiry back.
 *
 * [[MongoTtlIndex]] now rebuilds a disagreeing index itself, within the privileges
 * it has. This gauge covers the case where even that fails — including the one that
 * actually costs something, a drop that succeeded and a create that did not,
 * leaving the collection with NO TTL index and growing.
 *
 * A COUNT RATHER THAN A LABELLED SERIES, deliberately. An alerting expression fires
 * on the presence of a sample rather than on its truth, so a per-collection series
 * that vanishes when healthy cannot be told from one that vanished because the
 * scrape did — the mistake `MongodNoPrimary` was written as. This is always
 * present and reads 0 when every index agrees. The collection names are in the WARN
 * lines `MongoTtlIndex` emits, which is where triage reads them.
 *
 * Process-level, like [[StringPoolMetrics]]: the reconciler is one `object` shared
 * by every country's wiring in the JVM, so a `country` label would be a lie.
 */
object TtlIndexMetrics {

  def register(registry: PrometheusRegistry): Unit =
    GaugeWithCallback.builder()
      .name("kinowo_worker_ttl_index_mismatches")
      .help("TTL indexes whose expireAfterSeconds disagrees with the code and which the app could " +
        "not rebuild. Zero is healthy. Above zero, a collection is either reaping on the wrong " +
        "schedule or — if a rebuild dropped the index and failed to recreate it — not reaping at all.")
      .callback(callback => callback.call(MongoTtlIndex.Mismatches.count.toDouble))
      .register(registry)
}
