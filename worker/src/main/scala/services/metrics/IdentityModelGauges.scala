package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{IdentityModelMetrics, ModelBatch}

/**
 * The incremental identity model (`IdentityModelService`), per country:
 *
 *  - `kinowo_worker_identity_model_families` — families the model holds after its last drain;
 *  - `kinowo_worker_identity_model_resolved_total` — families re-resolved, summed over drains: the
 *    work the pipeline's events cost (rate() of it is the model's load);
 *  - `kinowo_worker_identity_model_events_total` — venue scrapes and observations drained, by kind;
 *  - `kinowo_worker_identity_model_drain_seconds` — how long the last drain that did work took;
 *  - `kinowo_worker_identity_model_rebuilds_total` — drains that failed and rebuilt the model from
 *    its store (0 is healthy);
 *  - `kinowo_worker_identity_model_takeup_failures_total` — take-ups that failed, each leaving the
 *    model down until a retry succeeds (0 is healthy);
 *  - `kinowo_worker_identity_model_largest_family_listings` / `_largest_family_nodes` — the largest
 *    family's listings and the most evidence nodes any family holds;
 *  - `kinowo_worker_identity_model_large_families` — families larger than one region: each
 *    resolves slowly whenever anything in it moves (0 is healthy).
 *
 * Nothing is seeded: a country whose model is off exports no series.
 */
final class IdentityModelGauges(registry: PrometheusRegistry) {

  private val families: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_families")
    .help("Families the incremental identity model holds after its last drain.")
    .labelNames("country")
    .register(registry)

  private val resolved: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_resolved_total")
    .help("Families the incremental identity model re-resolved, summed over its drains.")
    .labelNames("country")
    .register(registry)

  private val events: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_events_total")
    .help("Events the incremental identity model drained, by kind (venue scrapes, observations).")
    .labelNames("country", "kind")
    .register(registry)

  private val seconds: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_drain_seconds")
    .help("Wall-clock seconds the model's last working drain took.")
    .labelNames("country")
    .register(registry)

  private val rebuilds: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_rebuilds_total")
    .help("Drains that failed and rebuilt the incremental identity model from its store (0 is healthy).")
    .labelNames("country")
    .register(registry)

  private val takeUpFailures: Counter = Counter.builder()
    .name("kinowo_worker_identity_model_takeup_failures_total")
    .help("Take-ups of the incremental identity model that failed, leaving it down until a retry succeeds (0 is healthy).")
    .labelNames("country")
    .register(registry)

  private val largestListings: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_largest_family_listings")
    .help("Listings in the incremental identity model's largest family.")
    .labelNames("country")
    .register(registry)

  private val largestNodes: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_largest_family_nodes")
    .help("The most evidence nodes any family of the incremental identity model holds.")
    .labelNames("country")
    .register(registry)

  private val large: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_model_large_families")
    .help("Families of the incremental identity model larger than one region (0 is healthy).")
    .labelNames("country")
    .register(registry)

  def forCountry(country: String): IdentityModelMetrics = new IdentityModelMetrics {
    def batch(batch: ModelBatch): Unit = {
      families.labelValues(country).set(batch.families.toDouble)
      resolved.labelValues(country).inc(batch.familiesResolved.toDouble)
      events.labelValues(country, "venue").inc(batch.venues.toDouble)
      events.labelValues(country, "observation").inc(batch.observations.toDouble)
      seconds.labelValues(country).set(batch.seconds)
      largestListings.labelValues(country).set(batch.sizes.largestListings.toDouble)
      largestNodes.labelValues(country).set(batch.sizes.largestNodes.toDouble)
      large.labelValues(country).set(batch.sizes.large.toDouble)
    }
    def rebuilt(): Unit = rebuilds.labelValues(country).inc()
    def takeUpFailed(): Unit = takeUpFailures.labelValues(country).inc()
  }
}
