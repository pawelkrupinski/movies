package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{ShadowIdentityMetrics, ShadowRelation}

/**
 * The identity resolver's shadow run (`ShadowIdentityReaper`, docs/design/identity-resolver.md §8),
 * each gauge with a leading `country` label and written by that country's run after every resolve:
 *
 *  - `kinowo_worker_identity_shadow_films{relation}` — the resolver's clusters by how they relate
 *    to the pipeline's films (`ShadowRelation`: identical, split, merged, moved). The share of
 *    identical is the shadow's agreement with today; the rest is the diff `/admin/identity` and the
 *    `identity_shadow_diff` collection itemise;
 *  - `kinowo_worker_identity_family_crossings` — constraint edges that crossed a family in the last
 *    resolve. ZERO IS HEALTHY: a crossing means a rule was added without its block key, and the
 *    resolve was refused;
 *  - `kinowo_worker_identity_resolve_seconds` — how long the last resolve took.
 *
 * Nothing is seeded: a country whose shadow run is off exports no series, rather than a zero that
 * reads as "the resolver agrees with nothing".
 */
final class IdentityShadowMetrics(registry: PrometheusRegistry) {

  private val films: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_shadow_films")
    .help("The identity resolver's shadow clusters in the last resolve, by relation to the pipeline's films " +
      "(identical / split / merged / moved).")
    .labelNames("country", "relation")
    .register(registry)

  private val crossingCount: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_family_crossings")
    .help("Constraint edges crossing a family in the last shadow resolve (0 is healthy; >0 refuses the resolve).")
    .labelNames("country")
    .register(registry)

  private val seconds: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_resolve_seconds")
    .help("Wall-clock seconds the last shadow resolve took.")
    .labelNames("country")
    .register(registry)

  def forCountry(country: String): ShadowIdentityMetrics = new ShadowIdentityMetrics {
    def resolved(counts: Map[ShadowRelation, Int], resolveSeconds: Double): Unit = {
      ShadowRelation.values.foreach(r => films.labelValues(country, r.label).set(counts.getOrElse(r, 0).toDouble))
      seconds.labelValues(country).set(resolveSeconds)
    }
    def crossings(count: Int): Unit = crossingCount.labelValues(country).set(count.toDouble)
  }
}
