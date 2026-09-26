package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{ShadowIdentityMetrics, ShadowLookupMetrics, ShadowLookupRound, ShadowRelation}

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
 *  - `kinowo_worker_identity_resolve_seconds` — how long the last resolve took;
 *  - `kinowo_worker_identity_shadow_lookups{outcome}` — the paced live lookup fill's last round
 *    (`ShadowLookupFill`): `asked` live, of those `answered` and `failed`, and `deferred` (gaps it
 *    had no budget left for), plus `rate`, the asks per minute it ran at (below the configured cap
 *    while backed off). Deferred falling to zero is the fill catching up.
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

  private val lookups: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_shadow_lookups")
    .help("The identity shadow run's live lookup fill, last round: asked / answered / failed / deferred, and the rate " +
      "(asks per minute) it ran at.")
    .labelNames("country", "outcome")
    .register(registry)

  def lookupsForCountry(country: String): ShadowLookupMetrics = (r: ShadowLookupRound) => {
    Seq("asked" -> r.asked, "answered" -> r.answered, "failed" -> r.failed, "deferred" -> r.deferred, "rate" -> r.rate.perMinute)
      .foreach { case (outcome, n) => lookups.labelValues(country, outcome).set(n.toDouble) }
  }

  def forCountry(country: String): ShadowIdentityMetrics = new ShadowIdentityMetrics {
    def resolved(counts: Map[ShadowRelation, Int], resolveSeconds: Double): Unit = {
      ShadowRelation.values.foreach(r => films.labelValues(country, r.label).set(counts.getOrElse(r, 0).toDouble))
      seconds.labelValues(country).set(resolveSeconds)
    }
    def crossings(count: Int): Unit = crossingCount.labelValues(country).set(count.toDouble)
  }
}
