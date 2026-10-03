package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{ShadowLookupMetrics, ShadowLookupRound}

/**
 * The identity model's paced live lookup fill (`ShadowLookupFill`, docs/design/identity-resolver.md §19),
 * a gauge with a leading `country` label written after every round:
 *
 *  - `kinowo_worker_identity_shadow_lookups{outcome}` — the last round: `asked` live, of those `answered`
 *    and `failed`, and `deferred` (gaps it had no budget left for), plus `rate`, the asks per minute it ran
 *    at (below the configured cap while backed off). Deferred falling to zero is the fill catching up.
 *
 * Nothing is seeded: a country that has run no round exports no series.
 */
final class IdentityShadowMetrics(registry: PrometheusRegistry) {

  private val lookups: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_shadow_lookups")
    .help("The identity model's live lookup fill, last round: asked / answered / failed / deferred, and the rate " +
      "(asks per minute) it ran at.")
    .labelNames("country", "outcome")
    .register(registry)

  def lookupsForCountry(country: String): ShadowLookupMetrics = (r: ShadowLookupRound) => {
    Seq("asked" -> r.asked, "answered" -> r.answered, "failed" -> r.failed, "deferred" -> r.deferred, "rate" -> r.rate.perMinute)
      .foreach { case (outcome, n) => lookups.labelValues(country, outcome).set(n.toDouble) }
  }
}
