package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.adapters.{LookupFillMetrics, LookupFillRound}

/**
 * The identity model's paced live lookup fill (`LookupFill`, docs/design/identity-resolver.md §19),
 * a gauge with a leading `country` label written after every round:
 *
 *  - `kinowo_worker_identity_shadow_lookups{outcome}` — the last round: `asked` live, of those `answered`
 *    and `failed`, and `deferred` (gaps it had no budget left for), plus `rate`, the asks per minute it ran
 *    at (below the configured cap while backed off). Deferred falling to zero is the fill catching up.
 *
 * Every country's outcomes exist from boot at NaN — no round run yet: a last round's 0 would claim one ran and
 * asked nothing.
 */
final class IdentityLookupFillMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val lookups: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_shadow_lookups")
    .help("The identity model's live lookup fill, last round: asked / answered / failed / deferred, and the rate " +
      "(asks per minute) it ran at.")
    .labelNames("country", "outcome")
    .register(registry)

  for (c <- countryCodes; outcome <- IdentityLookupFillMetrics.Outcomes) lookups.labelValues(c, outcome).set(Double.NaN)

  def lookupsForCountry(country: String): LookupFillMetrics = (r: LookupFillRound) => {
    IdentityLookupFillMetrics.Outcomes.zip(Seq(r.asked.toDouble, r.answered.toDouble, r.failed.toDouble, r.deferred.toDouble, r.rate.perMinute.toDouble))
      .foreach { case (outcome, n) => lookups.labelValues(country, outcome).set(n.toDouble) }
  }
}

object IdentityLookupFillMetrics {
  /** The `outcome` values, in the order a round reports them. */
  val Outcomes: Seq[String] = Seq("asked", "answered", "failed", "deferred", "rate")
}
