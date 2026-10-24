package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.ResolverDecision
import services.identity.adapters.IdentityStagingPublisher

/**
 * Every take the identity projection last wrote, by basis (`ResolverDecision.Basis`: every way a cluster is matched or
 * left unmatched), as the staging publisher reads them every 20 s (`IdentityStagingPublisher`):
 * `kinowo_worker_identity_takes{basis}` / `kinowo_worker_identity_take_listings{basis}` — the clusters, and their
 * listings, each basis holds now. Every basis is exported at 0 from boot. How the takes CHANGE, by the same basis, is the
 * projection's change list's counter (`kinowo_worker_identity_take_changes_total{basis,kind}`, `IdentityProjectionGauges`).
 */
final class IdentityTakeMetrics(registry: PrometheusRegistry) {

  private val clusters: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_takes")
    .help("Identity clusters by the basis of their take (matched or unmatched), as the projection last wrote them.")
    .labelNames("country", "basis").register(registry)

  private val listings: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_take_listings")
    .help("Listings in identity clusters, by the basis of their cluster's take, as the projection last wrote them.")
    .labelNames("country", "basis").register(registry)

  private val bases: Seq[String] = ResolverDecision.Basis.values.toSeq.map(_.toString)

  /** `country`'s series, every basis touched at 0. */
  def forCountry(country: String): IdentityStagingPublisher.TakeMetrics = {
    bases.foreach { basis => clusters.labelValues(country, basis); listings.labelValues(country, basis) }
    (byClusters, byListings) => (bases ++ byClusters.keys).distinct.foreach { basis =>
      clusters.labelValues(country, basis).set(byClusters.getOrElse(basis, 0).toDouble)
      listings.labelValues(country, basis).set(byListings.getOrElse(basis, 0).toDouble)
    }
  }
}
