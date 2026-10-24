package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.ResolverDecision
import services.identity.adapters.IdentityStagingPublisher

/**
 * Every take the identity projection last wrote, by basis (`ResolverDecision.Basis`: every way a cluster is matched or
 * left unmatched), as the staging publisher reads them every 20 s (`IdentityStagingPublisher`):
 *  - `kinowo_worker_identity_takes{basis}` / `kinowo_worker_identity_take_listings{basis}` — the clusters, and their
 *    listings, each basis holds now;
 *  - `kinowo_worker_identity_cluster_changes_total{basis,change}` — each cluster since the last publish `decided` (new),
 *    `settled` (seen waiting first), or whose film moved: `gained` (had none), `lost` (has none now) or `changed`; by the
 *    basis it holds after. Counted from the first publish after a boot, which only records.
 * Every basis is exported at 0 from boot.
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

  private val changes: Counter = Counter.builder()
    .name("kinowo_worker_identity_cluster_changes_total")
    .help("Identity takes decided, settled, or whose film was gained, lost or changed, by the basis they hold after.")
    .labelNames("country", "basis", "change").register(registry)

  private val bases: Seq[String] = ResolverDecision.Basis.values.toSeq.map(_.toString)

  /** `country`'s series, every basis touched at 0. */
  def forCountry(country: String): IdentityStagingPublisher.TakeMetrics = {
    bases.foreach { basis => clusters.labelValues(country, basis); listings.labelValues(country, basis) }
    new IdentityStagingPublisher.TakeMetrics {
      def held(byClusters: Map[String, Int], byListings: Map[String, Int]): Unit = (bases ++ byClusters.keys).distinct.foreach { basis =>
        clusters.labelValues(country, basis).set(byClusters.getOrElse(basis, 0).toDouble)
        listings.labelValues(country, basis).set(byListings.getOrElse(basis, 0).toDouble)
      }
      def changed(basis: String, change: String): Unit = changes.labelValues(country, basis, change).inc()
    }
  }
}
