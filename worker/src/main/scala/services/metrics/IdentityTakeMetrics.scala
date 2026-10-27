package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.ResolverDecision
import services.identity.adapters.IdentityStagingPublisher

/**
 * Every take the identity projection last wrote, by basis (`ResolverDecision.Basis`: every way a cluster is matched or
 * left unmatched), as the staging publisher reads them every 20 s (`IdentityStagingPublisher`):
 * `kinowo_worker_identity_takes{basis}` / `kinowo_worker_identity_take_listings{basis}` — the clusters, and their
 * listings, each basis holds now. Every basis is exported at 0 from boot. And
 * `kinowo_worker_identity_staging_dropped_total`: events the staging screen was never told (its feed full, or failed
 * publishes past what they keep) — 0 unless the screen misses something. How the takes CHANGE, by the same basis, is the
 * projection's change list's counter (`kinowo_worker_identity_take_changes_total{basis,kind}`, `IdentityProjectionGauges`).
 */
final class IdentityTakeMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val clusters: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_takes")
    .help("Identity clusters by the basis of their take (matched or unmatched), as the projection last wrote them.")
    .labelNames("country", "basis").register(registry)

  private val listings: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_take_listings")
    .help("Listings in identity clusters, by the basis of their cluster's take, as the projection last wrote them.")
    .labelNames("country", "basis").register(registry)

  private val dropped: Counter = Counter.builder()
    .name("kinowo_worker_identity_staging_dropped_total")
    .help("Identity staging events (take changes, waits) the staging publisher let go before writing them: 0 unless the staging screen misses something.")
    .labelNames("country").register(registry)

  private val bases: Seq[String] = ResolverDecision.Basis.values.toSeq.map(_.toString)

  /** `country`'s series, every basis touched at 0. */
  private def seed(country: String): Unit = {
    bases.foreach { basis => clusters.labelValues(country, basis); listings.labelValues(country, basis) }
    dropped.labelValues(country)
  }
  countryCodes.foreach(seed)

  def forCountry(country: String): IdentityStagingPublisher.TakeMetrics = {
    seed(country)
    new IdentityStagingPublisher.TakeMetrics {
      def held(byClusters: Map[String, Int], byListings: Map[String, Int]): Unit = (bases ++ byClusters.keys).distinct.foreach { basis =>
        clusters.labelValues(country, basis).set(byClusters.getOrElse(basis, 0).toDouble)
        listings.labelValues(country, basis).set(byListings.getOrElse(basis, 0).toDouble)
      }
      def dropped(events: Long): Unit = IdentityTakeMetrics.this.dropped.labelValues(country).inc(events.toDouble)
    }
  }
}
