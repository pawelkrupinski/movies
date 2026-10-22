package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.adapters.IdentityDriftCheck

/**
 * Each country's daily drift check (`IdentityDriftCheck`): its live takes against a whole resolve of the same listings.
 *
 *  - `kinowo_worker_identity_drift_listings{country,kind}` — listings whose live film the last check found the whole
 *    resolve takes otherwise, by what adopting it would do (`lost`, `gained`, `changed`). Zero when incremental = whole;
 *    the `IdentityIncrementalDrift` alert (infra/nix/files/monitoring/rules/identity-regressions.rules) reads it;
 *  - `kinowo_worker_identity_drift_unsettled{country}` — listings the whole resolve left waiting on an answer, not compared;
 *  - `kinowo_worker_identity_drift_checked_timestamp_seconds{country}` — when the last check ran: a stale one checks nothing.
 */
final class IdentityDriftGauges(registry: PrometheusRegistry) {
  private val listings: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_drift_listings")
    .help("Listings whose live identity take the last drift check found a whole resolve of the same listings takes otherwise.")
    .labelNames("country", "kind")
    .register(registry)

  private val unsettled: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_drift_unsettled")
    .help("Listings the last drift check's whole resolve left waiting on an answer (not compared).")
    .labelNames("country")
    .register(registry)

  private val checked: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_drift_checked_timestamp_seconds")
    .help("When the last identity drift check ran (epoch seconds).")
    .labelNames("country")
    .register(registry)

  def forCountry(country: String, clock: java.time.Clock): IdentityDriftCheck.Metrics = report => {
    val byKind = report.byKind
    Seq("lost", "gained", "changed").foreach(kind => listings.labelValues(country, kind).set(byKind.getOrElse(kind, 0).toDouble))
    unsettled.labelValues(country).set(report.unsettled.toDouble)
    checked.labelValues(country).set(clock.millis() / 1000.0)
  }
}
