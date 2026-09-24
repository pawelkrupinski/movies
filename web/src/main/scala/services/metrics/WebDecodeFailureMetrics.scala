package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.readmodel.DecodeFailureMetrics

/**
 * `kinowo_web_decode_failures_total{country, collection}` — documents the web's read-model
 * scans skipped as undecodable: a film or screening every pod serving from that scan goes
 * without. The worker counts its own scans under `kinowo_worker_decode_failures_total`;
 * `DocumentsUndecodable` (worker-pipeline.rules) alerts on either. Seeded at 0.
 */
class WebDecodeFailureMetrics(registry: PrometheusRegistry, country: String) extends DecodeFailureMetrics {

  // The client auto-appends `_total`.
  private val skipped: Counter = Counter.builder()
    .name("kinowo_web_decode_failures")
    .help("Documents a read-model scan SKIPPED because they could not be decoded, by country and collection " +
      "(web_movies|web_screenings). ZERO IS THE HEALTHY READING: each is a film or screening the web serves without.")
    .labelNames("country", "collection")
    .register(registry)

  DecodeFailureMetrics.Collections.foreach(c => skipped.labelValues(country, c))

  def recordDecodeFailure(collection: String): Unit = skipped.labelValues(country, collection).inc()
}
