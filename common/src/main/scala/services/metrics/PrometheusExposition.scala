package services.metrics

import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry

import java.io.ByteArrayOutputStream

/** Renders a [[PrometheusRegistry]] as Prometheus text exposition (version
 *  0.0.4) — the body both apps' `/metrics` endpoints serve.
 *
 *  Shared because the scrape-and-write shape is identical on both sides of the
 *  fleet: the worker's health server (`WorkerTaskMetrics.Series.render`) and the
 *  web app's `MetricsController` each hold a registry and need its current
 *  snapshot as text. The writer is stateless and thread-safe, so one instance
 *  serves every caller. */
object PrometheusExposition {

  private val writer = PrometheusTextFormatWriter.create()

  /** Scrape `registry` and return its current snapshot as exposition text. */
  def render(registry: PrometheusRegistry): String = {
    val out = new ByteArrayOutputStream()
    writer.write(out, registry.scrape())
    out.toString("UTF-8")
  }
}
