package services.metrics

import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream

/**
 * Locks that the worker registers the standard JVM + process resource collectors
 * onto its metrics registry, so `/metrics` carries the CPU/memory health signals
 * (not business data). Asserts the cross-platform MXBean-based series (memory,
 * GC, threads); `process_cpu_seconds_total` rides the same registration but comes
 * from ProcessMetrics via /proc, so it's Linux-only (present on Fly, absent on a
 * macOS dev box) and isn't asserted here.
 */
class JvmProcessMetricsSpec extends AnyFlatSpec with Matchers {

  private def scrape(registry: PrometheusRegistry): String = {
    val out = new ByteArrayOutputStream()
    PrometheusTextFormatWriter.create().write(out, registry.scrape())
    out.toString("UTF-8")
  }

  "JvmProcessMetrics.register" should "add the JVM resource series to the registry" in {
    val registry = new PrometheusRegistry()

    // Before registration the registry is empty of these series.
    scrape(registry) should not include "jvm_memory_used_bytes"

    JvmProcessMetrics.register(registry)

    val out = scrape(registry)
    out should include ("jvm_memory_used_bytes")
    out should include ("jvm_threads")
    out should include ("jvm_gc")
  }
}
