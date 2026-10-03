package services.metrics

import io.prometheus.metrics.core.metrics.Histogram
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * How much heap one page render allocated, measured on the thread that produced the
 * response's bytes (`ResponseBody.measured`): building the page's schedules, rendering
 * it and encoding it — everything a render costs, nothing a neighbouring request does.
 *
 * WHY. A city listing is the web tier's biggest allocation by far: New York's cost
 * ~320 MB a render before 2026-10-02 and ~20 MB after, measured from outside as a burst
 * of renders against the pod's whole-JVM allocation counters, minus an idle window. That
 * is slow, noisy (the counters only move at a GC) and blind to where the rest goes;
 * recorded per render, the figure separates what a render costs from what the HTTP
 * server and the pod's background work do.
 */
class WebRenderMetrics(registry: PrometheusRegistry, country: String) {

  private val allocated: Histogram = Histogram.builder()
    .name("kinowo_web_page_render_allocated_bytes")
    .help("Heap one page render allocated on the thread producing its response bytes — schedules, " +
      "template and encoding — by country and page (`listing` is a city's repertoire). Cache hits " +
      "that render nothing are not recorded.")
    .labelNames("country", "page")
    .classicOnly()
    .classicUpperBounds(WebRenderMetrics.ByteBuckets*)
    .register(registry)

  def record(page: String, bytes: Long): Unit =
    allocated.labelValues(country, page).observe(bytes.toDouble)
}

object WebRenderMetrics {
  private val MB = 1024.0 * 1024
  /** 1 MB to 512 MB in doublings: one render of the smallest city to a worst case
   *  above what New York ever cost. */
  val ByteBuckets: Seq[Double] = (0 to 9).map(i => MB * math.pow(2, i))
}
