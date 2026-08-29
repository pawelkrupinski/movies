package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * The web app's Prometheus registry, carrying the JVM + process resource
 * collectors — the web-side counterpart to the worker's [[WorkerMetrics]].
 *
 * WHY: until this existed the web app exported only hand-rendered business
 * gauges (`kinowo_uptime_*`, `kinowo_web_movies_served`), so every JVM question
 * about it — heap headroom against `-Xmx384m`, GC pauses, non-heap creep, thread
 * count — was answerable only through Fly's host metrics, which see machine RAM
 * and not the heap inside it. The worker has had `jvm_*` / `process_*` since the
 * CPU-credit investigation; this gives the Fly-health dashboard the same panel
 * for web.
 *
 * No `country` label: these are process-level facts about one JVM, and a web
 * deployment already serves exactly one country per Fly app (`KINOWO_COUNTRY`),
 * so the scrape's `app` label identifies the country's process on its own (see
 * fly/grafana/victoria/scrape.yml).
 *
 * One instance per process, built in [[modules.Wiring]] — the collector names
 * may be registered only once per registry.
 *
 * The registry is the PROCESS-WIDE one, not a JVM-only one: [[WebHttpMetrics]]
 * registers `kinowo_web_http_*` on it and [[WebHostMetrics]] registers
 * `kinowo_web_host_*`, so [[render]] emits all of them and the `/metrics` body
 * keeps its single shape. Anything else that needs a Prometheus metric on the
 * web side belongs here as well — a second registry would need a second thing
 * appended to the endpoint for no gain.
 */
class WebJvmMetrics {

  val registry: PrometheusRegistry = new PrometheusRegistry()

  JvmProcessMetrics.register(registry)

  /** The registry's current snapshot as Prometheus text exposition — `jvm_*`,
   *  `process_*` AND the `kinowo_web_http_*` request families — appended to the
   *  `/metrics` body by [[controllers.MetricsController]]. */
  def render(): String = PrometheusExposition.render(registry)
}
