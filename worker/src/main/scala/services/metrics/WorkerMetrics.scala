package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country

/**
 * The process-wide worker metrics bundle: ONE Prometheus registry and ONE set of
 * metric objects, shared across every country's [[modules.WorkerWiring]] in the
 * single JVM.
 *
 * WHY ONE registry: the worker now runs a wiring per country in one process, but
 * Fly scrapes a single `/metrics` endpoint. A metric name may be registered only
 * once per registry, so the countries can't each own a registry with the same
 * `kinowo_worker_*` names — instead every worker metric is registered here once
 * with a leading `country` label, and each country's wiring writes only its own
 * `country="…"` slice. That way ALL countries' series surface on the one endpoint
 * (fixing the earlier "primary country's registry only, others headless" gap).
 *
 * JVM/process resource metrics (`process_*`, `jvm_*`) and the native-memory
 * sampler are genuinely process-level (one JVM for all countries), so they are
 * registered here ONCE and carry NO country label.
 *
 * Built once in [[modules.WorkerMain]] and injected into every wiring; a
 * single-country boot / test uses [[WorkerMetrics.singleCountry]].
 */
class WorkerMetrics(countryCodes: Seq[String], poolSize: Int) {

  val registry: PrometheusRegistry = new PrometheusRegistry()

  // Process/JVM resource metrics (process CPU, RSS, GC, threads) — process-level,
  // registered once, no country label.
  JvmProcessMetrics.register(registry)

  // Native-memory + vitals sampler — one JVM, so a single process-level sampler.
  val jvmVitals: JvmVitalsSampler = new JvmVitalsSampler(registry)

  // The registered-once task-pipeline metric objects, shared across countries.
  val taskSeries: WorkerTaskMetrics.Series = new WorkerTaskMetrics.Series(poolSize, countryCodes, registry)

  // Per-attempt outbound-HTTP outcome counter (kinowo_worker_http_total), one
  // registered-once family with a leading `country` label; each wiring binds its
  // own country's recorder into the innermost fetch decorator.
  val httpMetrics: WorkerHttpMetrics = new WorkerHttpMetrics(countryCodes, registry)

  // Census gauges, each registered once with a leading `country` label; a
  // per-country sampler (built in the wiring) writes its own slice.
  val corpusGauge:    Gauge          = WorkerCorpusMetrics.gauge(registry)
  val servedGauge:    Gauge          = WorkerSourceFilmsMetrics.gauge(registry)
  val showtimesGauge: Gauge          = WorkerShowtimesMetrics.gauge(registry)
  val (ratingNotRunGauge, ratingOldestAgeGauge): (Gauge, Gauge) = RatingRunCensus.gauges(registry)

  /** The per-country task-metrics facade a wiring holds. Cheap — it just binds the
   *  country code to the shared [[taskSeries]]. */
  def taskMetricsFor(country: Country): WorkerTaskMetrics = new WorkerTaskMetrics(country.code, taskSeries)

  /** Start the process-level samplers (the per-country census samplers are started
   *  by each wiring). */
  def start(): Unit = jvmVitals.start()

  /** Stop the process-level samplers. */
  def stop(): Unit = jvmVitals.stop()
}

object WorkerMetrics {
  /** A single-country bundle — the default for a one-country boot and for the
   *  wiring/test constructs that don't inject a shared one. */
  def singleCountry(country: Country, poolSize: Int): WorkerMetrics =
    new WorkerMetrics(Seq(country.code), poolSize)
}
