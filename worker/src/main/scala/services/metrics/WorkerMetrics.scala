package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
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

  // Intern-pool occupancy and eviction — also process-level: StringPool is one
  // object shared by every country's wiring, so a `country` label would be a lie.
  StringPoolMetrics.register(registry)

  // In-process cache occupancy (`kinowo_worker_cache_*`), one labelled family for
  // every cache any country's wiring registers. Registered once here; the callback
  // reads whatever is in `cacheRegistrations` at scrape time, so a wiring built
  // later still surfaces without re-registering a metric name.
  private val cacheRegistrations =
    new java.util.concurrent.CopyOnWriteArrayList[WorkerCacheMetrics.Registration]()
  WorkerCacheMetrics.register(registry, () => {
    import scala.jdk.CollectionConverters._
    cacheRegistrations.asScala.toSeq
  })

  /** Publish a cache's occupancy under `kinowo_worker_cache_*{country,cache}`.
   *  `read` is called on every scrape, so it must be cheap and must not throw. */
  def registerCache(country: String, cache: String, read: () => CacheOccupancy): Unit =
    cacheRegistrations.add(WorkerCacheMetrics.Registration(country, cache, read))

  // The registered-once task-pipeline metric objects, shared across countries.
  val taskSeries: WorkerTaskMetrics.Series = new WorkerTaskMetrics.Series(poolSize, countryCodes, registry)

  // Per-attempt outbound-HTTP outcome counter (kinowo_worker_http_total), one
  // registered-once family with a leading `country` label; each wiring binds its
  // own country's recorder into the innermost fetch decorator.
  val httpMetrics: WorkerHttpMetrics = new WorkerHttpMetrics(countryCodes, registry)

  // Per-attempt identity-resolution counter (kinowo_worker_resolution_total) —
  // what each per-source resolution cache saved or had to run. Same shape: one
  // registered-once family, each wiring binds its own country's recorders.
  val resolutionMetrics: WorkerResolutionMetrics = new WorkerResolutionMetrics(countryCodes, registry)

  // Census gauges, each registered once with a leading `country` label; a
  // per-country sampler (built in the wiring) writes its own slice.
  val corpusGauge:    Gauge          = WorkerCorpusMetrics.gauge(registry)
  val servedGauge:    Gauge          = WorkerSourceFilmsMetrics.gauge(registry)
  val showtimesGauge: Gauge          = WorkerShowtimesMetrics.gauge(registry)
  val widestSlotsGauge: Gauge        = WorkerSlotFanoutMetrics.gauge(registry)
  val (ratingNotRunGauge, ratingOldestAgeGauge) = RatingRunCensus.gauges(registry)
  val (scrapeOldestAgeGauge, scrapeNeverScrapedGauge) = CinemaScrapeCensus.gauges(registry)
  val (contentOldestAgeGauge, neverContentGauge) = CinemaContentCensus.gauges(registry)

  // Counts census passes that could not read the whole corpus. The gauges above publish
  // NOTHING on such a pass (a partial count is indistinguishable from a real collapse),
  // so this counter is what keeps a stuck census visible instead of frozen-and-plausible.
  val corpusScanIncomplete: Counter = WorkerCorpusScan.incompleteCounter(registry)

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
