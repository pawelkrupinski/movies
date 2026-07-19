package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import tools.{HttpOutcome, HttpOutcomeRecorder}

/**
 * `kinowo_worker_http_total` — one counter incremented per OUTBOUND HTTP attempt
 * the worker makes, labelled by `country` and `outcome` ([[HttpOutcome]]:
 * success / http_429 / http_4xx / http_5xx / timeout / connection_error / other).
 *
 * Fed by [[tools.CountingHttpFetch]] wired innermost in each country's fetch
 * chain, so EVERY attempt counts once — every 429 retry and every scraper-level
 * backoff retry is its own entry, which is the whole point: it surfaces the
 * throttling and failure taxonomy per country that the cinema scrape hits.
 *
 * Scope note: this is ALL worker outbound HTTP, not scraping alone — cinema
 * scraping is ~10x the enrichment (TMDB/IMDb/…) request volume, so the failure
 * lines are dominated by scraping, but a TMDB 429 also lands here under
 * `http_429`. Splitting scrape vs enrichment would need a `kind` label; left as a
 * follow-up since the categories and per-country breakdown are what was asked.
 *
 * Registered ONCE on the shared [[WorkerMetrics]] registry with a leading
 * `country` label (like every other `kinowo_worker_*` series), and seeded to 0
 * for the full country × outcome grid so no Grafana line starts with a gap.
 */
class WorkerHttpMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`, so the name is declared without it.
  private val requests: Counter = Counter.builder()
    .name("kinowo_worker_http")
    .help("Outbound HTTP attempts the worker made since boot, by country and outcome " +
      "(success / http_429 / http_4xx / http_5xx / timeout / connection_error / other). " +
      "Every retry is a separate attempt. Dominated by cinema scraping.")
    .labelNames("country", "outcome")
    .register(registry)

  seed()

  /** Materialize every (country, outcome) at 0 so each Grafana line exists from
   *  boot rather than popping in when a category first fires. */
  private def seed(): Unit =
    countryCodes.foreach(c => HttpOutcome.all.foreach(o => requests.labelValues(c, o)))

  /** A recorder bound to one country — what a [[modules.WorkerWiring]] injects
   *  into its [[tools.CountingHttpFetch]]. */
  def recorderFor(country: String): HttpOutcomeRecorder =
    (outcome: String) => requests.labelValues(country, outcome).inc()
}
