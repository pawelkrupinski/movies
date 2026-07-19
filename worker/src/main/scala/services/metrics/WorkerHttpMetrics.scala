package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import tools.{HttpOutcome, HttpOutcomeRecorder}

/**
 * `kinowo_worker_http_total` — one counter incremented per OUTBOUND HTTP attempt
 * the worker makes, labelled by `country`, `phase` and `outcome` ([[HttpOutcome]]:
 * success / http_429 / http_4xx / http_5xx / timeout / connection_error / other).
 *
 * Fed by [[tools.CountingHttpFetch]] wired innermost in each country's fetch
 * chain, so EVERY attempt counts once — every 429 retry and every scraper-level
 * backoff retry is its own entry, which is the whole point: it surfaces the
 * throttling and failure taxonomy per country that the cinema scrape hits.
 *
 * The `phase` label (see [[WorkerHttpMetrics.Phase]]) splits the two very
 * different populations that used to be lumped together: `scrape` is the
 * cinema-site HTTP (every listing scrape, chunk scrape and per-film detail
 * fetch), `enrich` is the third-party metadata/rating/resolution APIs
 * (TMDB/IMDb/RT/Metacritic/Filmweb/Trakt/Letterboxd/Wikidata/Cinemeta). They fail
 * for opposite reasons — a rising scrape 403 is a cinema blocking us, while a
 * steady enrich 404 is the RT/Metacritic resolvers probing candidate slugs by
 * design — so a Grafana panel filtered to one phase reads a failure budget the
 * combined line would blur. [[modules.WorkerWiring]] builds one full fetch chain
 * per phase, differing ONLY at this counter's `phase` label.
 *
 * Registered ONCE on the shared [[WorkerMetrics]] registry with a leading
 * `country` label (like every other `kinowo_worker_*` series), and seeded to 0
 * for the full country × phase × outcome grid so no Grafana line starts with a gap.
 */
class WorkerHttpMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`, so the name is declared without it.
  private val requests: Counter = Counter.builder()
    .name("kinowo_worker_http")
    .help("Outbound HTTP attempts the worker made since boot, by country, phase " +
      "(scrape = cinema-site scraping incl. chunk & per-film detail; enrich = third-party " +
      "metadata/rating/resolution APIs) and outcome (success / http_429 / http_4xx / http_5xx / " +
      "timeout / connection_error / other). Every retry is a separate attempt.")
    .labelNames("country", "phase", "outcome")
    .register(registry)

  seed()

  /** Materialize every (country, phase, outcome) at 0 so each Grafana line exists
   *  from boot rather than popping in when a category first fires. */
  private def seed(): Unit =
    for (c <- countryCodes; p <- WorkerHttpMetrics.Phase.all; o <- HttpOutcome.all)
      requests.labelValues(c, p, o)

  /** A recorder bound to one country and call [[WorkerHttpMetrics.Phase]] — what a
   *  [[modules.WorkerWiring]] injects into that phase's [[tools.CountingHttpFetch]]. */
  def recorderFor(country: String, phase: String): HttpOutcomeRecorder =
    (outcome: String) => requests.labelValues(country, phase, outcome).inc()
}

object WorkerHttpMetrics {

  /** The call-phase label values: which POPULATION of outbound HTTP an attempt
   *  belongs to. Kept a small closed set so the seed grid and the Grafana panel
   *  filters agree on the exact strings. */
  object Phase {
    /** Cinema-site HTTP: listing scrapes, chunk scrapes, per-film detail fetches. */
    val Scrape = "scrape"
    /** Third-party metadata/rating/resolution APIs (TMDB/IMDb/RT/Metacritic/
     *  Filmweb/Trakt/Letterboxd/Wikidata/Cinemeta). */
    val Enrich = "enrich"

    /** Every phase, for seeding the metric at 0. */
    val all: Seq[String] = Seq(Scrape, Enrich)
  }
}
