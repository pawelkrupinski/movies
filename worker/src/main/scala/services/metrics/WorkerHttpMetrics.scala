package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import tools.{ExternalService, HttpOutcome, HttpOutcomeRecorder, HttpPath}

/**
 * `kinowo_worker_http_total` — one counter incremented per OUTBOUND HTTP attempt
 * the worker makes, labelled by `country`, `phase`, `service`, `path` and `outcome` ([[HttpOutcome]]:
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
 * (TMDB/IMDb/RT/Metacritic/Filmweb/Letterboxd/Wikidata/Cinemeta). They fail
 * for opposite reasons — a rising scrape 403 is a cinema blocking us, while a
 * steady enrich 404 is the RT/Metacritic resolvers probing candidate slugs by
 * design — so a Grafana panel filtered to one phase reads a failure budget the
 * combined line would blur. [[modules.WorkerWiring]] builds one full fetch chain
 * per phase, differing ONLY at this counter's `phase` label.
 *
 * The `service` label (see [[WorkerHttpMetrics.Service]]) names WHICH external
 * service within a phase, from the attempt's URL ([[tools.ExternalService]]):
 * on `enrich` each metadata API (omdb, tmdb, imdb, …) or `other`, on `scrape`
 * `cinema` for every cinema site — Filmweb's showtimes scraper included, it is
 * a cinema source there — or `telegram` for the alerting bot that shares that
 * chain. It is what tells an OMDb quota 401 from a TMDB key problem.
 *
 * The `path` label ([[tools.HttpPath]]) names WHO ASKED: `fill` (the identity model's lookup
 * fill), `live` (its live asks at take-up), `agreement` (the agreement stage's questions), `sweep`
 * (TMDB's change lists), `refresh` (aged questions asked again), `venue_page` (a venue detail page
 * read into venue_pages), `enrich` (the pipeline's ratings, IMDb-id resolution and projected films'
 * details), `scrape` (the scraping pipeline), or `other` for a request no caller named. The caller
 * scopes it around its work; the counter reads it on the requesting thread, so no signature carries
 * it. Per-path rate per country:
 * `sum by (country, path) (rate(kinowo_worker_http_total[5m]))`.
 *
 * Registered ONCE on the shared [[WorkerMetrics]] registry with a leading
 * `country` label (like every other `kinowo_worker_*` series), and seeded to 0
 * for the full country × (phase, service) × path × outcome grid so no Grafana line
 * starts with a gap. The grid is bounded: [[WorkerHttpMetrics.Service.grid]]
 * pairs each phase only with the services it can reach.
 */
class WorkerHttpMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`, so the name is declared without it.
  private val requests: Counter = Counter.builder()
    .name("kinowo_worker_http")
    .help("Outbound HTTP attempts the worker made since boot, by country, phase " +
      "(scrape = cinema-site scraping incl. chunk & per-film detail; enrich = third-party " +
      "metadata/rating/resolution APIs), service (the external service called: cinema / telegram " +
      "on scrape; omdb / tmdb / imdb / rottentomatoes / metacritic / filmweb / letterboxd / wikidata / " +
      "cinemeta / other on enrich), path (who asked: fill / live / agreement / sweep / refresh / venue_page / " +
      "enrich / scrape / other) and outcome (success / http_429 / http_4xx / http_5xx / " +
      "timeout / connection_error / other). Every retry is a separate attempt.")
    .labelNames("country", "phase", "outcome", "service", "path")
    .register(registry)

  seed()

  /** Materialize every (country, phase, service, path, outcome) at 0 so each Grafana line
   *  exists from boot rather than popping in when a category first fires. */
  private def seed(): Unit =
    for (c <- countryCodes; (p, s) <- WorkerHttpMetrics.Service.grid; path <- HttpPath.all; o <- HttpOutcome.all)
      requests.labelValues(c, p, o, s, path)

  /** A recorder bound to one country and call [[WorkerHttpMetrics.Phase]] — what a
   *  [[modules.WorkerWiring]] injects into that phase's [[tools.CountingHttpFetch]].
   *  The counter asks it [[HttpOutcomeRecorder.forRequest]] per attempt, which binds
   *  the `service` label, and [[HttpOutcomeRecorder.forPath]], which binds `path`; a bare
   *  `record` (no URL known, no path named) is the phase's default service and `other`. */
  def recorderFor(country: String, phase: String): HttpOutcomeRecorder =
    labelled(country, phase, WorkerHttpMetrics.Service.default(phase), HttpPath.Other)

  private def labelled(country: String, phase: String, service: String, path: String): HttpOutcomeRecorder =
    new HttpOutcomeRecorder {
      def record(outcome: String): Unit = requests.labelValues(country, phase, outcome, service, path).inc()
      override def forRequest(url: String): HttpOutcomeRecorder =
        labelled(country, phase, WorkerHttpMetrics.Service.of(phase, url), path)
      override def forPath(asked: String): HttpOutcomeRecorder = labelled(country, phase, service, asked)
    }

  /** Every cell's count now, whatever labels the family carries — what a CI recording leg dumps
   *  into its job summary and pushes beside production (tools.CiHttpCalls, scripts/ci/ci_http_calls.py). */
  def snapshot(): io.prometheus.metrics.model.snapshots.CounterSnapshot = requests.collect()
}

object WorkerHttpMetrics {

  /** The call-phase label values: which POPULATION of outbound HTTP an attempt
   *  belongs to. Kept a small closed set so the seed grid and the Grafana panel
   *  filters agree on the exact strings. */
  object Phase {
    /** Cinema-site HTTP: listing scrapes, chunk scrapes, per-film detail fetches. */
    val Scrape = "scrape"
    /** Third-party metadata/rating/resolution APIs (TMDB/IMDb/RT/Metacritic/
     *  Filmweb/Letterboxd/Wikidata/Cinemeta). */
    val Enrich = "enrich"

    /** Every phase, for seeding the metric at 0. */
    val all: Seq[String] = Seq(Scrape, Enrich)
  }

  /** The `service` label: which external service an attempt reached, within its phase. */
  object Service {
    /** Every (phase, service) pair the worker can produce — the seed grid. A scrape
     *  attempt is a cinema site or the Telegram bot; an enrich attempt one of the APIs
     *  or, for a host none of them names, `other`. */
    val grid: Seq[(String, String)] =
      Seq(Phase.Scrape -> ExternalService.Cinema, Phase.Scrape -> ExternalService.Telegram) ++
        (ExternalService.apis :+ ExternalService.Other).map(Phase.Enrich -> _)

    /** The service an attempt to `url` on `phase` is labelled with — a [[grid]] pair for the
     *  worker's own phases. Any other phase (a CI fill's `fill`) is named like `enrich`: it
     *  calls the same APIs. */
    def of(phase: String, url: String): String = {
      val named = ExternalService.ofUrl(url)
      if (phase == Phase.Scrape) named.filter(_ == ExternalService.Telegram).getOrElse(ExternalService.Cinema)
      else named.filter(ExternalService.apis.contains).getOrElse(ExternalService.Other)
    }

    /** The label for an attempt whose URL is not known. */
    def default(phase: String): String =
      if (phase == Phase.Scrape) ExternalService.Cinema else ExternalService.Other
  }
}
