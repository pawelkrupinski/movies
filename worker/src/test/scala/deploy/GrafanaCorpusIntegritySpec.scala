package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{CorpusScanMetrics, PrometheusExposition, WorkerCorpusMetrics, WorkerCorpusScan, WorkerMetrics, WorkerShowtimesMetrics, WorkerSourceFilmsMetrics}

/**
 * Guards the dashboard/alert coverage over the CORPUS-INTEGRITY event families —
 * the ones the `sourceData` work (the `movie_slots` split and the 2026-07-27
 * failed-read investigation) either created or made load-bearing.
 *
 * Two shifts happened under the existing panels, neither of which changed a
 * single query:
 *
 *  - THE CENSUS GAUGES NOW FREEZE INSTEAD OF COLLAPSING. Before, a corpus scan
 *    that failed mid-way published what it had managed to read, so a read fault
 *    showed up as a cliff on `kinowo_worker_corpus_movies` / `_showtimes` /
 *    `_movies_served` — ugly, but visible, and the showtime-collapse alert could
 *    trip on it. Since the census skips its publish on an incomplete pass, the
 *    same fault now leaves every one of those gauges sitting at its last good
 *    value: flat, plausible, and indistinguishable from a quiet corpus. A frozen
 *    gauge also holds a constant ratio against its own `max_over_time` baseline,
 *    so `kinowo-showtime-volume-collapsed` cannot fire on it either. The ONLY
 *    remaining signal is `kinowo_worker_corpus_scan_incomplete_total` — which
 *    makes charting AND alerting on it a hard requirement, not a nicety. Without
 *    it the 2026-07-27 shape (`Missing field: sourceData` failing every batch for
 *    ~50 minutes) is invisible on every panel and every rule we own.
 *
 *  - THE LOSS-SIDE EVENTS BECAME THE INTERESTING ONES. With slots living in
 *    `movie_slots` and the read model projected from them, a film losing its
 *    showtimes is no longer one write you can read off the change-stream panels:
 *    it is a read-model prune, a reconcile sweep that removed something, or a
 *    change the incremental stream missed entirely and the backstop rehydrate had
 *    to catch. Those three counters existed and were charted nowhere, so the
 *    dashboards showed the corpus's SIZE but never the events that shrink it.
 *
 * The metric names are asserted against a real registry render first, so a rename
 * in the metric code fails here rather than silently emptying a panel — which is
 * the failure mode a hardcoded name string in a JSON dashboard has by nature.
 */
class GrafanaCorpusIntegritySpec extends AnyFlatSpec with Matchers {

  private val FlyOverview = "infra/nix/files/monitoring/grafana/dashboards/apps/fly-overview.json"
  private val AlertRules  = "infra/nix/files/monitoring/grafana/alerting/alert-rules.yaml"

  private lazy val dashboard  = RepoFile.read(FlyOverview)
  private lazy val alertRules = RepoFile.read(AlertRules)

  /** The worker's real metric names, as the `/metrics` endpoint exposes them.
   *  The census families carry a `country` label and only materialize once a
   *  wiring binds a per-country sampler to them, so bind one here too — otherwise
   *  they'd be absent from the render for a reason that has nothing to do with
   *  the naming this spec is checking. */
  private lazy val exposed: String = {
    val metrics = WorkerMetrics.singleCountry(Country.Poland, poolSize = 1)
    new WorkerCorpusMetrics(metrics.corpusGauge, Country.Poland.code)
    new WorkerSourceFilmsMetrics(metrics.servedGauge, Country.Poland.code)
    new WorkerShowtimesMetrics(metrics.showtimesGauge, Country.Poland.code)
    CorpusScanMetrics.prometheus(metrics.corpusScanIncomplete, Country.Poland.code)
    PrometheusExposition.render(metrics.registry)
  }

  /** The three census gauges, all of which now publish NOTHING on an incomplete
   *  corpus pass and therefore go stale-but-plausible rather than collapsing. */
  private val censusGauges = Seq(
    WorkerCorpusMetrics.Name,      // kinowo_worker_corpus_movies
    WorkerShowtimesMetrics.Name,   // kinowo_worker_showtimes
    WorkerSourceFilmsMetrics.Name  // kinowo_worker_movies_served
  )

  /** The events that REMOVE things — a film's derived docs pruned, a sweep that
   *  actually pruned, a row the change stream missed. Names spelled here as the
   *  metric code registers them (they're inline in WorkerTaskMetrics.Series, so
   *  `mustBeExposed` is what keeps this list honest). */
  private val lossSideEvents = Seq(
    "kinowo_worker_readmodel_films_pruned",
    "kinowo_worker_readmodel_reconcile_sweeps",
    "kinowo_worker_cache_rehydrate_changes",
    "kinowo_worker_readmodel_metadata_projections"
  )

  private def mustBeExposed(name: String): Unit =
    withClue(s"$name is not on the worker's /metrics endpoint — the name this spec and the " +
      "dashboards use has drifted from the metric code. ") {
      exposed should include(name)
    }

  private def mustBeCharted(name: String): Unit = {
    mustBeExposed(name)
    withClue(s"no panel in $FlyOverview queries $name. ") {
      dashboard should include(name)
    }
  }

  "the corpus census gauges" should "each be charted" in {
    censusGauges.foreach(mustBeCharted)
  }

  "the census freshness counter" should "be charted alongside the gauges it qualifies" in {
    withClue(
      s"${WorkerCorpusScan.IncompleteMetricName} is charted nowhere. The census gauges " +
        s"(${censusGauges.mkString(", ")}) deliberately publish NOTHING on a pass that could not " +
        "read the whole corpus — they hold their last complete values — so a flat line on those " +
        "panels means EITHER a quiet corpus OR a census that has stopped reading, and the two are " +
        "indistinguishable without this counter. Chart it next to them. "
    ) {
      mustBeCharted(WorkerCorpusScan.IncompleteMetricName)
    }
  }

  it should "have an alert rule of its own" in {
    withClue(
      s"no alert rule reads ${WorkerCorpusScan.IncompleteMetricName}. Freezing the gauges on a " +
        "failed read also disarmed the rules that watched them: kinowo-showtime-volume-collapsed " +
        "compares kinowo_worker_showtimes against its own max_over_time baseline, and a frozen " +
        "gauge holds that ratio at 1.0 forever. So a total read failure now pages nobody unless " +
        s"this counter does it. Add a rule to $AlertRules. "
    ) {
      alertRules should include(WorkerCorpusScan.IncompleteMetricName)
    }
  }

  "the loss-side corpus events" should "be charted, not just the corpus size" in {
    lossSideEvents.foreach { name =>
      withClue(
        s"$name is exposed but charted nowhere. With the slots split out into movie_slots, a film " +
          "losing its showtimes shows up as a read-model prune / a reconcile sweep that removed " +
          "something / a row the incremental change stream missed — none of which the corpus-size " +
          "gauges or the change-stream op panels can show. "
      ) {
        mustBeCharted(name)
      }
    }
  }
}
