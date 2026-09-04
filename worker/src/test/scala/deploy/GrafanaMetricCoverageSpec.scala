package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerMetrics

import scala.jdk.CollectionConverters._

/**
 * Every metric we pay to export must appear on a dashboard.
 *
 * An exported-but-uncharted metric is the worst of both worlds: it costs a
 * sample, a series and a scrape, it reads in code review as "we have visibility
 * on this", and it shows nobody anything. They accumulate silently, because
 * nothing fails when a metric is registered and then never charted — which is
 * how this repo ended up carrying nine of them at once, including
 * `kinowo_worker_native_offbook_gap_bytes` (whose own doc comment calls it "the
 * primary signal" for the ~5-6h native OOM), `kinowo_worker_rating_resolved_not_run`
 * ("so Grafana can alert on it" — Grafana had never heard of it), and
 * `kinowo_worker_corpus_scan_incomplete_total`, the only surviving evidence that
 * the census gauges have gone stale.
 *
 * The worker's family list is derived from a real registry scrape rather than
 * written out here, so a metric added tomorrow is covered by this guard the
 * moment it is registered — the author either charts it or records WHY not in
 * [[UnchartedOnPurpose]]. That map is deliberately empty: there is currently no
 * worker metric worth exporting and not worth drawing, and an empty map is the
 * honest default. Adding an entry is a decision someone has to write down.
 *
 * Names are compared against the BASE family name (`kinowo_worker_tasks_started`),
 * which is a prefix of every form a dashboard can spell — `_total` for counters,
 * `_bucket`/`_sum`/`_count` for histograms.
 *
 * Scope: the worker registry (mechanically enumerable) plus the handful of
 * families the WEB app hand-renders, which are listed explicitly here because
 * they come from `MetricsController`'s own text exposition rather than a
 * Prometheus registry this module can scrape. Their NAMES are guarded on the web
 * side (`MetricsControllerSpec`, `WebMovieMetricsSpec`); what's guarded here is
 * that a dashboard draws them.
 */
class GrafanaMetricCoverageSpec extends AnyFlatSpec with Matchers {

  private val Dashboards = Seq(
    // THE LIVE DASHBOARDS -- the ones monitoring-1's Grafana provisions. They used to have a
    // frozen twin under fly/grafana/provisioning/dashboards for the stopped kinowo-grafana app,
    // and guarding THAT would have meant this spec passing while the dashboards people actually
    // open had no panel for a metric. The twin is gone; there is one copy to guard.
    "infra/nix/files/monitoring/grafana/dashboards/apps/application-health.json",
    "infra/nix/files/monitoring/grafana/dashboards/apps/worker-diagnostics.json",
    "infra/nix/files/monitoring/grafana/dashboards/fleet/kinowo-fleet.json",
    // Added 2026-08-29 with the HTTP dashboard. EVERY live dashboard has to be listed here or the
    // spec's guarantee inverts: a metric charted only on an unlisted dashboard reads as uncharted,
    // and someone "fixes" that by adding a duplicate panel to a listed one.
    "infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json"
  )

  /** Worker metrics deliberately exported without a panel, each with the reason.
   *  Empty on purpose — see the class comment. */
  private val UnchartedOnPurpose: Map[String, String] = Map.empty

  /** Families the web app exports that this module cannot enumerate — the ones
   *  `controllers.MetricsController` renders by hand, plus the ones registered on
   *  the web's own Prometheus registry, which lives in a module the worker does
   *  not depend on. Listed by name here so the coverage guarantee still reaches
   *  them; their NAMES are guarded on the web side (`MetricsControllerSpec`,
   *  `WebMovieMetricsSpec`, `HttpMetricsFilterSpec`, `WebHostMetricsSpec`) and
   *  what's guarded here is that a dashboard draws them. */
  private val WebExportedFamilies = Seq(
    "kinowo_web_movies_served",
    // Added 2026-08-29 with the HTTP filter. These two are the web tier's replacement for Fly's
    // proxy metrics (fly_app_http_*), which died with the Fly Prometheus token -- and unlike them
    // they measure the application's own work rather than the edge in front of it.
    "kinowo_web_http_requests_total",
    "kinowo_web_http_request_duration_seconds",
    // Added 2026-08-29 alongside them: the web MACHINE's free RAM and free disk, read by the
    // process from its own kernel. Same cause -- fly_instance_memory_* and fly_volume_* died with
    // the Fly Prometheus token, and nothing scrapes the Fly host at all.
    "kinowo_web_host_memory_available_bytes",
    "kinowo_web_host_memory_total_bytes",
    "kinowo_web_host_disk_free_bytes",
    "kinowo_web_host_disk_total_bytes",
    "kinowo_uptime_recent_successes",
    "kinowo_uptime_recent_failures",
    "kinowo_uptime_recent_zeroes"
  )

  /** `kinowo_*` families exported by the FLEET rather than by either application —
   *  written by a shell script into node_exporter's textfile directory, so no
   *  registry in this build can enumerate them. Listed here so the reverse guard
   *  below does not read a perfectly live panel as dangling.
   *
   *  Both come from `mongodumpScript` / `publishDumpMetrics` in
   *  nix/modules/roles/mongodb.nix on mongo-1: the backup timer is the only thing that
   *  knows a dump happened, and a dump that silently stopped is invisible everywhere
   *  else. */
  private val FleetExportedFamilies = Seq(
    "kinowo_mongodump_last_success_timestamp_seconds",
    "kinowo_mongodump_last_size_bytes"
  )

  /** Every `kinowo_worker_*` family the worker registers, base names, straight
   *  from the registry — NOT from the text exposition, which omits a family that
   *  has no data points yet and would quietly under-report the very metrics most
   *  likely to be forgotten. */
  private lazy val workerFamilies: Seq[String] =
    WorkerMetrics
      .singleCountry(Country.Poland, poolSize = 1)
      .registry
      .scrape()
      .asScala
      .map(_.getMetadata.getPrometheusName)
      .filter(_.startsWith("kinowo_"))
      .toSeq
      .distinct
      .sorted

  private lazy val allDashboardJson: String = Dashboards.map(RepoFile.read).mkString("\n")

  private def chartedIn(family: String): Boolean = allDashboardJson.contains(family)

  "every worker metric family" should "be charted on a dashboard, or recorded as deliberately uncharted" in {
    workerFamilies should not be empty // a broken enumeration must not pass vacuously

    val orphans = workerFamilies.filterNot(chartedIn).filterNot(UnchartedOnPurpose.contains)

    withClue(
      s"exported but drawn nowhere: ${orphans.mkString(", ")}. Every one of these costs a series on " +
        "every scrape and shows nobody anything. Add a panel to one of " + Dashboards.mkString(" / ") +
        ", or add the name to UnchartedOnPurpose with the reason it is worth exporting and not worth " +
        "drawing. "
    ) {
      orphans shouldBe empty
    }
  }

  it should "not carry a stale exemption for a metric that is charted after all" in {
    val stale = UnchartedOnPurpose.keys.filter(chartedIn)
    withClue(s"charted, so the exemption is now misleading: ${stale.mkString(", ")}. ") {
      stale shouldBe empty
    }
  }

  it should "not exempt a metric that no longer exists" in {
    val gone = UnchartedOnPurpose.keys.filterNot(workerFamilies.contains)
    withClue(s"exempted but not registered any more: ${gone.mkString(", ")}. ") {
      gone shouldBe empty
    }
  }

  /**
   * Charted is not the same as visible, and both of these were caught only by
   * querying the live data after the panels shipped:
   *
   *  - `kinowo_worker_native_offbook_gap_bytes` is RSS MINUS NMT-committed, and
   *    committed routinely exceeds resident (committed pages that were never
   *    touched aren't resident), so on a healthy worker it sits at roughly
   *    −200 to −320 MB. A panel with `min: 0` clips every one of those points
   *    and draws an empty chart — charted, provisioned, guarded by the coverage
   *    check above, and showing nothing.
   *
   *  - `kinowo_uptime_recent_zeroes` is per-SERVICE across ~2,800 services, of
   *    which ~120 are legitimately empty at any moment. One series each is 120
   *    lines of spaghetti; the readable signal is how MANY services are empty,
   *    which is what moves when something breaks.
   */
  "the off-book native memory panel" should "not floor its axis at zero on a routinely-negative series" in {
    val panel = panelBlockContaining("kinowo_worker_native_offbook_gap_bytes")
    withClue(
      "the off-book gap is RSS minus NMT-committed and is normally NEGATIVE (committed > resident); " +
        "a min:0 axis clips the whole series and draws an empty panel. "
    ) {
      panel should not include "\"min\": 0"
    }
  }

  "the empty-uptime-checks panel" should "aggregate services rather than drawing one line each" in {
    val panel = panelBlockContaining("kinowo_uptime_recent_zeroes")
    withClue(
      "~120 of ~2,800 services report an empty listing at any moment, so a per-service query draws " +
        "~120 overlapping lines. Aggregate (count) so the panel shows the population, which is the " +
        "thing that moves when a venue stops returning results. "
    ) {
      panel should include("count(")
      panel should not include "by (service)"
    }
  }

  /** One panel's raw JSON, located by a query it runs and bounded at the next
   *  panel's `"id":` so a neighbour's config never leaks into the assertion. */
  private def panelBlockContaining(expr: String): String = {
    val json  = RepoFile.read(Dashboards.find(d => RepoFile.read(d).contains(expr)).getOrElse(
      fail(s"no dashboard queries $expr")))
    val start = json.lastIndexOf("\"id\":", json.indexOf(expr))
    val end   = json.indexOf("\"id\":", json.indexOf(expr)) match {
      case -1 => json.length
      case i  => i
    }
    json.substring(start, end)
  }

  "every web-exported metric family" should "be charted too" in {
    WebExportedFamilies.foreach { family =>
      withClue(s"$family is exported by the web app's /metrics and charted nowhere. ") {
        chartedIn(family) shouldBe true
      }
    }
  }

  /**
   * AND THE OTHER DIRECTION, which is the one that breaks silently.
   *
   * Everything above asks "is this exported metric drawn?" — a panel too few. The
   * failure that actually ships is a panel too many: a dashboard querying a
   * `kinowo_*` family nothing exports any more. Prometheus answers an unknown metric
   * with an empty result, not an error, so the panel renders "No data" and looks
   * exactly like a quiet period. A TEMPLATE VARIABLE built on one is worse — its
   * dropdown empties, every panel scoped by it matches nothing, and a whole dashboard
   * goes blank at once.
   *
   * Both happened here on 2026-09-04: deleting the worker's throttle path removed
   * `kinowo_worker_throttled`, which was charted on one panel AND was the
   * `label_values(...)` source for the Country dropdown on TWO dashboards. Nothing in
   * this spec noticed, because it only ever looked for orphaned metrics, never
   * orphaned queries.
   */
  "every kinowo_ metric a dashboard queries" should "actually be exported by something" in {
    val exported = (workerFamilies ++ WebExportedFamilies ++ FleetExportedFamilies).distinct
    val queried  = MetricReference.findAllMatchIn(allDashboardJson).map(_.group(0)).toSeq.distinct.sorted

    queried should not be empty // a broken regex must not pass vacuously

    // A dashboard spells a family in several forms: `_total` on a counter,
    // `_bucket`/`_sum`/`_count` on a histogram, `_created` on either. Match on the
    // BASE family the way `chartedIn` does, from the other side.
    val dangling = queried.filterNot(q => exported.exists(f => q == f || q.startsWith(f + "_")))

    withClue(
      s"queried by a panel or template variable but exported by nothing: ${dangling.mkString(", ")}. " +
        "Prometheus answers an unknown metric with an empty result rather than an error, so each of " +
        "these is a panel that reads as a quiet period — or, if it backs a `label_values` variable, " +
        "a dashboard that goes blank. Either restore the metric or repoint the query. "
    ) {
      dangling shouldBe empty
    }
  }

  /** Every `kinowo_*` identifier appearing anywhere in a dashboard — panel targets and
   *  template-variable queries alike, since both break the same way. Deliberately not
   *  parsed as PromQL: a regex over the raw JSON cannot miss a spelling the parser
   *  would, and over-matching is caught by the base-family comparison above. */
  private val MetricReference = raw"kinowo_[a-z0-9_]+".r
}
