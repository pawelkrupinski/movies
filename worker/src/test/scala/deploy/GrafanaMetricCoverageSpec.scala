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
    "fly/grafana/provisioning/dashboards/fly-overview.json",
    "fly/grafana/provisioning/dashboards/worker-diagnostics.json"
  )

  /** Worker metrics deliberately exported without a panel, each with the reason.
   *  Empty on purpose — see the class comment. */
  private val UnchartedOnPurpose: Map[String, String] = Map.empty

  /** Families the web app renders by hand (`controllers.MetricsController`), not
   *  through a registry this module can enumerate. */
  private val WebExportedFamilies = Seq(
    "kinowo_web_movies_served",
    "kinowo_uptime_recent_successes",
    "kinowo_uptime_recent_failures",
    "kinowo_uptime_recent_zeroes"
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

  "every web-exported metric family" should "be charted too" in {
    WebExportedFamilies.foreach { family =>
      withClue(s"$family is exported by the web app's /metrics and charted nowhere. ") {
        chartedIn(family) shouldBe true
      }
    }
  }
}
