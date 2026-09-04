package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Caps how much work ONE LOOK at the overview dashboard asks of the box that
 * renders it.
 *
 * `monitoring-1` is a 2-core cx23 that carries Prometheus, Grafana, Alertmanager,
 * VictoriaLogs AND the k3s control plane — a trade `server.monitoring.tf` makes
 * knowingly, warning that "a k3s control plane under load is the loudest
 * neighbour on a 2-core box". Its 7-day CPU average is 11.8%, so it is not
 * short of capacity; what it is short of is TOLERANCE FOR BURSTS. Load1 on that
 * host averages 0.40 and peaks at 10.44 — a run queue 5x the core count, from a
 * box that is otherwise idle.
 *
 * The burst is this dashboard. Every panel re-queries on a template-variable
 * change, so switching `country` fired all 71 panels at once and the switch
 * visibly hung. Grafana is OSS here, so there is no query caching to absorb a
 * repeat, and the datasource has no per-dashboard concurrency cap.
 *
 * Two settings bound it, and this spec holds both:
 *
 *  - COLLAPSED ROWS. Grafana does not query a panel inside a collapsed row until
 *    the row is opened, so collapsing the diagnostic rows takes the on-load and
 *    on-variable-change burst from 71 panels to the dozen in the two rows that
 *    answer "is it alive?". The detail is one click away and costs nothing until
 *    someone wants it.
 *  - REFRESH INTERVAL. `refresh` re-fires that same set on a timer for as long
 *    as a tab is open, whether or not anyone is looking. At 1m this dashboard
 *    was a permanent background load on the host it is meant to observe.
 *
 * This is deliberately a CEILING, not a fixed shape: add panels, add rows,
 * re-order them freely. It fails only when a change puts the whole burst back
 * on the front page.
 */
class GrafanaQueryBurstSpec extends AnyFlatSpec with Matchers {
  private val Dashboard = "infra/nix/files/monitoring/grafana/dashboards/apps/application-health.json"
  private lazy val json = RepoFile.read(Dashboard)

  /** Rows worth rendering unqueried-on-arrival. Everything below "what the two
   *  tiers are serving" is diagnosis, reached deliberately. */
  private val MinCollapsedRows = 8

  /** Fast enough to notice an incident on a dashboard someone is watching, slow
   *  enough that an abandoned tab is not a load generator. */
  private val ForbiddenRefreshes = Seq("5s", "10s", "30s", "1m", "2m")

  "the overview dashboard" should "collapse its diagnostic rows so a country switch does not fire every panel" in {
    val collapsed = "\"collapsed\": true".r.findAllMatchIn(json).size
    val rows      = "\"type\": \"row\"".r.findAllMatchIn(json).size

    withClue(
      s"$Dashboard renders $rows rows, only $collapsed of them collapsed. Grafana queries every " +
      "panel in an expanded row on load AND on every template-variable change, and this dashboard " +
      "is read on a 2-core box that also runs the k3s control plane: ") {
      collapsed should be >= MinCollapsedRows
    }
    info(s"$collapsed of $rows rows collapsed")
  }

  it should "not re-fire that burst on a fast timer" in {
    val refresh = "\"refresh\": \"([^\"]+)\"".r.findFirstMatchIn(json).map(_.group(1))
    withClue(s"$Dashboard declares no top-level `refresh`: ")(refresh should not be empty)

    withClue(
      s"$Dashboard auto-refreshes every ${refresh.get}. Every open tab then re-queries on that " +
      "timer forever, with no OSS query cache to absorb it: ") {
      ForbiddenRefreshes should not contain refresh.get
    }
    info(s"refresh = ${refresh.get}")
  }
}
