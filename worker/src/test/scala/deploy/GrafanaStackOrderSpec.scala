package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the stacking ORDER of the worker HTTP outcomes panel.
 *
 * Grafana stacks frames in the order its targets return them, so the LAST
 * target is drawn on top. That is load-bearing here rather than cosmetic: the
 * panel exists to show the failure budget, and the error bands are only
 * readable when they sit together at the floor of the chart with the ~90-99%
 * `clean %` band above them. Put `clean %` first and it becomes the bottom
 * band, shoving every error band up to float in the middle of the plot against
 * a moving baseline — which is exactly the arrangement that made the errors
 * unreadable and got the separate raw-rate panel deleted.
 *
 * A reordering is a one-line JSON edit with no visible compile or test
 * consequence, hence this lock.
 */
class GrafanaStackOrderSpec extends AnyFlatSpec with Matchers {

  /** ASCII-only slice of the panel title — see `legendsOfPanelTitled`. */
  private val PanelTitleFragment = "clean % over error share (per country)"

  private val dashboard = RepoFile.read("fly/grafana/provisioning/dashboards/worker-diagnostics.json")

  /** The `legendFormat` values of one panel's targets, in declaration order.
   *  Read straight off the raw JSON: the panel is located by an ASCII fragment
   *  of its title, and the legends follow in document order within that panel's
   *  block. The fragment must be ASCII — the provisioning JSON escapes non-ASCII
   *  (the title's em-dash is written `—`), so matching a literal one fails. */
  private def legendsOfPanelTitled(titleFragment: String): Seq[String] = {
    val start = dashboard.indexOf(titleFragment)
    withClue(s"no panel title containing '$titleFragment': ") { start should be >= 0 }
    // Bound the scan at the next panel's `"id":` so we never read a neighbour's targets.
    val end = dashboard.indexOf("\n      \"id\":", start) match {
      case -1 => dashboard.length
      case i  => i
    }
    """"legendFormat": "([^"]*)"""".r.findAllMatchIn(dashboard.substring(start, end)).map(_.group(1)).toSeq
  }

  "the worker HTTP outcomes panel" should "stack `clean %` last so it rides above the error bands" in {
    val legends = legendsOfPanelTitled(PanelTitleFragment)
    legends should contain ("clean %")
    withClue(s"targets in order: ${legends.mkString(" -> ")}; ") {
      legends.last shouldBe "clean %"
    }
  }

  it should "draw the error breakdown beneath it, not as a separate panel" in {
    val legends = legendsOfPanelTitled(PanelTitleFragment)
    legends should contain ("{{outcome}}")
    // The retired panel showed these same categories as a raw rate, with no
    // denominator — one panel with both is strictly more informative.
    dashboard should not include "Scraper HTTP failures by category"
  }
}
