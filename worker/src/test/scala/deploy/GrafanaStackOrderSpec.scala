package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the worker HTTP-outcomes panels' "failure budget" reading.
 *
 * The panels exist to show the failure budget: the error bands are only legible
 * when they stack together at the FLOOR of the chart, so the stack height is the
 * total error share and the empty space above is the clean remainder. An earlier
 * revision drew a full-height `clean %` band at the bottom, which shoved every
 * error band up to float in the middle against a moving baseline — exactly the
 * arrangement that made the errors unreadable and got the separate raw-rate panel
 * deleted. `clean %` was therefore dropped entirely: reintroducing it (or any
 * `outcome="success"` series) re-floats the error bands, so this locks its
 * absence — a change with no visible compile or test consequence otherwise.
 *
 * It also guards the phase split: there are TWO such panels — one over all phases
 * and one filtered to `phase="scrape"` (cinema-site scraping only) — and the
 * scrape panel must actually carry that filter, or it silently duplicates the
 * all-phases panel.
 */
class GrafanaStackOrderSpec extends AnyFlatSpec with Matchers {

  /** ASCII-only slices of the two panel titles — the provisioning JSON escapes
   *  non-ASCII (the em-dash is written `—`), so matching a literal one fails. */
  private val AllPhasesTitleFragment = "error share, all phases (per country)"
  private val ScrapePhaseTitleFragment = "error share, scrape phase (per country)"

  private val dashboard = RepoFile.read("fly/grafana/provisioning/dashboards/worker-diagnostics.json")

  /** One panel's raw JSON block, located by an ASCII fragment of its title and
   *  bounded at the next panel's `"id":` so a neighbour's targets never leak in. */
  private def panelBlockTitled(titleFragment: String): String = {
    val start = dashboard.indexOf(titleFragment)
    withClue(s"no panel title containing '$titleFragment': ") { start should be >= 0 }
    val end = dashboard.indexOf("\n      \"id\":", start) match {
      case -1 => dashboard.length
      case i  => i
    }
    dashboard.substring(start, end)
  }

  /** The `legendFormat` values of one panel's targets, in declaration order. */
  private def legendsOfPanelTitled(titleFragment: String): Seq[String] =
    """"legendFormat": "([^"]*)"""".r.findAllMatchIn(panelBlockTitled(titleFragment)).map(_.group(1)).toSeq

  "the all-phases HTTP outcomes panel" should "stack only error bands, with no clean%/success baseline" in {
    val legends = legendsOfPanelTitled(AllPhasesTitleFragment)
    legends should contain ("{{outcome}}")
    legends should not contain "clean %"
    panelBlockTitled(AllPhasesTitleFragment) should not include "outcome=\\\"success\\\""
    // The retired panel showed these same categories as a raw rate, with no
    // denominator — one panel with the share is strictly more informative.
    dashboard should not include "Scraper HTTP failures by category"
  }

  "the scrape-phase HTTP outcomes panel" should "stack error bands filtered to phase=scrape, no clean% baseline" in {
    val block = panelBlockTitled(ScrapePhaseTitleFragment)
    legendsOfPanelTitled(ScrapePhaseTitleFragment) should contain ("{{outcome}}")
    legendsOfPanelTitled(ScrapePhaseTitleFragment) should not contain "clean %"
    // The whole point of the second panel: it must isolate the scrape phase.
    block should include ("phase=\\\"scrape\\\"")
    block should not include "outcome=\\\"success\\\""
  }
}
