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
 * It also guards the phase split: the HTTP outcomes now live as TWO per-phase
 * panels — `phase="scrape"` (cinema-site scraping) and `phase="enrich"` (the
 * third-party metadata/rating/resolution APIs) — with NO combined all-phases
 * panel. Each must actually carry its own `phase=` filter, or it silently
 * duplicates the other.
 */
class GrafanaStackOrderSpec extends AnyFlatSpec with Matchers {

  /** ASCII-only slices of the two panel titles — the provisioning JSON escapes
   *  non-ASCII (the em-dash is written `—`), so matching a literal one fails. */
  private val ScrapePhaseTitleFragment = "error share, scrape phase (per country)"
  private val EnrichPhaseTitleFragment = "error share, enrich phase (per country)"

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

  private def assertPhasePanel(titleFragment: String, phase: String): Unit = {
    val block = panelBlockTitled(titleFragment)
    legendsOfPanelTitled(titleFragment) should contain ("{{outcome}}")
    legendsOfPanelTitled(titleFragment) should not contain "clean %"
    // The whole point of a per-phase panel: it must isolate its own phase.
    block should include (s"""phase=\\"$phase\\"""")
    block should not include "outcome=\\\"success\\\""
  }

  "the scrape-phase HTTP outcomes panel" should "stack error bands filtered to phase=scrape, no clean% baseline" in {
    assertPhasePanel(ScrapePhaseTitleFragment, "scrape")
  }

  "the enrich-phase HTTP outcomes panel" should "stack error bands filtered to phase=enrich, no clean% baseline" in {
    assertPhasePanel(EnrichPhaseTitleFragment, "enrich")
  }

  "the HTTP outcomes panels" should "have no combined all-phases panel and not resurrect the retired raw-rate one" in {
    // Dropping the all-phases panel is the point of this revision — its query
    // summed over `phase` and blurred the two populations. Lock it out.
    dashboard should not include "all phases (per country)"
    // The even-earlier raw-rate failures panel stayed retired throughout.
    dashboard should not include "Scraper HTTP failures by category"
  }
}
