package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the alert coverage over SLOT VOLUME, the axis the film-count alerts
 * are structurally blind to.
 *
 * 2026-07-27: the UK lost 73% of its upcoming showtimes over six hours
 * (152,315 → ~40,000; London 22,066 → 9,956) while its film count ROSE over the
 * same window, 1,271 → 1,559 rows. Nothing paged. It was noticed by eye, hours
 * later, on the `kinowo — total upcoming showtimes served` panel — whose own
 * description names this exact failure ("a cliff here with the film count flat
 * means slots were lost without a whole film disappearing") while no rule
 * watched it.
 *
 * Films UP while slots crater is the part that makes this its own alert rather
 * than a tuning of the film-count rules: a scrape outage takes the films with
 * the slots, so this shape can only come from the slot path itself.
 *
 * Three properties have to hold together, and each maps to a way the existing
 * film-count rules missed it:
 *
 *   - WATCHED AT ALL. `kinowo-movies-served-swing` and
 *     `kinowo-movies-served-city-empty` both read `kinowo_web_movies_served` —
 *     distinct FILMS. A chain can drop every evening slot of every film it
 *     carries and the film count is unchanged, so no film-count rule can see
 *     this class of loss. It needs its own metric.
 *
 *   - COUNTRY-SCOPED. The fleet total is dominated by whichever country is
 *     largest; the UK's collapse was a 27% dent in `sum(kinowo_worker_showtimes)`
 *     while PL and DE were untouched (ratios 0.86 and 0.99 against their own
 *     baselines at the time, versus the UK's 0.23). Summed fleet-wide it reads
 *     as noise; summed per country it is unmissable.
 *
 *   - A BASELINE LONGER THAN THE INCIDENT. `kinowo-movies-served-swing`
 *     compares against `avg_over_time(...[1h])`, so a bleed lasting more than
 *     an hour drags its own reference down with it and the test never trips —
 *     that rule was live and silent throughout. A multi-hour window is the
 *     whole point, hence the >= 6h floor asserted below.
 *
 * The threshold and window numbers themselves are deliberately NOT pinned here
 * (tuning them is an alerting-noise judgement, not a drift bug) — only that a
 * slot-volume rule exists, is per country, and cannot be blinded by a slow
 * bleed.
 */
class GrafanaShowtimeVolumeAlertSpec extends AnyFlatSpec with Matchers {

  private val AlertRules = "fly/grafana/provisioning/alerting/alert-rules.yaml"

  private lazy val alertRules = RepoFile.read(AlertRules)

  /** Every `expr:` line in the provisioning file, unquoted. */
  private lazy val expressions: Seq[String] =
    """(?m)^\s*expr:\s*'(.*)'\s*$""".r
      .findAllMatchIn(alertRules)
      .map(_.group(1))
      .toSeq

  /** The expressions that alert on the worker's per-city slot-volume gauge. */
  private lazy val showtimeExpressions: Seq[String] =
    expressions.filter(_.contains("kinowo_worker_showtimes"))

  "the alert rules" should "watch the showtime-volume gauge, not only the film count" in {
    withClue(
      "no alert rule reads kinowo_worker_showtimes. The film-count rules " +
        "(kinowo-movies-served-swing, kinowo-movies-served-city-empty) count DISTINCT FILMS, " +
        "so a cinema dropping every slot of every film it carries moves none of them — which " +
        s"is how the 2026-07-27 UK slot collapse ran six hours unpaged. Add a rule to $AlertRules. "
    ) {
      showtimeExpressions should not be empty
    }
  }

  it should "scope the showtime-volume alert per country" in {
    showtimeExpressions.foreach { expr =>
      withClue(
        s"'$expr' does not aggregate `by (country)`. A fleet-wide sum hides a single country's " +
          "collapse behind the other countries' healthy volume — the UK's 63% loss was only a " +
          "27% dent in the fleet total. Aggregate with `sum by (country) (...)`. "
      ) {
        expr should include("by (country)")
      }
    }
  }

  it should "compare the showtime volume against a baseline longer than a slow bleed" in {
    val BaselineWindow = """\[(\d+)h""".r

    showtimeExpressions.foreach { expr =>
      val windows = BaselineWindow.findAllMatchIn(expr).map(_.group(1).toInt).toSeq

      withClue(
        s"'$expr' has no multi-hour lookback window. A baseline shorter than the incident " +
          "follows the decline down and the comparison never trips — exactly how " +
          "kinowo-movies-served-swing's `avg_over_time(...[1h])` stayed silent while the UK bled " +
          "out over six hours. "
      ) {
        windows should not be empty
        windows.max should be >= 6
      }
    }
  }
}
