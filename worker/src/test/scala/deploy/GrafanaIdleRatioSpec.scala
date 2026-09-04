package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards ratio panels against fabricating a value for a window with no traffic.
 *
 * `a / clamp_min(b, 1e-9)` was the house idiom for "don't render NaN when the
 * denominator is 0". But a counter that nobody incremented still reports its
 * old value on every scrape, so `rate(...)` over an idle window is 0, not
 * absent — the clamp turns 0/0 into a hard 0 and Grafana plots it as a real
 * sample. The panel then says the worker had a 0% success rate, or (for
 * `1 - fast/total`) that 100% of requests were slow, in every minute it simply
 * did nothing.
 *
 * That is not cosmetic: the legend's `mean` averages those fabricated samples
 * in. On 2026-09-04 the "Outbound success rate" panel read uk at 44.9% mean
 * over three hours. Measured against Prometheus directly, uk succeeded on
 * 1249 of 1378 outbound fetches (90.7%) in that window — 44% of the panel's
 * buckets were idle and scored 0. us, the other low-volume country, sat at the
 * same 44%. pl, de and es, which fetch continuously, were unaffected — so the
 * artefact reads exactly like a country-specific outage and isn't one.
 *
 * The fix is `a / (b > 0)`: the filter drops the denominator series when it is
 * 0, the division finds no match, and the idle window is a GAP. Real zeroes
 * (traffic that all failed) still plot as 0, because then the denominator is
 * positive.
 *
 * Dashboards only. An ALERT rule may legitimately clamp — there a missing
 * sample means NoData, whose handling is a separate policy decision (see the
 * alert-rules promtool suite under `infra/test/alert-rules`).
 */
class GrafanaIdleRatioSpec extends AnyFlatSpec with Matchers {

  /** `/ clamp_min(` — a clamp used as a DIVISOR, in any spacing. */
  private val ClampedDivisor = """/\s*clamp_min\(""".r

  private def dashboards(): Seq[java.io.File] =
    Option(new java.io.File("infra/nix/files/monitoring/grafana/dashboards/apps").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(_.getName.endsWith(".json"))
      .sortBy(_.getName)
      .toSeq

  "every provisioned dashboard" should "gap an idle ratio rather than clamp its denominator to a fake zero" in {
    val files = dashboards()
    files.size should be > 1 // otherwise the sweep is vacuous

    val offenders = files.flatMap { file =>
      val text = RepoFile.read(file.getPath)
      ClampedDivisor.findAllIn(text).map(_ => file.getName).toSeq
    }

    withClue(
      s"""These dashboards divide by a clamped denominator, so an idle window plots as a
         |real sample (0%, or 100% for a `1 - share` panel) and the legend mean averages it in:
         |
         |  ${offenders.distinct.mkString("\n  ")}
         |
         |Write `a / (b > 0)` instead — the filter drops a zero denominator, the division
         |finds no match, and the window is a gap.
         |""".stripMargin) {
      offenders shouldBe empty
    }
  }
}
