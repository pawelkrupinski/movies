package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards `kinowo-movies-served-city-empty`'s cause-1-vs-cause-2 gate.
 *
 * Before 2026-09-13 this alert fired on `kinowo_web_movies_served{city}==0` alone: the corpus-side
 * reading (`kinowo_worker_movies_served`, the same gauge `ReadModelServingDiffersFromCorpus` reads)
 * was pulled into the page as an annotation but never into `condition: C`, so it could change what
 * the page SAID without ever changing whether it fired. A three-incident sample on 2026-09-11 —
 * es/zamora, us/pueblo, us/havre, all confirmed against the worker log as thin venues or a
 * legitimate depth-guard hold, none of them bugs — paged `severity: critical` every time regardless.
 *
 * The fix folds the discriminant into query A itself: it now requires the SAME city's corpus
 * reading to be nonzero (`and on (city) (max by (city) (kinowo_worker_movies_served{scope="all"}) >
 * 0)`) before the alert can fire at all, so a city whose corpus has also run dry — cause (2), not a
 * bug — stays silent, and only a site short of a nonzero corpus — cause (1), the read-model defect
 * this alert exists to catch — still pages. Safe against a corpus OUTAGE rather than merely a quiet
 * one: `ReadModelServedGaugesAbsent` in read-model-projection.rules already pages separately if the
 * whole `kinowo_worker_movies_served` metric goes missing, so this rule losing sight of one thin
 * city inside an otherwise-healthy gauge is never the only alarm covering a real corpus outage.
 *
 * What the expression PRODUCES for both causes is pinned by feeding it series in
 * `infra/test/alert-rules/grafana-movies-served-city-empty.yml` — promtool cannot load a
 * Grafana-managed rule, but it will evaluate its `expr`. The last test here is what stops that
 * suite testing an expression production no longer runs.
 */
class GrafanaCityEmptyAlertSpec extends AnyFlatSpec with Matchers {

  private val AlertRules = "infra/nix/files/monitoring/grafana/alerting/alert-rules.yaml"

  /** The promtool suite that EVALUATES this rule's expression, rather than reading it. */
  private val PromtoolSuite = "infra/test/alert-rules/grafana-movies-served-city-empty.yml"

  private lazy val alertRules = RepoFile.read(AlertRules)

  /** Every `expr:` line in the provisioning file, unquoted. */
  private lazy val expressions: Seq[String] =
    """(?m)^\s*expr:\s*'(.*)'\s*$""".r
      .findAllMatchIn(alertRules)
      .map(_.group(1))
      .toSeq

  /** Query A of `kinowo-movies-served-city-empty` — the one `condition: C` thresholds on. */
  private lazy val cityEmptyExpressions: Seq[String] =
    expressions.filter(_.contains("""kinowo_web_movies_served{scope="all"} == 0"""))

  "the alert rules" should "still have exactly one query A for the city-empty alert" in {
    withClue(
      s"expected exactly one `expr:` containing kinowo_web_movies_served{scope=\"all\"} == 0 in " +
        s"$AlertRules (query A of kinowo-movies-served-city-empty); found ${cityEmptyExpressions.size}. " +
        "If the rule was rewritten, update this spec's filter to find its query A again."
    ) {
      cityEmptyExpressions should have size 1
    }
  }

  it should "require the corpus reading for the SAME city to be nonzero before the city-empty alert can fire" in {
    cityEmptyExpressions.foreach { expr =>
      withClue(
        s"'$expr' no longer gates on kinowo_worker_movies_served. Without " +
          "`and on (city) (max by (city) (kinowo_worker_movies_served{scope=\"all\"}) > 0)`, this " +
          "alert pages critical for cause (2) as well as cause (1) — a thin venue whose own " +
          "listing ran dry between scrapes (es/zamora, us/pueblo, us/havre, all 2026-09-11) is not " +
          "a bug, and this alert exists to catch cause (1): a corpus with films for a city that the " +
          "site is not serving. "
      ) {
        expr should include("and on (city) (max by (city) (kinowo_worker_movies_served")
        expr should include("> 0)")
      }
    }
  }

  it should "be evaluated, not merely read, by the promtool suite that claims to test it" in {
    val suite = RepoFile.read(PromtoolSuite)

    cityEmptyExpressions.foreach { expr =>
      withClue(
        s"$PromtoolSuite does not contain '$expr' verbatim, so it is pinning the value of some " +
          "OTHER expression than the one Grafana evaluates — and every case in it can pass while " +
          "the live rule is broken. Every assertion in this file reads the expression; only that " +
          "suite feeds it series and asks what number comes out, which is the only thing that has " +
          "ever actually been wrong with this class of rule (see GrafanaShowtimeVolumeAlertSpec). " +
          "Copy the expression across. "
      ) {
        suite should include(expr)
      }
    }
  }
}
