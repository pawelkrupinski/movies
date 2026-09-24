package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards `kinowo-movies-served-swing`'s cause-1-vs-cause-2 gate.
 *
 * Before 2026-09-18 query A read only `kinowo_web_movies_served` — a city's count falling >50%
 * and >=4 films below its trailing 1h average paged `severity: warning` regardless of WHY. es/zamora
 * hit this three times in September 2026 (09-03, 09-11, 09-17) and all three were confirmed cause
 * (2): a single-venue city's corpus thinning as its freshest scraped listing ran out of upcoming
 * screenings between scrape passes, not a web tier failing to serve what the corpus still had. The
 * 2026-09-17 episode (7 → 6 → 2, held 8h, back to 13) had `kinowo_worker_movies_served{city="zamora"}`
 * move in EXACT lockstep with the web-side gauge at every 5-minute sample, and
 * `kinowo_worker_readmodel_films_pruned_total{city="zamora"}` was empty throughout — ruling out a
 * prune.
 *
 * The fix ports the discriminant `kinowo-movies-served-city-empty` already carries (since
 * 2026-09-13, see [[GrafanaCityEmptyAlertSpec]]) onto this rule: query A now excludes
 * (`unless on (city) (...)`) any city whose corpus-side gauge swung down by the same >50% margin
 * over the same window, so a city whose corpus thinned in lockstep — cause (2), not a bug — stays
 * silent, and only a city whose corpus did NOT also crater still fires: cause (1), the read-model
 * defect this rule exists to catch. `unless` rather than an `and`-ed threshold, because query A's
 * OUTPUT VALUE must stay the web-side ratio the page reports — the corpus term only decides which
 * cities survive the join, exactly as the city-empty rule's own `and on (city) (... > 0)` decides
 * membership without changing its query A's value.
 *
 * What the expression PRODUCES for both causes is pinned by feeding it series in
 * `infra/test/alert-rules/grafana-movies-served-swing.yml` — promtool cannot load a
 * Grafana-managed rule, but it will evaluate its `expr`. infra/test/test_alert_rule_coverage.py is
 * what stops that suite testing an expression production no longer runs.
 */
class GrafanaServedSwingCorpusDiscriminantSpec extends AnyFlatSpec with Matchers {

  private val SwingUid = "kinowo-movies-served-swing"


  private lazy val swingRule: String =
    AlertRule.withUid(SwingUid).getOrElse(fail(s"no rule with uid `$SwingUid` in ${AlertRule.File}"))

  private lazy val swingQueries: Seq[String] = AlertRule.expressionsIn(swingRule)

  /** Query A of `kinowo-movies-served-swing` — the one `condition: C` thresholds on. */
  private lazy val queryA: Seq[String] =
    swingQueries.filter(_.contains("""kinowo_web_movies_served{scope="all"}"""))

  "the served-swing rule" should "still have exactly one query A" in {
    withClue(
      s"expected exactly one `expr:` containing kinowo_web_movies_served{scope=\"all\"} in " +
        s"${AlertRule.File} (query A of $SwingUid); found ${queryA.size}. If the rule was " +
        "rewritten, update this spec's filter to find its query A again."
    ) {
      queryA should have size 1
    }
  }

  it should "exclude a city whose corpus reading swung down by the same margin" in {
    queryA.foreach { expr =>
      withClue(
        s"'$expr' no longer excludes on kinowo_worker_movies_served. Without " +
          "`unless on (city) (... kinowo_worker_movies_served ... > 0.5)`, this alert pages for " +
          "cause (2) as well as cause (1) — a single-venue city whose own corpus thinned between " +
          "scrapes (es/zamora, three times in September 2026: 09-03, 09-11, 09-17) is not a bug, " +
          "and this alert exists to catch cause (1): a corpus that HELD its count while the web " +
          "stopped serving it. "
      ) {
        expr should include("unless on (city)")
        expr should include("kinowo_worker_movies_served")
      }
    }
  }

  it should "compare the corpus reading against the SAME 50% margin as the web reading" in {
    queryA.foreach { expr =>
      withClue(
        s"'$expr' no longer gates the corpus exclusion at > 0.5 — a mismatched margin would " +
          "either miss cause (2) cities the web-side 50% threshold already lets through, or " +
          "wrongly exclude a cause (1) city whose corpus dipped by less than the web did. "
      ) {
        expr should include("> 0.5)")
      }
    }
  }
}
