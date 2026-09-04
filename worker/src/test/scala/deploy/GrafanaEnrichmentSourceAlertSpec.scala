package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards alert coverage over ENRICHMENT SOURCE HEALTH — a whole upstream
 * refusing us.
 *
 * 2026-07-30: IMDb's GraphQL CDN began 403-ing every request that didn't name a
 * client. Ratings across PL, DE and UK stopped refreshing for ~47 hours and
 * nothing paged. Every piece of detection already existed:
 *
 *   - `HttpOutcome.Http403` is its own metric label, split out from the 404
 *     noise floor precisely so "a host refusing us" is legible, and its own doc
 *     comment calls a rising 403 "an outage in the making".
 *   - `MonitoringHttpFetch` maps `caching.graphql.imdb.com` to the "IMDb"
 *     service and counts 403 as a failure, so /uptime showed IMDb at 100%
 *     failure for two days.
 *
 * What was missing was anyone listening: not one rule referenced enrichment
 * health, so the signal sat on a page nobody had open. This spec exists so the
 * next dead source pages instead.
 *
 * Two properties have to hold, each mapping to a way this outage stayed silent:
 *
 *   - WATCHED AT ALL. A rule must read a source's recent failure RATIO, not a
 *     raw failure count: the resolvers probe candidate slugs by design, so
 *     absolute failure counts are never zero and only the ratio separates "a few
 *     probes missed" from "this upstream is refusing us wholesale".
 *
 *   - PER SOURCE. One rule summed across all five sources cannot fire for a
 *     single dead one — IMDb was at ~100% failure while TMDB, RT, Metacritic and
 *     Filmweb were healthy, which fleet-wide reads as roughly a fifth of traffic
 *     failing. Grouped by service it is unmissable, and the alert names the
 *     source that died.
 *
 * The threshold and window are deliberately NOT pinned (tuning them is an
 * alerting-noise judgement, not a drift bug) — only that the rule exists, reads
 * a ratio, and is grouped per source.
 */
class GrafanaEnrichmentSourceAlertSpec extends AnyFlatSpec with Matchers {

  private val AlertRules = "infra/nix/files/monitoring/grafana/alerting/alert-rules.yaml"

  private lazy val alertRules = RepoFile.read(AlertRules)

  /** Every `expr:` line in the provisioning file, unquoted. */
  private lazy val expressions: Seq[String] =
    """(?m)^\s*expr:\s*'(.*)'\s*$""".r
      .findAllMatchIn(alertRules)
      .map(_.group(1))
      .toSeq

  /** Expressions reading the per-service uptime health gauges. */
  private lazy val enrichmentHealthExpressions: Seq[String] =
    expressions.filter(e => e.contains("kinowo_uptime_recent_failures") && e.contains("IMDb"))

  "the alerting stack" should "watch enrichment source health at all" in {
    withClue(
      "No alert rule reads kinowo_uptime_recent_failures for the enrichment sources. " +
      "IMDb's CDN block ran ~47h with /uptime showing it 100% failed and nothing paging. "
    ) {
      enrichmentHealthExpressions should not be empty
    }
  }

  it should "cover every enrichment source, not just the one that happened to break" in {
    val covered = enrichmentHealthExpressions.mkString(" ")
    Seq("IMDb", "TMDB", "Filmweb", "Metacritic", "Rotten Tomatoes").foreach { source =>
      withClue(s"$source is not covered by any enrichment-health alert: ") {
        covered should include (source)
      }
    }
  }

  it should "read a failure RATIO, so routine probe misses aren't mistaken for an outage" in {
    // The resolvers probe candidate slugs that mostly 404 by design, so a raw
    // failure count is never zero. Only failures/(failures+successes) separates
    // "some probes missed" from "this upstream refuses us".
    enrichmentHealthExpressions.foreach { expression =>
      withClue(s"not a ratio: $expression ") {
        expression should include ("kinowo_uptime_recent_successes")
        expression should include ("/")
      }
    }
  }

  it should "group per source, so one dead source can't hide behind four healthy ones" in {
    // `by (service)` or any grouping that leads with it — `by (service, country)`
    // is finer still, which is fine; what must not happen is an ungrouped sum.
    enrichmentHealthExpressions.foreach { expression =>
      withClue(s"not grouped by service: $expression ") {
        expression should include ("by (service")
      }
    }
  }

  it should "guard against divide-by-zero on an idle window, like the proxy rule does" in {
    enrichmentHealthExpressions.foreach { expression =>
      withClue(s"no clamp_min guard: $expression ") {
        expression should include ("clamp_min")
      }
    }
  }
}
