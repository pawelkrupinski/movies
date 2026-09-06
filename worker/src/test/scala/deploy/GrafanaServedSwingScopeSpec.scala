package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards `kinowo-movies-served-swing` against the ONE series it cannot read:
 * `kinowo_web_movies_served{scope="tomorrow"}`.
 *
 * The rule compares each city's current count against `avg_over_time(...[1h])`.
 * That baseline is only meaningful while the series is measuring the same thing
 * for the whole hour, and the `tomorrow` scope is not — it counts films with a
 * showtime on the NEXT LOCAL CALENDAR DAY, so at local midnight every city's
 * series steps to a different day's programme. For the hour that follows, the
 * baseline is an average of two unrelated days and the rule is comparing
 * Sunday's cinema listings against Monday's.
 *
 * 2026-09-06, 00:31 CEST: a batch of DE cities paged within minutes of local
 * midnight, when `tomorrow` rolled from Sunday to Monday and the small-town
 * German Kinos that are dark on Mondays emptied out — bernkastel-kues 18 → 0,
 * meppen 20 → 1, finsterwalde 14 → 3, marktredwitz 11 → 1. Nothing was wrong.
 * The same four cities had printed the SAME four values (0, 1, 3, 1) at the
 * same rollover exactly one week earlier, on Monday 2026-08-31, and their
 * `scope="all"` series did not move by a single film across either midnight —
 * 23 / 43 / 27 / 12, flat from 18:00 through 04:00. It is a weekly programme
 * faithfully scraped, not an outage.
 *
 * This is not tuning. No threshold separates the rollover from a real drop,
 * because on this metric they are the same event: a step change to a much lower
 * number, held. The only thing that distinguishes them is which calendar day
 * each side of the step is counting, which the rule has no way to ask.
 *
 * So the rule reads `scope="all"` only, which is what its own comment says it
 * exists for — the read-model-outage shape, a malformed `web_movies` row
 * emptying the corpus, craters `all` just as hard — and which has no midnight
 * discontinuity to be fooled by. Over 2026-08-29..09-06 the `tomorrow` arm
 * fired on 12, 19, 36, 11 and 4 evaluation samples on five separate nights,
 * clustered at the local-midnight rollover; the `all` arm's background over the
 * same days was 1-4 series outside the one real 08-30 incident.
 *
 * `kinowo-movies-served-city-empty`, the companion that keeps paging while a
 * city stays dark, already scopes itself to `all` for the same reason.
 */
class GrafanaServedSwingScopeSpec extends AnyFlatSpec with Matchers {

  private val AlertRules = "infra/nix/files/monitoring/grafana/alerting/alert-rules.yaml"

  private val SwingUid = "kinowo-movies-served-swing"

  private val Gauge = "kinowo_web_movies_served"

  /** The `- uid: kinowo-movies-served-swing` list item, up to the next rule. */
  private lazy val swingRule: String =
    RepoFile
      .read(AlertRules)
      .split("(?m)^\\s*- uid:")
      .find(_.trim.startsWith(SwingUid))
      .getOrElse(fail(s"no rule with uid `$SwingUid` in $AlertRules"))

  /** The rule's PromQL, without the prose around it — the `description:`
   *  annotation names the gauge too, and a metric name in a sentence carries no
   *  selector. */
  private lazy val swingQueries: Seq[String] =
    """(?m)^\s*expr:\s*'(.*)'\s*$""".r
      .findAllMatchIn(swingRule)
      .map(_.group(1))
      .toSeq

  /** Every selector the rule's PromQL puts on the gauge, as the label matcher it
   *  carries (`""` when the gauge is named bare, which selects every scope). */
  private lazy val gaugeSelectors: Seq[String] =
    swingQueries.flatMap { query =>
      s"""\\Q$Gauge\\E(\\{[^}]*\\})?""".r
        .findAllMatchIn(query)
        .map(m => Option(m.group(1)).getOrElse(""))
    }

  "the served-swing rule" should "read the gauge at all" in {
    withClue(s"`$SwingUid` in $AlertRules no longer selects `$Gauge`: ") {
      gaugeSelectors should not be empty
    }
  }

  it should "compare only the scope that does not step at local midnight" in {
    gaugeSelectors.foreach { selector =>
      withClue(
        s"`$SwingUid` selects `$Gauge$selector`, which includes scope=\"tomorrow\". That series " +
          "counts the NEXT LOCAL CALENDAR DAY, so it steps to a different day's programme at " +
          "every local midnight and the rule's `avg_over_time(...[1h])` baseline spends the " +
          "following hour averaging two unrelated days. That is what paged a batch of DE cities " +
          "at 00:31 CEST on 2026-09-06 for a Sunday->Monday rollover — bernkastel-kues 18 -> 0 " +
          "with its scope=\"all\" series flat at 23 across the same midnight, and the identical " +
          "four values a week earlier. Select `scope=\"all\"`, as kinowo-movies-served-city-empty " +
          "does. "
      ) {
        selector should include("scope=\"all\"")
      }
    }
  }
}
