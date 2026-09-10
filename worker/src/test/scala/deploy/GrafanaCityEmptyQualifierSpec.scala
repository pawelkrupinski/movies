package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards `kinowo-movies-served-city-empty` against the qualifier that switches
 * it off as the outage it is reporting gets worse.
 *
 * The rule pages when a city serves zero films, and qualifies that on the city
 * having been active recently — without which every roster entry whose venue is
 * shut, seasonal or not yet scraped would page forever. The qualifier used to be
 * `avg_over_time(...[6h] offset 10m) >= 4`, and an AVERAGE over a TRAILING window
 * is made of the very samples the outage is zeroing. Each dark minute drags the
 * mean down; a few hours in, the city's own history no longer clears 4 and the
 * rule falls silent while the city is still dark. It stops alerting exactly when
 * the incident has gone on long enough to matter.
 *
 * Measured over 2026-09-02..09-08, replaying both forms against the real series:
 *
 *   avg_over_time([6h])  1 episode,  70 dark-minutes reported
 *   max_over_time([24h]) 4 episodes, 2610 dark-minutes reported
 *
 * es/zamora went dark for 460 minutes on 09-03 and the old form reported 70 of
 * them. us/butte was dark from 09-07 02:50Z for more than 27 hours — still dark
 * when this was written — and the old form never paged for it at all, because
 * butte fell from 4 films and a mean of 4 dips under the threshold within one
 * window. The other two the new form finds, de/sassnitz and us/havre, were
 * likewise real and likewise unreported.
 *
 * `max` is the load-bearing half: it asks "did this city serve >= 4 films at any
 * point in the window", which a blackout cannot erode. The 24h window is the
 * natural unit for a cinema programme, and it still lapses once a city has been
 * dark a full day — deliberately, because a venue dark that long is a roster
 * question rather than an incident, and a 7d window (6 episodes over the same
 * days) keeps paging for a week about cinemas that have simply closed.
 */
class GrafanaCityEmptyQualifierSpec extends AnyFlatSpec with Matchers {

  private val CityEmptyUid = "kinowo-movies-served-city-empty"

  private val Gauge = "kinowo_web_movies_served"
  private val CorpusGauge = "kinowo_worker_movies_served"

  private lazy val rule: String =
    AlertRule.withUid(CityEmptyUid).getOrElse(fail(s"no rule with uid `$CityEmptyUid` in ${AlertRule.File}"))

  // The DETECTION query (refId A) — everything below guards this one
  // specifically, not every query in the rule, because the rule also carries
  // a purely informational companion (refId B, see the test at the bottom)
  // that is deliberately shaped differently: a `expressionsIn`-wide check
  // would wrongly demand the companion read the same gauge and qualifier.
  private lazy val query: String =
    AlertRule.expressionFor(rule, "A").getOrElse(fail(s"no refId A query in `$CityEmptyUid` in ${AlertRule.File}"))

  "the city-empty rule" should "read the gauge at all" in {
    withClue(s"`$CityEmptyUid` in ${AlertRule.File} no longer selects `$Gauge`: ") {
      query should include(Gauge)
    }
  }

  it should "qualify on a peak the blackout cannot erode, not on a decaying average" in {
    withClue(
      s"`$CityEmptyUid` qualifies its zero-check with `avg_over_time`. That average is computed " +
        "over a trailing window made of the samples the outage is zeroing, so it sinks under the " +
        ">= 4 threshold while the city is still dark and the rule goes quiet mid-incident. " +
        "es/zamora was dark 460 minutes on 2026-09-03 and the average form reported 70 of them; " +
        "us/butte was dark over 27 hours from 09-07 and it never paged at all. Use " +
        "`max_over_time`, which asks whether the city was ever active in the window. "
    ) {
      query should not include "avg_over_time"
    }
  }

  it should "ask whether the city was active over a whole day" in {
    withClue(
      s"`$CityEmptyUid` no longer qualifies on `max_over_time($Gauge{scope=\"all\"}[24h] offset 10m)`. " +
        "A shorter window shortens how long a genuine blackout is reported: over 2026-09-02..09-08 " +
        "a [6h] max covered 980 dark-minutes against [24h]'s 2610, cutting zamora's 460-minute " +
        "blackout off at 320 and butte's at 290. A day is the unit a cinema programme comes in. "
    ) {
      query should include(s"""max_over_time($Gauge{scope="all"}[24h] offset 10m)""")
    }
  }

  // Added 2026-09-10 alongside the Decodo-outage investigation: the rule used
  // to make a responder manually check the venue's own site before deciding
  // whether a dark city is a read-model bug or a genuinely empty listing.
  // refId B answers that from the notification itself.
  it should "carry the corpus-side count for the same city as an informational companion query" in {
    withClue(
      s"`$CityEmptyUid` no longer carries a refId B query reading `$CorpusGauge` by city. That " +
        "companion is what lets the notification say whether a dark city is a genuine content gap " +
        "(both sides zero) or a read-model defect (corpus nonzero while the site reads zero) " +
        "without a human running a follow-up query first. "
    ) {
      val companion =
        AlertRule.expressionFor(rule, "B").getOrElse(fail(s"no refId B query in `$CityEmptyUid` in ${AlertRule.File}"))
      companion should include(CorpusGauge)
    }
  }
}
