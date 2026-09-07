package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the alert over a SILENT `movies` change-stream cursor — a cursor that is open and
 * delivering nothing while the collection keeps changing.
 *
 * The event-rate rules cannot see this failure: a stalled cursor and a quiet night are both a
 * flat line at zero. The worker exports the AGE of each cursor's last delivered event
 * (`kinowo_worker_change_stream_last_event_age_seconds`), and `ChangeStreamMoviesCursorSilent`
 * compares the `movies` cursor's age against 2x each country's deployed sweep cadence, gated on
 * the corpus census having moved in the last hour. A dead stream after a Mongo migration sat
 * behind green panels for hours before either existed.
 *
 * What is asserted is the same as `CinemaScrapeStalenessAlertSpec` asserts for its sibling: the
 * per-country literals are DERIVED (2x `KINOWO_SCRAPE_FRESHNESS_MINUTES`, in seconds) and a
 * derived literal that nothing checks drifts the moment its source moves. The multiplier is a
 * judgement about noise, asserted only to be uniform across countries.
 */
class ChangeStreamSilenceAlertSpec extends AnyFlatSpec with Matchers {

  private val Rules      = "infra/nix/files/monitoring/rules/read-model-projection.rules"
  private val OverlayDir = "infra/kubernetes/worker/overlays"
  private val Metric     = "kinowo_worker_change_stream_last_event_age_seconds"

  /** Two full sweeps. Within ONE sweep every venue is scraped and a film whose metadata or
   *  slots changed is written to `movies`, so a healthy cursor delivers at least once per
   *  sweep whenever anything changes at all; the census gate handles the case where nothing
   *  did. Two sweeps is one sweep of slack past that — the same shape as the 1.5x on the
   *  oldest-scrape rule, wider because a cursor's silence has a legitimate cause the
   *  scrape's staleness does not (a country where nothing changed). */
  private val Multiplier = 2.0

  private lazy val rules = RepoFile.read(Rules)

  private lazy val deployedCountries: Seq[String] =
    Option(new java.io.File(OverlayDir).listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(_.isDirectory)
      .map(_.getName)
      .sorted
      .toSeq

  private lazy val thresholds: Map[String, Long] = RepoFile.perCountryThresholds(rules, Metric)

  "the fleet" should "deploy at least one worker to watch" in {
    deployedCountries should not be empty
  }

  "the ChangeStreamMoviesCursorSilent rule" should "watch the movies cursor for every deployed country" in {
    deployedCountries.foreach { cc =>
      withClue(
        s"no clause for country=\"$cc\" reading $Metric in $Rules. That country's movies change " +
          "stream is UNWATCHED: a cursor that stops delivering emits nothing the event-rate rules " +
          "can see. Add a clause to ChangeStreamMoviesCursorSilent at 2x its overlay cadence. "
      ) {
        thresholds.keySet should contain(cc)
      }
    }
  }

  it should "compare only the movies cursor, never a side one" in {
    // The `screenings` and `movie_slots` cursors legitimately go quiet for a whole sweep, and
    // the census the rule is gated on counts the `movies` collection. A clause without the
    // collection label would fire on a quiet slots cursor.
    val clauses = (java.util.regex.Pattern.quote(Metric) + """\{([^}]*)\}\s*>""").r
      .findAllMatchIn(rules).map(_.group(1)).toSeq
    clauses should not be empty
    clauses.foreach(labels => withClue(s"clause `$labels` does not pin collection=\"movies\": ") {
      labels should include("""collection="movies"""")
    })
  }

  it should "pin each country's threshold to twice its own deployed sweep window" in {
    deployedCountries.foreach { cc =>
      val window = RepoFile
        .deployedFreshnessMinutes(cc)
        .getOrElse(fail(s"$OverlayDir/$cc/patch.yaml has no KINOWO_SCRAPE_FRESHNESS_MINUTES"))
      val expected = (window * 60 * Multiplier).toLong
      withClue(
        s"$cc deploys a ${window}min sweep window, so its clause in $Rules must read " +
          s"`> $expected` (${Multiplier}x, in seconds) and reads `> ${thresholds.getOrElse(cc, 0L)}`. " +
          "A threshold that no longer matches the cadence it was derived from is worse than none: " +
          "too low it pages on a slow but healthy sweep, too high it stays quiet through the dead " +
          "stream it exists for. "
      ) {
        thresholds.get(cc) shouldBe Some(expected)
      }
    }
  }

  it should "give every country the same multiplier, rather than tuning one quietly" in {
    val ratios = deployedCountries.flatMap { cc =>
      for {
        window    <- RepoFile.deployedFreshnessMinutes(cc)
        threshold <- thresholds.get(cc)
      } yield threshold.toDouble / (window * 60)
    }
    withClue(s"the per-country thresholds in $Rules imply different multipliers: ${ratios.distinct}. ") {
      ratios.distinct should have size 1
    }
  }

  it should "demand that the corpus census moved, so a quiet collection cannot page" in {
    // The age alone is true of a country where nothing changed all night. The rule must be
    // gated on independent evidence that `movies` was written: the off-band corpus census.
    val body = """(?s)alert:\s*ChangeStreamMoviesCursorSilent.*?\n\s*for:""".r
      .findFirstIn(rules)
      .getOrElse(fail(s"$Rules has no ChangeStreamMoviesCursorSilent rule"))
    body should include("changes(kinowo_worker_corpus_movies[")
    body should include("and on (country)")
  }
}
