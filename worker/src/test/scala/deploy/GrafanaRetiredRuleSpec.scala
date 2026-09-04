package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * REMOVING AN ALERT RULE FROM THE PROVISIONING FILE DOES NOT REMOVE IT FROM
 * GRAFANA, and this spec is what makes the second half of the deletion happen.
 *
 * Grafana's file provisioning applies `groups` as create-or-update. A rule that
 * disappears from the file is left in the database exactly as it was: still
 * scheduled, still evaluated, still able to notify, from a definition that no
 * longer exists anywhere a reader would look for it. The file says the rule is
 * gone; Grafana disagrees, and only Grafana is right.
 *
 * MEASURED, NOT THEORISED. On 2026-09-04 the ten Fly rules were taken out of
 * `groups` and the live instance was found still holding FOURTEEN of them — the
 * ten, plus four the 2026-08-29 audit had removed the same way, which nobody had
 * noticed in the six days since. Every one queried the deleted `fly-prometheus`
 * datasource, so each logged `Failed to build rule evaluator: data source not
 * found` once per evaluation interval. `execErrState: OK` stopped that paging,
 * which is also what stopped anyone seeing it.
 *
 * So the contract is: take the rule out of `groups`, leave a `RETIRED RULE <uid>`
 * tombstone where it stood, and add the uid to `deleteRules`. The tombstone is
 * for the reader and the `deleteRules` entry is for Grafana; this spec is what
 * keeps the two in step, because the tombstone is the half people remember.
 *
 * Tests run with the repo root as CWD, so the path resolves directly.
 */
class GrafanaRetiredRuleSpec extends AnyFlatSpec with Matchers {

  private val AlertRules = "infra/nix/files/monitoring/grafana/alerting/alert-rules.yaml"
  private lazy val lines = RepoFile.read(AlertRules).linesIterator.toVector

  /** Every `# RETIRED RULE <uid>` tombstone, in file order. */
  private lazy val tombstoned: Seq[String] =
    lines.map(_.trim).collect { case s"# RETIRED RULE $rest" => rest.takeWhile(c => !c.isWhitespace) }

  /** Every uid under `deleteRules:`, which is the list Grafana actually acts on. */
  private lazy val deleted: Seq[String] =
    RepoFile.block(lines.mkString("\n"), "deleteRules")
      .linesIterator.map(_.trim).collect { case s"uid: $uid" => uid }.toSeq

  /** Every uid still DEFINED in the file — the rules that must keep existing. */
  private lazy val live: Seq[String] =
    lines.map(_.trim).collect { case s"- uid: $uid" => uid }

  "every retired rule" should "be listed in deleteRules, or Grafana goes on running it" in {
    tombstoned should not be empty
    val orphaned = tombstoned.filterNot(deleted.contains)
    withClue(
      "these rules are tombstoned in the file but absent from `deleteRules`, so the live Grafana " +
        "still schedules and evaluates them from its database — the exact shape that left 14 " +
        s"zombie rules querying a deleted datasource for weeks: ${orphaned.mkString(", ")}. ") {
      orphaned shouldBe empty
    }
  }

  /**
   * And the reverse, which is the quieter mistake: a uid in `deleteRules` with no
   * tombstone leaves the next reader unable to find out what the rule was or why
   * it went — a deletion with its reasoning thrown away.
   */
  it should "carry a tombstone saying what it was and why it went" in {
    val undocumented = deleted.filterNot(tombstoned.contains)
    withClue(s"`deleteRules` names uids nothing in this file explains: ${undocumented.mkString(", ")}. ") {
      undocumented shouldBe empty
    }
  }

  /**
   * `deleteRules` RUNS ALONGSIDE `groups`, so a uid in both is a rule that is
   * created and destroyed by the same file. Grafana does not promise an order
   * between them, which makes the outcome a coin toss that changes on restart —
   * far worse than either intent on its own.
   */
  "a live rule" should "never also be marked for deletion" in {
    val contradictory = live.filter(deleted.contains)
    withClue(s"defined in `groups` AND listed in `deleteRules`: ${contradictory.mkString(", ")}. ") {
      contradictory shouldBe empty
    }
  }

  /**
   * The entries are cumulative on purpose. Pruning one once it has been applied
   * everywhere looks tidy and is how a rule resurrects: any instance provisioned
   * from an older database still holds it, and Grafana ignores a uid it does not
   * have, so an entry costs one line and removing one costs a zombie nobody is
   * looking for. The four from the 2026-08-29 audit are the proof — they were
   * still running six days later.
   */
  it should "keep the audit-era retirements listed, not just the newest batch" in {
    deleted should contain allOf (
      "kinowo-disk-space-low", "kinowo-disk-fill-projected",
      "kinowo-mongo-down", "kinowo-worker-credit-low")
  }

  /**
   * The Grafana FOLDER is part of the provisioning contract too: rules land in the
   * folder their group names, and a folder called "Fly Alerts" on a fleet with no
   * Fly in it sends whoever is mid-incident looking for a platform that is gone.
   */
  "the alert folder" should "not still be named for the platform this fleet left" in {
    val folders = lines.map(_.trim).collect { case s"folder: $name" => name }.distinct
    folders should not be empty
    folders.foreach(f => withClue(s"folder '$f': ")(f.toLowerCase should not include "fly"))
  }
}
