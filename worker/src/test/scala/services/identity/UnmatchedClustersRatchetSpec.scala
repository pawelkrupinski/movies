package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.UnmatchedClustersRatchet
import tools.{IdentityJudgements, UnmatchedClusters}

/**
 * The unmatched-cluster ratchet ([[UnmatchedClustersRatchet]]: zero wrong takes but those known-wrong in scope
 * `capture`, every take judged, every right take of `expected-matches.tsv` kept) over the checked-in captures, one test
 * a check — the gate CI runs. Every take's finding is written to `target/identity-unmatched/ratchet-takes.tsv` before
 * any check reads it, so a red ratchet still leaves it to read; a run that takes more right ones writes the new list to
 * `target/identity-unmatched/expected-matches.tsv` — copy it over the checked-in one to ratchet.
 */
class UnmatchedClustersRatchetSpec extends AnyFlatSpec with Matchers {

  private val run = new UnmatchedClustersRatchet.Run(UnmatchedClusters.Directory, IdentityJudgements.CheckedIn,
    UnmatchedClusters.FindingsPath.getParent, measure = false)

  run.checks.foreach { check =>
    "the unmatched-cluster ratchet" should check.name in {
      if (check eq run.checks.last) run.rightSummary.foreach(info(_))
      check.failure.foreach(fail(_))
    }
  }
}
