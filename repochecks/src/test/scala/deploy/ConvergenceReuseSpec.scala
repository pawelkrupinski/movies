package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Main reuses a convergence verdict only when it provably applies (scripts/ci/convergence_reuse.py).
 *
 * A landing check runs the whole `Identity model convergence` suite over the commit it calls LANDABLE; Main's kick then
 * dispatched the same 17 rows again over the same tree. Main's kick now names that run (`reuse-run`), and the run's
 * preflight skips every leg only when the source covered every row main runs -- both order-independence phases among
 * them -- over the same tree key, pairs, inputs and takes bases, and every row passed. These hold the wiring that keeps
 * the daily full run and the legs' inputs honest; the real judgement over the cases that must NOT reuse runs in
 * [[ConvergenceReuseIntegrationSpec]].
 */
class ConvergenceReuseSpec extends AnyFlatSpec with Matchers {

  private def suite = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private def leg   = RepoFile.read(".github/workflows/country-convergence-leg.yml")

  // The reuse facts are the preflight's outputs (the run's plan, WorkflowPlanSpec), so they are what the legs ran.
  "main's legs" should "read every input through the preflight's outputs" in {
    val call = RepoFile.withoutComments(RepoFile.block(RepoFile.jobs(suite)("leg"), "with"))
    call should not include "inputs."
    call should not include "github."
    call should include("reused-from:                   ${{ needs.preflight.outputs.reused-from }}")
    RepoFile.block(RepoFile.jobs(suite)("preflight"), "outputs") should include(
      "reused-from: ${{ steps.reuse.outputs.reused-from || '' }}")
  }

  it should "skip every row only on a reuse the preflight proved" in {
    RepoFile.block(leg, "convergence").linesIterator.find(_.trim.startsWith("if:")).getOrElse("") should
      include("if: ${{ inputs.reused-from == '' && ")
    RepoFile.step(suite, "Does the named run's verdict hold for this commit?") should
      include("if: ${{ inputs.reuse-run != '' }}")
  }

  // The daily full run (convergence-daily.yml) exists so that one run a day takes every take in full: it never reuses.
  // Only Main's kick names a run to reuse.
  "the daily full run" should "never name a run to reuse" in {
    RepoFile.read(".github/workflows/convergence-daily.yml") should not include "reuse-run"
    RepoFile.read(".github/scripts/kick-convergence.sh") should include("-f reuse-run=")
  }
}
