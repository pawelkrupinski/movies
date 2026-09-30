package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `identity-model-convergence.yml`: the pipeline suite's legs asked of the identity model — a
 * trigger-only measurement of a model no country serves yet, so it must never be mistaken for, or
 * get in the way of, the suite Main dispatches.
 */
class IdentityModelConvergenceWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val suite    = RepoFile.read(".github/workflows/country-convergence.yml")
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val main     = RepoFile.read(".github/workflows/main.yml")
  private lazy val overlay  = RepoFile.read(".github/actions/convergence-overlay-publish/action.yml")

  "the identity model convergence build" should "run only when dispatched by hand" in {
    val triggers = RepoFile.block(workflow, "on")
    triggers should include("workflow_dispatch:")
    Seq("schedule:", "push:", "pull_request:", "workflow_run:", "workflow_call:").foreach(triggers should not include _)
    main should not include "Identity model convergence"
  }

  it should "measure every country the pipeline suite does, on the same budgets and heap" in {
    RepoFile.matrixRows(workflow) shouldBe RepoFile.matrixRows(suite)
  }

  it should "decide its legs' films by the identity model" in {
    RepoFile.jobs(workflow)("leg") should include regex """identity-model:\s+true"""
  }

  /** The pipeline's pair was recorded by the PIPELINE; the model's enrichment gaps are filled live and
   *  published as an overlay beside it — so the next dispatch replays more and fetches less. */
  it should "fill and publish the model's gaps as an overlay, never into the pipeline's pair" in {
    RepoFile.jobs(workflow)("leg") should include regex """mode:\s+overlay"""
    overlay should include("identity-overlay-")
    val commands = overlay.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    Seq("enrichment-${{ inputs.code }}.tar.gz", "hermetic-", "gh release delete", "delete-asset").foreach(commands should not include _)
    leg should include regex """uses: ./.github/actions/convergence-overlay-publish\s+if: always\(\) && inputs.mode == 'overlay'"""
  }

  it should "never mark the pipeline's corpus green from a new-model leg" in {
    val conditions = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("uses: ./.github/actions/convergence-publish") => cond }.toSeq
    conditions should not be empty
    conditions.foreach(cond => cond should (include("inputs.mode == 'record'") or include("inputs.mode != 'overlay'")))
  }

  it should "hold a lane of its own, which a newer dispatch replaces" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include("group: identity-model-convergence")
    concurrency should include("cancel-in-progress: true")
    RepoFile.block(suite, "concurrency") should not include "identity-model-convergence"
  }

  it should "request no bisect and file no issue: a red leg is a finding about the model" in {
    RepoFile.jobs(workflow).keySet shouldBe Set("preflight", "leg")
    val requests = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("convergence-bisect-request") => cond }.toSeq
    requests should not be empty
    requests.foreach(_ should include("!inputs.identity-model"))
  }

  "the convergence leg" should "cut over its own country only when asked, and on every sbt step" in {
    val cutovers = leg.linesIterator.filter(_.contains("KINOWO_IDENTITY_CUTOVER:")).map(_.trim).toSeq
    cutovers should not be empty
    cutovers.distinct shouldBe Seq("KINOWO_IDENTITY_CUTOVER: ${{ inputs.identity-model && inputs.code || '' }}")
    cutovers.size shouldBe leg.linesIterator.count(_.contains("KINOWO_IDENTITY_LOOKUPS:"))
    RepoFile.block(leg, "identity-model") should include("default: false")
  }
}
