package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `identity-model-convergence.yml`: the pipeline suite's legs asked of the identity model — a
 * measurement of a model no country serves yet, which Main dispatches beside the pipeline suite on
 * the same gate and the same superseding rules, yet must never be mistaken for, or get in the way of,
 * that suite.
 */
class IdentityModelConvergenceWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val suite    = RepoFile.read(".github/workflows/country-convergence.yml")
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val main     = RepoFile.read(".github/workflows/main.yml")
  private lazy val overlay  = RepoFile.read(".github/actions/convergence-overlay-publish/action.yml")

  "the identity model convergence build" should "be dispatched by Main's convergence kick, on the pipeline suite's gate" in {
    val triggers = RepoFile.block(workflow, "on")
    triggers should include("workflow_dispatch:")
    Seq("schedule:", "push:", "pull_request:", "workflow_run:", "workflow_call:").foreach(triggers should not include _)
    RepoFile.jobs(main)("kick-convergence") should include(
      """kick-convergence.sh "$GITHUB_SHA" "$GITHUB_REF_NAME" "Country convergence" "Identity model convergence"""")
    // An edit to this workflow can change its verdict, so it is one of the paths the gate dispatches for.
    RepoFile.read(".github/convergence-paths.txt").linesIterator.map(_.trim).toSeq should contain(
      ".github/workflows/identity-model-convergence.yml")
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
    Seq("enrichment-${{ inputs.code }}.tar.", "hermetic-", "gh release delete", "delete-asset").foreach(commands should not include _)
    // ONE overlay publisher per leg, under `always()` and after the sample: the sample runs in the
    // full row's job, so a red sample — which ends that job before the suite — still publishes what
    // it fetched, as the separate sample job's own publish did.
    val publishes = leg.linesIterator.sliding(2).collect {
      case Seq(uses, cond) if uses.contains("uses: ./.github/actions/convergence-overlay-publish") => cond.trim }.toSeq
    publishes shouldBe Seq("if: always() && matrix.phase == 'convergence' && inputs.mode == 'overlay'")
    val convergence = RepoFile.block(leg, "convergence")
    convergence.indexOf("- name: Run the ${{ inputs.country }} sample ahead of the suite") should be <
      convergence.indexOf("uses: ./.github/actions/convergence-overlay-publish")
  }

  it should "never mark the pipeline's corpus green from a new-model leg" in {
    val conditions = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("uses: ./.github/actions/convergence-publish") => cond }.toSeq
    conditions should not be empty
    conditions.foreach(cond => cond should (include("inputs.mode == 'record'") or include("inputs.mode != 'overlay'")))
  }

  /** Its own lane — neither suite queues behind, or evicts the pending run of, the other — under the
   *  pipeline suite's superseding rules: finish the run in flight, keep one newer run pending, and let
   *  each newer dispatch replace that pending one. */
  it should "hold a lane of its own, superseded the way the pipeline suite's is" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include("group: identity-model-convergence")
    concurrency should include("cancel-in-progress: false")
    RepoFile.block(suite, "concurrency") should include("cancel-in-progress: false")
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
