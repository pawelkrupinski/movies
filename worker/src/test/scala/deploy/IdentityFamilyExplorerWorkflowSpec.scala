package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The per-family convergence check runs NIGHTLY, OUTSIDE THE GATE (the owner, 2026-11-06, option A;
 *  `.github/workflows/identity-family-explorer.yml`): a per-commit gate cost +36% runner minutes, and 37 of 40 identity
 *  commits re-walk every family. It explores and publishes; it judges no commit and files no backlog row. */
class IdentityFamilyExplorerWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-family-explorer.yml")
  private lazy val jobs     = RepoFile.jobs(RepoFile.withoutComments(workflow))
  private lazy val gating   = RepoFile.read(".github/gating-checks.tsv").linesIterator.filterNot(_.startsWith("#")).toSeq

  "the family explorer" should "be no workflow of THE GATE, so no push waits on it or is held red by it" in {
    gating.filter(_.startsWith("workflow\t")).map(_.split('\t')(1)) should not contain "identity-family-explorer.yml"
    gating.filter(_.startsWith("job\t")).map(_.split('\t')(1)) should not contain "identity-family-explorer.yml"
    // the gate's own workflows neither call it nor carry its rows
    Seq("main.yml", "identity-model-convergence.yml", "country-convergence-leg.yml").foreach { gate =>
      RepoFile.read(s".github/workflows/$gate") should not include "family-check"
    }
  }

  it should "run weekly and on dispatch, never on a push" in {
    val on = RepoFile.block(workflow, "on")
    on should include("schedule:")
    on should include("* * 0'")
    on should include("workflow_dispatch:")
    on should not include "push:"
    on should not include "pull_request"
  }

  it should "split every country over its shard rows, planned in one job" in {
    keysOf("walk")("strategy") should include("fromJson(needs.plan.outputs.matrix)")
    WorkflowText.needs(jobs("walk")) shouldBe Set("plan")
    workflow should include("scripts/ci/family_shards.py plan")
  }

  it should "declare the release tag convergence-setup reads, which a workflow's env does not inherit from the legs" in {
    RepoFile.block(workflow, "env") should include("FIXTURE_RELEASE_TAG: convergence-fixtures")
  }

  it should "walk bounded, and end what a timed-out walk left before anything else runs" in {
    val walk = WorkflowText.steps(jobs("walk"))
    walk.exists(_.get("run").exists(_.contains("scripts/ci/identity-family-nightly.sh"))) shouldBe true
    walk.exists(s => s.get("run").exists(_.contains("scripts/ci/bounded-step.sh --reap")) && s.get("if").exists(_.contains("always()"))) shouldBe true
  }

  it should "publish what the walks found as release assets, and push nothing to a branch" in {
    val publish = RepoFile.stepScript(workflow, "Join the shards and publish the flips")
    publish should include("scripts/ci/family-explorer-publish.sh")
    workflow should not include "git push"
    RepoFile.read("scripts/ci/family-explorer-publish.sh") should include("rel=identity-family-explorer")
  }

  private def keysOf(job: String) = WorkflowText.keys(jobs(job))
}
