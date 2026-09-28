package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The decorations relearn on GitHub's runners (`.github/workflows/identity-decorations.yml`):
 *  learned from one named recording, by the same tool a local relearn runs, and handed back for
 *  review — never pushed, and never reading production. */
class IdentityDecorationsWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-decorations.yml")
  private lazy val learn    = RepoFile.jobs(workflow)("learn")

  "the decorations relearn" should "run only when dispatched, on a named recording" in {
    RepoFile.block(workflow, "on") should include("workflow_dispatch:")
    RepoFile.block(workflow, "on") should not include "schedule:"
    workflow should include("required: true")
    learn should include("enrichment-$cc-$RECORDING.tar.gz")
  }

  it should "learn with the relearn tool's decorations-only mode, which reads no production data" in {
    learn should include("scripts/identity-calibrate.sh --decorations-only")
    learn should not include "PROD="
    learn should not include "secrets.MONGO"
  }

  it should "hand the learned file back as an artifact, never commit it" in {
    learn should include("common/src/main/resources/identity-decorations.json")
    workflow should not include "git push"
    workflow should not include "contents: write"
  }
}
