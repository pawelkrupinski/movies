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
    // The corpus and tree only: the identity overlay is not the relearn's input.
    learn should include("scripts/identity-fetch-recording.sh --no-overlay \"$RECORDING\"")
  }

  it should "learn with the relearn tool's decorations-only mode, which reads no production data" in {
    learn should include("scripts/identity-calibrate.sh --decorations-only")
    learn should not include "PROD="
    learn should not include "secrets.MONGO"
  }

  it should "hand the learned file back as an artifact, never commit it" in {
    learn should include("identity/src/main/resources/identity-decorations.json")
    workflow should not include "git push"
    workflow should not include "contents: write"
  }

  /** The recording is laid out by the tool every local replay uses, not a hand-rolled copy of it. */
  it should "fetch the recording through the shared tool, failing rather than learn from fewer countries in silence" in {
    val restore = RepoFile.stepScript(workflow, "Restore the recording's corpora and enrichment trees")
    restore should include("set -euo pipefail")
    restore should include("pl uk de us es")
    restore should not include "gh run download"
    restore should not include "gh release download"
    learn should not include "|| true"
    RepoFile.read("scripts/identity-fetch-recording.sh") should include("exit 1")
  }

  it should "hand the dispatched recording to its scripts through the environment, never spliced into them" in {
    Seq("Restore the recording's corpora and enrichment trees", "Learn the decorations").foreach { step =>
      RepoFile.stepScript(workflow, step) should not include "${{"
    }
    RepoFile.stepScript(workflow, "Learn the decorations") should include("VERSION=\"decorations-$RECORDING\"")
  }
}
