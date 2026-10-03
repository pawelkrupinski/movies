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
    learn should include("enrichment-$cc-$RECORDING.tar.*")
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

  it should "fail on a corpus that will not unpack, rather than learn from fewer countries in silence" in {
    learn should include(".github/scripts/unpack-fixture-archive.sh \"scrape-$cc/scrapes-$cc.tar.gz\"")
    learn should not include "|| true"
  }

  it should "hand the dispatched recording to its scripts through the environment, never spliced into them" in {
    Seq("Restore the recording's corpora and enrichment trees", "Learn the decorations").foreach { step =>
      RepoFile.stepScript(workflow, step) should not include "${{"
    }
    RepoFile.stepScript(workflow, "Learn the decorations") should include("VERSION=\"decorations-$RECORDING\"")
  }
}
