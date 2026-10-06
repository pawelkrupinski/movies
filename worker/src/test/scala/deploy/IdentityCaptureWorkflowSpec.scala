package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The weekly unmatched-cluster recapture (`.github/workflows/identity-capture.yml`): every country captured by the
 *  same one command a laptop runs, early enough in the week that its PR is reviewed before Thursday's refit reads the
 *  fixture, and handed back as ONE bot PR — never pushed to main, and prod read only over the read-only CI route. */
class IdentityCaptureWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-capture.yml")
  private lazy val jobs     = RepoFile.jobs(workflow)
  private lazy val capture  = jobs("capture")

  private def weekday(yml: String): Int =
    """cron:\s*'[^']*\s(\d)'""".r.findFirstMatchIn(RepoFile.block(yml, "on")).map(_.group(1).toInt).getOrElse(fail("no weekly cron"))

  "the weekly recapture" should "run early in the week, before the Thursday refit that reads the fixture" in {
    weekday(workflow) should be < weekday(RepoFile.read(".github/workflows/identity-refit.yml"))
    weekday(workflow) should be >= 1
    RepoFile.block(workflow, "on") should include("workflow_dispatch:")
  }

  it should "capture every recorded country, one runner each, with the one command" in {
    scripts.IdentityCapture.Countries.foreach(cc => capture should include(cc))
    capture should include("fail-fast: false")
    capture should include("scripts/identity-capture.sh")
  }

  it should "read prod's family answers only over the read-only CI tunnel, and close it" in {
    capture should include("scripts/ci/wait-for-mongo-tunnel.sh")
    capture should include("scripts/ci/close-mongo-tunnel.sh")
    capture should include("KINOWO_IDENTITY_FAMILY_URI")
    workflow should not include "secrets.MONGODB_URI"
  }

  it should "hand the countries' refreshed fixtures to ONE pull request, never a push to main" in {
    workflow.split("uses: peter-evans/create-pull-request@").length shouldBe 2
    jobs("pull-request") should include("test/resources/fixtures/identity-unmatched/")
    workflow should not include "git push"
  }
}
