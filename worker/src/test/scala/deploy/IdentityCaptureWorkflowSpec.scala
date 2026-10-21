package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The weekly unmatched-cluster recapture (`.github/workflows/identity-capture.yml`): every country captured by the
 *  same one command a laptop runs, early enough in the week that it is judged before Thursday's refit reads the
 *  fixture, and handed back as ONE bot branch — never pushed to main, and prod read only over the read-only CI route.
 *  The branch becomes a PR only once the ratchet over it is green: a PR is a request for a build, and a recapture's
 *  Branch CI is red on the ratchet until its takes are judged (ten such PRs in two days, #223–#232, each closed by
 *  the next run). */
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

  private lazy val publish = jobs("publish")
  private val Branch       = "chore/identity-capture"

  it should "hand the countries' refreshed fixtures to ONE bot branch, fresh off main, never a push to main" in {
    publish should include("test/resources/fixtures/identity-unmatched/")
    publish should include("uses: ./.github/actions/drop-bot-branch")
    publish.indexOf("uses: ./.github/actions/drop-bot-branch") should be < publish.indexOf("git push")
    publish should include(s"BRANCH: $Branch")
    val pushes = RepoFile.runSteps(workflow).flatMap(_.script.linesIterator).filter(_.contains("git push"))
    pushes should not be empty
    all(pushes) should include("\"HEAD:refs/heads/$BRANCH\"")
    workflow should not include "--force"
  }

  it should "open the PR only when the ratchet over the recapture is green — never on every run" in {
    workflow should not include "peter-evans/create-pull-request"
    val open = RepoFile.step(workflow, "Open the PR when the ratchet is green")
    open should include("steps.ratchet.outputs.green == 'true'")
    open should include("gh pr create")
    val ratchet = RepoFile.stepScript(workflow, "Run the ratchet over the recapture")
    ratchet should include("services.identity.UnmatchedClustersRatchetSpec")
    ratchet should include("green=true")
    ratchet should not include "|| true"
    // A build that never reached the ratchet is no "judge the UNJUDGED takes": no findings written, the job fails.
    ratchet should include(s"rm -f ${tools.UnmatchedClusters.FindingsPath}")
    ratchet should include(s"elif [ -f ${tools.UnmatchedClusters.FindingsPath} ]; then")
    ratchet should include("::error::the ratchet did not run")
  }

  it should "re-check the judged branch on demand and open its PR then, without capturing again" in {
    RepoFile.block(workflow, "on") should include("open-pr:")
    capture should include("if: ${{ !inputs.open-pr }}")
    publish should include(s"inputs.open-pr && '$Branch' || 'main'")
    RepoFile.step(workflow, "Put the countries' fixtures back") should include("if: ${{ !inputs.open-pr }}")
    RepoFile.step(workflow, "Drop the old branch") should include("!inputs.open-pr")
  }

  it should "upload the ratchet's findings as the artifact scripts/identity-ratchet-report reads" in {
    publish should include("name: identity-ratchet")
    publish should include(tools.UnmatchedClusters.FindingsPath.toString)
    RepoFile.read("scripts/identity-ratchet-report") should include("\"identity-ratchet\"")
  }

  /** The report's findings are the ratchet's TSV through the one tool that tables it, not a second grep of the log. */
  it should "write its report from the ratchet's findings TSV, through scripts/identity-ratchet-report" in {
    val report = RepoFile.stepScript(workflow, "Write the report")
    report should include(s"scripts/ci/identity-capture-report.sh captures ${tools.UnmatchedClusters.FindingsPath} ratchet.log")
    report should not include "grep"
    RepoFile.read("scripts/ci/identity-capture-report.sh") should include("identity-ratchet-report\" \"$takes\"")
  }
}
