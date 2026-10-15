package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The identity findings report (`.github/workflows/identity-findings-report.yml`): a recording's
 *  whole-corpus findings, read from the artifacts its convergence rows left and triaged through one
 *  issue per country — never a gate on pushes, and never a re-run of the suites. */
class IdentityFindingsReportWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-findings-report.yml")
  private lazy val findings = RepoFile.jobs(workflow)("findings")
  private lazy val report   = RepoFile.stepScript(workflow, "Report each country's findings")

  "the findings report" should "run when a recording completes, daily as a backstop, and by hand" in {
    val on = RepoFile.block(workflow, "on")
    on should include("workflows: [Record scrape fixtures]")
    on should include("types: [completed]")
    on should include("schedule:")
    on should include("workflow_dispatch:")
    withClue("the recording it follows must be the workflow of that name: ")(
      RepoFile.workflowName(RepoFile.read(".github/workflows/record-scrape-fixtures.yml")) shouldBe "Record scrape fixtures")
  }

  it should "report only main's recordings — a branch recording pins nothing anyone replays" in {
    findings should include("github.event.workflow_run.head_branch == 'main'")
    withClue("the newest recording, when none is named, is resolved by the script — main's only (its test holds the query): ")(
      report should include("python3 scripts/ci/identity_findings_report.py --run \"${INPUT_RUN:-$EVENT_RUN}\""))
  }

  it should "read the findings the recording left, through the report script, and re-run no suite" in {
    report should include("python3 scripts/ci/identity_findings_report.py --run")
    RepoFile.withoutComments(findings) should not include "sbt"
    withClue("the report reads the judgements to tell triaged from untriaged: ")(
      findings should include("test/resources/fixtures/identity-judgements.tsv"))
    ScriptTests.runInCi("scripts/ci/identity_findings_report_test.py") shouldBe true
  }

  it should "fail, not report nothing, when there is no recording to read" in {
    val script = RepoFile.read("scripts/ci/identity_findings_report.py")
    script should include("::error::no completed Record scrape fixtures run to report")
    withClue("held by identity_findings_report_test.py's no-run-named case, which the runner runs: ")(
      ScriptTests.runInCi("scripts/ci/identity_findings_report_test.py") shouldBe true)
  }

  it should "take the dispatched run id through the environment, never spliced into the script" in {
    report should not include "${{"
  }

  it should "write issues and nothing else" in {
    findings should include("issues: write")
    workflow should not include "contents: write"
    workflow should not include "git push"
  }

  // Gating pushes on a recording's data would rebuild the coupling this report exists to undo:
  // no push makes or mends a recording.
  it should "not be one of the red-build gate's workflows" in {
    val gating = """GatingWorkflows=\(([^)]*)\)""".r.findFirstMatchIn(RepoFile.read("scripts/hooks/pre-push"))
      .getOrElse(fail("scripts/hooks/pre-push names no GatingWorkflows")).group(1)
    gating should include("\"Record scrape fixtures\"")
    gating should not include RepoFile.workflowName(workflow)
  }
}
