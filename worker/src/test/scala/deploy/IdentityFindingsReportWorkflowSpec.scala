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

  // Run 3139 went red with "run 3136 left no country's findings" after recording 3136 failed before any
  // convergence row wrote them: a red for lack of input, not for a finding.
  it should "report main's newest successful recording when the one it follows failed or ran off main" in {
    val plan = RepoFile.jobs(workflow)("plan")
    plan should include("uses: ./.github/actions/upstream-run")
    withClue("an older recording's findings are still the newest to triage: no age limit: ")(
      plan should include("max-age-hours: '0'"))
    withClue("a branch recording pins nothing anyone replays: the resolver passes it over (UpstreamRunSpec): ")(
      findings should not include "head_branch")
    findings should include("RECORDING: ${{ needs.plan.outputs.run }}")
    report should include("python3 scripts/ci/identity_findings_report.py --run \"$RECORDING\"")
  }

  it should "read the findings the recording left, through the report script, and re-run no suite" in {
    report should include("python3 scripts/ci/identity_findings_report.py --run")
    RepoFile.withoutComments(findings) should not include "sbt"
    withClue("the report reads the judgements to tell triaged from untriaged: ")(
      findings should include("test/resources/fixtures/identity-judgements.tsv"))
  }

  it should "fail, not report nothing, when there is no recording to read" in {
    val script = RepoFile.read("scripts/ci/identity_findings_report.py")
    script should include("::error::no completed Record scrape fixtures run to report")
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
    // THE GATE is exactly Main and Identity model convergence (the owner, 2026-10-27)
    gating.split("\\s+").filter(_.nonEmpty).toSeq shouldBe Seq("\"Main\"", "\"Identity", "model", "convergence\"")
    withClue("nor the capture, whose red is the night's, not a push's: ")(gating should not include "\"Record scrape fixtures\"")
    gating should not include RepoFile.workflowName(workflow)
  }
}
