package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The identity CI staleness check (`.github/workflows/identity-ci-staleness.yml`): every three hours,
 *  whether the identity CI is still telling anyone anything — and an owner-assigned issue when not. */
class IdentityCiStalenessWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-ci-staleness.yml")
  private lazy val check    = RepoFile.jobs(workflow)("check")
  private lazy val checker  = RepoFile.read("scripts/ci/identity-ci-staleness.sh")

  "the staleness check" should "run on a schedule and by hand" in {
    val on = RepoFile.block(workflow, "on")
    on should include("cron: '23 */3 * * *'")
    on should include("workflow_dispatch:")
  }

  it should "self-test the checker before trusting its verdict" in {
    val steps = check.linesIterator.map(_.trim).filter(_.startsWith("- ")).toSeq
    RepoFile.positionOf(steps, "Self-test the checker") should be < RepoFile.positionOf(steps, "Is the identity CI stale?")
    RepoFile.step(workflow, "Self-test the checker") should include("run: bash scripts/ci/identity-ci-staleness-test.sh")
    RepoFile.step(workflow, "Is the identity CI stale?") should include("run: scripts/ci/identity-ci-staleness.sh")
  }

  // The coverage verdict asks whether a run's commit descends from the newest pipeline commit,
  // which a shallow clone cannot answer. Depth 0 on github.com; on Forgejo a depth past the root,
  // since its copy of the repo is itself shallow and a depth-0 fetch of it fails.
  it should "check out the whole history" in {
    check should include("fetch-depth: ${{ github.server_url == 'https://github.com' && '0' || '2147483647' }}")
  }

  it should "ask about workflows that exist" in {
    Seq("CONVERGENCE_WORKFLOW:-", "RECORDING_WORKFLOW:-").foreach { variable =>
      val file = (java.util.regex.Pattern.quote(variable) + """([\w.-]+\.yml)""").r.findFirstMatchIn(checker)
        .getOrElse(fail(s"the checker names no default for $variable")).group(1)
      withClue(s"$variable defaults to $file: ")(RepoFile.exists(s".github/workflows/$file") shouldBe true)
    }
  }

  it should "turn a red scheduled check into the owner's issue" in {
    val report = RepoFile.jobs(workflow)("report")
    report should include("needs: [check, timings]")
    report should include("github.event_name == 'schedule'")
    report should include("uses: ./.github/actions/report-scheduled-run")
    report should include("failed: ${{ contains(needs.*.result, 'failure') }}")
  }

  it should "only read" in {
    check should include("actions: read")
    check should not include "write"
  }
}
