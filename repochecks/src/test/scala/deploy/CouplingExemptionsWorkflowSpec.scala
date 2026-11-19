package deploy

import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.sys.process.Process

/** The weekly exemption report (`.github/workflows/coupling-exemptions.yml`; the owner, 2026-11-19: agent exemptions
 *  "reported weekly"). The week's new rows reach the run's summary, and when there are any, one owner-assigned issue
 *  (report-scheduled-run's `alert`). A week with none raises no alert, so the next clean run closes that issue. The
 *  list step's script runs here with `scripts/coupling-report` stubbed to print what the real one prints. */
class CouplingExemptionsWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/coupling-exemptions.yml")
  private lazy val list     = RepoFile.stepScript(workflow, "List the week's new exemptions")

  private val Listed =
    """# Coupling exemptions added since 7d (on origin/main, test/resources/coupling-exemptions.tsv)
      |
      |- 2026-11-19 abc123def `class` **services.identity.Clustering** (exempt, identity): pipeline stage: clusters a family's members
      |  commit: Clustering exempt
      |  trailer: Coupling: exempt Clustering — pipeline stage: clusters a family's members
      |- 2026-11-20 def456abc `class` **services.identity.Voting** (exempt, identity): pipeline stage: votes a cluster's film
      |  commit: Voting exempt
      |  trailer: none
      |""".stripMargin
  private val NoneListed =
    """# Coupling exemptions added since 7d (on origin/main, test/resources/coupling-exemptions.tsv)
      |
      |No exemption rows added.
      |""".stripMargin

  /** The list step run against a stub report printing `report`: (its exit, the summary, the outputs). */
  private def runList(report: String): (Int, String, String) = {
    val dir  = Files.createTempDirectory("coupling-exemptions")
    val stub = dir.resolve("scripts/coupling-report")
    Files.createDirectories(stub.getParent)
    Files.writeString(dir.resolve("report.md"), report)
    Files.writeString(stub, "#!/usr/bin/env bash\n[ \"$*\" = '--new-exemptions --since 7d' ] || exit 9\ncat \"$(dirname \"$0\")/../report.md\"\n")
    stub.toFile.setExecutable(true)
    val (summary, output) = (dir.resolve("summary.md"), dir.resolve("output"))
    Seq(summary, output).foreach(Files.writeString(_, ""))
    val exit = Process(Seq("bash", "-e", "-o", "pipefail", "-c", list), dir.toFile,
      "RUNNER_TEMP" -> dir.toString, "GITHUB_STEP_SUMMARY" -> summary.toString, "GITHUB_OUTPUT" -> output.toString).!
    (exit, Files.readString(summary), Files.readString(output))
  }

  "the list step" should "run the weekly report and put it in the run's summary" in {
    val (exit, summary, _) = runList(Listed)
    exit shouldBe 0
    summary should include("**services.identity.Clustering**")
    summary should include("**services.identity.Voting**")
  }

  it should "say how many rows are new when there are any, for the issue" in {
    val (_, _, output) = runList(Listed)
    output should startWith("found=2 new coupling exemption row(s) this week")
  }

  it should "find nothing in a week with no new rows, and still pass" in {
    val (exit, summary, output) = runList(NoneListed)
    exit shouldBe 0
    summary should include("No exemption rows added.")
    output shouldBe ""
  }

  "the report job" should "raise the owner's issue exactly when the list found rows" in {
    val report = RepoFile.jobs(RepoFile.withoutComments(workflow))("report")
    report should include("alert: ${{ needs.exemptions.outputs.found != '' }}")
    report should include("found: ${{ needs.exemptions.outputs.found }}")
    report should include("uses: ./.github/actions/report-scheduled-run")
  }
}
