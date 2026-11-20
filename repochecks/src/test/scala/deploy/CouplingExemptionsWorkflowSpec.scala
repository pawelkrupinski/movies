package deploy

import java.nio.file.Files

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.sys.process.Process

/** The weekly exemption report (`.github/workflows/coupling-exemptions.yml`; the owner, 2026-11-19: agent exemptions
 *  "reported weekly"). The week's new rows reach the run's summary, and when there are any, one owner-assigned issue
 *  (report-scheduled-run's `alert`). A week with none raises no alert, so the next clean run closes that issue. The
 *  same report lists the week's `Fix-Replay: none` exemptions (the owner, 2026-11-20: a gate red's fix that agentctl
 *  ran no replay of, "listed in the owner's weekly report like exemptions"). The list step's script runs here with
 *  `scripts/coupling-report` and `scripts/ci/fix_replay_exemptions.py` stubbed to print what the real ones print. And
 *  it names the STALE exemptions (the owner, 2026-11-20): the slack step's `sbt couplingExemptionSlack`, stubbed here,
 *  lists every `class` row whose suggested cut would bring its class within the fan-out limit, and any raises the issue. */
class CouplingExemptionsWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/coupling-exemptions.yml")
  private lazy val list     = RepoFile.stepScript(workflow, "List the week's new exemptions")
  private lazy val slack    = RepoFile.stepScript(workflow, "Read every class exemption's slack")

  private val Slack =
    """# Exemption slack
      |
      |Every `class` exemption row: 1 of 2 would come within the limit (9): `Asker` 12 -> 9.
      |
      |## `services.identityadapters.Asker`: 12 -> 9 (within the limit: the exemption may be stale)
      |
      |## `services.movies.Repository`: 20 -> 18
      |""".stripMargin

  /** The slack step run against a stub sbt writing `report` to the task's out path: (its exit, the summary, the outputs). */
  private def runSlack(report: String): (Int, String, String) = {
    val dir  = Files.createTempDirectory("coupling-slack")
    val bin  = Files.createDirectories(dir.resolve("bin"))
    Files.writeString(dir.resolve("slack-report.md"), report)
    val sbt  = bin.resolve("sbt")
    Files.writeString(sbt, "#!/usr/bin/env bash\nset -- $1\n[ \"$1\" = couplingExemptionSlack ] || exit 9\ncp \"$(dirname \"$0\")/../slack-report.md\" \"$2\"\n")
    sbt.toFile.setExecutable(true)
    val (summary, output) = (dir.resolve("summary.md"), dir.resolve("output"))
    Seq(summary, output).foreach(Files.writeString(_, ""))
    // the stub first on the step's PATH, set by the shell itself
    val exit = Process(Seq("bash", "-e", "-o", "pipefail", "-c", s"PATH=\"$bin:$$PATH\"\n$slack"), dir.toFile,
      "RUNNER_TEMP" -> dir.toString, "GITHUB_STEP_SUMMARY" -> summary.toString, "GITHUB_OUTPUT" -> output.toString).!
    (exit, Files.readString(summary), Files.readString(output))
  }

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

  private val NonesListed =
    """# Fix-Replay exemptions since 7d (on origin/main)
      |
      |- 2026-11-20 0a1b2c3d4 Fixes-Red: 2600: the hosted runner lost its disk mid-job (infra); no code path replays it
      |  commit: web: the fix
      |""".stripMargin
  private val NoNones =
    """# Fix-Replay exemptions since 7d (on origin/main)
      |
      |No `Fix-Replay: none` exemptions.
      |""".stripMargin

  /** The list step run against stub reports printing `report` and `nones`: (its exit, the summary, the outputs). */
  private def runList(report: String, nones: String = NoNones): (Int, String, String) = {
    val dir  = Files.createTempDirectory("coupling-exemptions")
    val stub = dir.resolve("scripts/coupling-report")
    val replays = dir.resolve("scripts/ci/fix_replay_exemptions.py")
    Files.createDirectories(replays.getParent)
    Files.writeString(dir.resolve("report.md"), report)
    Files.writeString(dir.resolve("nones.md"), nones)
    Files.writeString(stub, "#!/usr/bin/env bash\n[ \"$*\" = '--new-exemptions --since 7d' ] || exit 9\ncat \"$(dirname \"$0\")/../report.md\"\n")
    Files.writeString(replays, "#!/usr/bin/env bash\n[ \"$*\" = '--since 7d' ] || exit 9\ncat \"$(dirname \"$0\")/../../nones.md\"\n")
    Seq(stub, replays).foreach(_.toFile.setExecutable(true))
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
    output should startWith("found=2 new coupling exemption row(s) and 0 Fix-Replay: none exemption(s) this week")
  }

  it should "list the week's Fix-Replay: none exemptions in the summary and raise the issue for them alone" in {
    val (exit, summary, output) = runList(NoneListed, NonesListed)
    exit shouldBe 0
    summary should include("Fixes-Red: 2600: the hosted runner lost its disk mid-job (infra)")
    output should startWith("found=0 new coupling exemption row(s) and 1 Fix-Replay: none exemption(s) this week")
  }

  it should "find nothing in a week with no new rows, and still pass" in {
    val (exit, summary, output) = runList(NoneListed)
    exit shouldBe 0
    summary should include("No exemption rows added.")
    summary should include("No `Fix-Replay: none` exemptions.")
    output shouldBe ""
  }

  "the slack step" should "put every class exemption's slack in the summary and count the stale ones for the issue" in {
    val (exit, summary, output) = runSlack(Slack)
    exit shouldBe 0
    summary should include("## `services.movies.Repository`: 20 -> 18")
    output should startWith("stale=1 class exemption(s) whose suggested cut would bring the class within the fan-out limit")
  }

  it should "find nothing stale when no class would come within the limit, and still pass" in {
    val (exit, _, output) = runSlack(Slack.linesIterator.filterNot(_.contains("may be stale")).mkString("\n"))
    exit shouldBe 0
    output shouldBe ""
  }

  "the report job" should "raise the owner's issue exactly when the list found rows or the slack a stale exemption" in {
    val report = RepoFile.jobs(RepoFile.withoutComments(workflow))("report")
    report should include("needs: [exemptions, slack]")
    report should include("alert: ${{ needs.exemptions.outputs.found != '' || needs.slack.outputs.stale != '' }}")
    report should include("found: ${{ needs.exemptions.outputs.found }} ${{ needs.slack.outputs.stale }}")
    report should include("uses: ./.github/actions/report-scheduled-run")
  }
}
