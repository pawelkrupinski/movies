package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A THIRD PARTY'S OR A BACKLOG'S PROBLEM IS AN ALERT, NOT A RED BUILD (ci-failures-2026-10-30, recommendation 2). In five
 * days, providers turned 34 runs red (~45 h: Decodo 21 runs in a row, security headers 6, OMDb's quota) and backlogs and
 * meta-alarms 11 more; each red was a fixer dispatch candidate for an outage only the owner could act on. Each such check
 * now runs its live step through scripts/ci/alert-step.sh, which keeps the run green on the check's third-party exit
 * codes and writes `alert` / `found`; its job exposes them and the `report` job hands them to
 * .github/actions/report-scheduled-run, which keeps ONE issue (refreshed, then closed by the next clean run). Any other
 * exit -- and every self-test step -- still fails red: a regression of our own code.
 */
class AlertIsNotRedSpec extends AnyFlatSpec with Matchers {

  /** workflow -> (the job that judges, its live step's id, the command alert-step.sh runs, the alert exit codes). */
  private val ThirdPartyChecks: Map[String, (String, String, String, String)] = Map(
    "decodo-probe.yml"             -> ("probe", "probe", "python3 scripts/ci/decodo-probe.py", "1,2"),
    "security-headers.yml"         -> ("check", "headers", "python3 scripts/ci/check-security-headers.py", "3"),
    "record-scrape-fixtures.yml"   -> ("preflight", "keys", "scripts/ci/recording-preflight.sh", "1"),
    "identity-findings-report.yml" -> ("findings", "report", "python3 scripts/ci/identity_findings_report.py", "2"),
    "identity-ci-staleness.yml"    -> ("check", "staleness", "scripts/ci/identity-ci-staleness.sh", "1"),
  )

  private def code(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))

  "every third-party and backlog check" should "turn its third-party exits into an alert the report job files" in {
    val problems = ThirdPartyChecks.toSeq.sortBy(_._1).flatMap { case (file, (job, step, command, codes)) =>
      val jobs   = RepoFile.jobs(code(file))
      val judge  = jobs.getOrElse(job, "")
      val report = jobs.getOrElse("report", "")
      Seq(
        Option.when(!judge.contains(s"id: $step"))(s"$file: job $job has no step `$step`"),
        Option.when(!judge.contains(s"scripts/ci/alert-step.sh $codes -- $command"))(s"$file: $command not through alert-step.sh $codes"),
        Option.when(!judge.contains(s"alert: $${{ steps.$step.outputs.alert }}"))(s"$file: job $job exposes no alert"),
        Option.when(!judge.contains(s"found: $${{ steps.$step.outputs.found }}"))(s"$file: job $job exposes no found"),
        Option.when(!report.contains(s"alert: $${{ needs.$job.outputs.alert }}"))(s"$file: report passes no alert"),
        Option.when(!report.contains(s"found: $${{ needs.$job.outputs.found }}"))(s"$file: report passes no found"),
      ).flatten
    }
    problems shouldBe empty
  }

  "the recording" should "skip its legs on a refused key, not spend them failing" in {
    RepoFile.jobs(code("record-scrape-fixtures.yml"))("enrichment") should include(
      "if: ${{ needs.preflight.outputs.alert != 'true' }}")
  }

  "the reporter" should "file an alert as the workflow's one issue" in {
    val action = RepoFile.read(".github/actions/report-scheduled-run/action.yml")
    action should include("verdict=alert")
    RepoFile.read("scripts/ci/scheduled-failure-issue.sh") should include("gh issue edit \"$open_issue\" --body \"$body\"")
  }
}
