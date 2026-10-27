package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * CI RED MEANS ONLY A REGRESSION; WORK PAST A DEADLINE IS A TASK. The Identity findings report was red for days
 * because a finding stayed untriaged past its 3-day limit, and that noise is why its red got ignored (the owner,
 * 2026-10-27). A check that judges lateness or debt -- untriaged findings, a red streak gone on too long, a recording
 * overdue -- now ends green, warns, and writes `overdue=true` to its step's output; its job exposes it, and the
 * workflow's `report` job hands it to .github/actions/report-scheduled-run as `overdue:`, which opens (or comments on)
 * the owner-assigned issue without a red run (scripts/ci/scheduled-failure-issue.sh `overdue`). Red is left for what
 * the check could not do.
 *
 *  - every deadline check below is wired that way, end to end;
 *  - no CI script reports lateness as an `::error` (the annotation a red run carries).
 */
class OverdueIsNotRedSpec extends AnyFlatSpec with Matchers {

  /** workflow -> (the job that judges, the step id whose output says overdue, the script it runs). */
  private val DeadlineChecks: Map[String, (String, String, String)] = Map(
    "identity-findings-report.yml" -> ("findings", "report", "scripts/ci/identity_findings_report.py"),
    "identity-ci-staleness.yml"    -> ("check", "staleness", "scripts/ci/identity-ci-staleness.sh"),
    // A source disagreeing with the cinema roster is data drift, the owner's fix (tools.RosterAudit.verdict).
    "roster-audit.yml"             -> ("roster-audit", "audit", "scripts/ci/roster-audit.sh"),
  )

  private def code(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))

  "every deadline check" should "hand its overdue output to the report job, not fail the run" in {
    val problems = DeadlineChecks.toSeq.sortBy(_._1).flatMap { case (file, (job, step, script)) =>
      val jobs   = RepoFile.jobs(code(file))
      val judge  = jobs.getOrElse(job, "")
      val report = jobs.getOrElse("report", "")
      Seq(
        Option.when(!judge.contains(s"overdue: $${{ steps.$step.outputs.overdue }}"))(s"$file: job $job exposes no overdue"),
        Option.when(!judge.contains(s"id: $step"))(s"$file: job $job has no step `$step`"),
        Option.when(!report.contains(s"overdue: $${{ needs.$job.outputs.overdue }}"))(s"$file: report passes no overdue"),
        Option.when(!RepoFile.read(script).contains("overdue="))(s"$script writes no overdue= output"),
      ).flatten
    }
    problems shouldBe empty
  }

  "a CI script" should "never report lateness as an error" in {
    lateErrors(ciScripts.map(path => path -> RepoFile.read(path))) shouldBe empty
  }

  it should "be caught when it does" in {
    lateErrors(Seq("x.py" -> """print(f"::error title=t::{n} finding(s) untriaged for more than {d} days")""")) should
      have size 1
    lateErrors(Seq("x.sh" -> """echo "::error::recording-stale: last success 40h ago is overdue"""")) should have size 1
    lateErrors(Seq("x.sh" -> """echo "::error::the recording failed"""")) shouldBe empty
  }

  private val Late = """(?i)untriaged|overdue|past (its|the) deadline|more than \S+ days""".r

  private def lateErrors(files: Seq[(String, String)]): Seq[String] = for {
    (path, text)  <- files
    (line, index) <- text.linesIterator.zipWithIndex.toSeq
    if line.contains("::error") && Late.findFirstIn(line).nonEmpty
  } yield s"$path:${index + 1}: $line"

  private def ciScripts: Seq[String] =
    RepoFile.listed("scripts/ci")(f =>
      (f.getName.endsWith(".sh") || f.getName.endsWith(".py")) && !f.getName.endsWith("-test.sh") &&
        !f.getName.endsWith("_test.py")
    ).map(_.getPath)
}
