package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every SCHEDULED workflow reports its own failure to a person.
 *
 * A red cron run notifies, at most, whoever last edited its `cron:` line — that is how the
 * OG-card generator ran a week on blank proxy credentials with nobody told, and how the weekly
 * roster audit's findings would have sat in the Actions tab. `.github/actions/report-scheduled-run`
 * turns a failed scheduled run into an owner-assigned issue and closes it on the next green one;
 * this pins that every workflow with a `schedule:` trigger has a job running it, after EVERY
 * other job, on schedule only, with the `issues: write` it needs. Enumerated from the directory,
 * so a scheduled workflow added tomorrow is held to it the day it lands.
 */
class ScheduledRunReportSpec extends AnyFlatSpec with Matchers {
  private val Action = "uses: ./.github/actions/report-scheduled-run"

  private lazy val scheduled: Seq[(String, String)] =
    RepoFile.workflows().map(f => f.getName -> RepoFile.read(f.getPath))
      .filter { case (_, yml) =>
        scala.util.Try(RepoFile.block(yml, "on")).getOrElse("").linesIterator.exists(_.trim == "schedule:")
      }

  "the scheduled workflows" should "be found, or this spec checks nothing" in {
    scheduled.map(_._1) should contain("roster-audit.yml")
  }

  they should "each report a failed scheduled run through report-scheduled-run, after every other job" in {
    val problems = scheduled.flatMap { case (name, yml) =>
      val all = RepoFile.jobs(yml)
      all.collectFirst { case (job, body) if body.contains(Action) => job -> body } match {
        case None => Seq(s"$name has no job using report-scheduled-run")
        case Some((job, body)) =>
          val others = all.keySet - job
          val needs = body.linesIterator.map(_.trim).collectFirst { case s"needs: $n" => n }.getOrElse("")
          Seq(
            Option.when(!others.forall(o => needs.split("[\\[\\], ]+").contains(o)))(
              s"$name: $job needs [$needs], not every other job ${others.toSeq.sorted.mkString(", ")}"),
            Option.when(!body.contains("github.event_name == 'schedule'"))(s"$name: $job does not run on schedule only"),
            Option.when(!body.contains("always()"))(s"$name: $job is skipped when a job it needs fails"),
            Option.when(!body.contains("issues: write"))(s"$name: $job lacks issues: write")
          ).flatten
      }
    }
    problems shouldBe empty
  }
}
