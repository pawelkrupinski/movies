package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every workflow reports its own run's job and step timings as its LAST job, `timings` -- a call of the reusable
 * .github/workflows/report-timings.yml, which runs .github/actions/report-timings (scripts/ci/ci_timings.py), one copy
 * of the job where 27 workflows each held their own: into its own log and summary, and onto
 * Grafana's "CI — build timings" dashboard. A workflow without the job is charted nowhere, and a
 * `timings` that does not wait for every other job reports a run with jobs missing -- both silent.
 *
 * Only the scheduled-failure `report` comes after it (report-scheduled-run needs every other job,
 * ScheduledRunReportSpec). The reusable `workflow_call`-only workflows (`CI`, `Country convergence
 * leg`) carry none: their jobs run, and are reported, inside their callers' runs; `Report run timings` is the
 * job they call.
 */
class RunTimingsWiringSpec extends AnyFlatSpec with Matchers {
  private val Reusable = ".github/workflows/report-timings.yml"
  private val Call     = s"uses: ./$Reusable"

  private def triggers(yml: String): Set[String] = {
    val on = RepoFile.block(yml, "on").linesIterator.toSeq
    val unit = on.drop(1).find(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length).getOrElse(0)
    on.collect { case line if line.takeWhile(_ == ' ').length == unit && line.trim.endsWith(":") => line.trim.stripSuffix(":") }.toSet
  }

  private lazy val builds: Seq[(String, String)] =
    RepoFile.workflows().map(f => f.getPath -> RepoFile.read(f.getPath))
      .filterNot { case (_, yml) => triggers(yml) == Set("workflow_call") }

  private def needsOf(job: String): Set[String] =
    job.linesIterator.map(_.trim).collectFirst { case s"needs: $n" => n }.getOrElse("")
      .split("[\\[\\], ]+").filter(_.nonEmpty).toSet

  "every workflow that runs on its own" should "be found, or this spec checks nothing" in {
    builds.map(_._1) should contain(".github/workflows/main.yml")
    builds.map(_._1) should not contain ".github/workflows/ci.yml"
  }

  it should "report its timings last, after every other job but the failure report" in {
    val problems = builds.flatMap { case (path, yml) =>
      val jobs = RepoFile.jobs(yml)
      jobs.get("timings") match {
        case None => Seq(s"$path has no `timings` job")
        case Some(timings) =>
          val others = jobs.keySet - "timings" - "report"
          Seq(
            Option.when(!timings.contains(Call))(s"$path: timings does not call $Reusable"),
            Option.when(needsOf(timings) != others)(s"$path: timings needs ${needsOf(timings).toSeq.sorted}, not every other job ${others.toSeq.sorted}"),
            Option.when(!timings.contains("!cancelled()"))(s"$path: timings is skipped when a job it needs fails"),
            Option.when(!timings.contains("actions: read"))(s"$path: timings cannot read the run's jobs"),
            Option.when(!timings.contains("GRAFANA_ANNOTATION_TOKEN: ${{ secrets.GRAFANA_ANNOTATION_TOKEN }}"))(
              s"$path: timings passes report-timings no Grafana token: nothing reaches the dashboard"),
            jobs.get("report").flatMap(r => Option.when(!needsOf(r).contains("timings"))(s"$path: report runs before timings"))
          ).flatten
      }
    }
    problems shouldBe empty
  }

  "the reusable timings workflow" should "run the report-timings action, able to read the run's jobs" in {
    val reusable = RepoFile.read(Reusable)
    triggers(reusable) shouldBe Set("workflow_call")
    reusable should include("uses: ./.github/actions/report-timings")
    reusable should include("actions: read")
    reusable should include("grafana-annotation-token: ${{ secrets.GRAFANA_ANNOTATION_TOKEN }}")
  }

  "the reusable workflows" should "carry no timings job of their own" in {
    RepoFile.workflows().map(f => RepoFile.read(f.getPath)).filter(triggers(_) == Set("workflow_call"))
      .foreach(yml => RepoFile.jobs(yml).keySet should not contain "timings")
  }
}
