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
 * ScheduledRunReportSpec).
 *
 * OFF THE GATE'S CRITICAL PATH (2026-11-06): in THE GATE's convergence workflow, whose run's END is the gate's verdict,
 * the report ran 40-70 s after every other job had ended. There `timings` only DISPATCHES report-timings.yml for its
 * run (a few seconds), and that run of its own waits for this one to end and reports it. Report run timings, a run that
 * reports another, carries no timings job of its own. The reusable `workflow_call`-only workflows (`CI`, `Country convergence
 * leg`) carry none: their jobs run, and are reported, inside their callers' runs; `Report run timings` is the
 * job they call.
 */
class RunTimingsWiringSpec extends AnyFlatSpec with Matchers {
  private val Reusable = ".github/workflows/report-timings.yml"
  private val Call     = s"uses: ./$Reusable"
  private val Dispatch = """gh workflow run report-timings.yml --ref "$GITHUB_REF_NAME" -f run-id="$GITHUB_RUN_ID""""
  // the workflows whose timings are a dispatched run of their own (THE GATE's convergence: its end is the verdict)
  private val Dispatched = Set(".github/workflows/identity-model-convergence.yml")

  private def triggers(yml: String): Set[String] = {
    val on = RepoFile.block(yml, "on").linesIterator.toSeq
    val unit = on.drop(1).find(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length).getOrElse(0)
    on.collect { case line if line.takeWhile(_ == ' ').length == unit && line.trim.endsWith(":") => line.trim.stripSuffix(":") }.toSet
  }

  private lazy val builds: Seq[(String, String)] =
    RepoFile.workflows().map(f => f.getPath -> RepoFile.read(f.getPath))
      .filterNot { case (path, yml) => triggers(yml) == Set("workflow_call") || path == Reusable }

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
          val dispatched = Dispatched(path)
          Seq(
            Option.when(!dispatched && !timings.contains(Call))(s"$path: timings does not call $Reusable"),
            Option.when(dispatched && !timings.contains(Dispatch))(s"$path: timings does not dispatch $Reusable for its run"),
            Option.when(dispatched && !timings.contains("actions: write"))(s"$path: timings cannot dispatch the report"),
            Option.when(needsOf(timings) != others)(s"$path: timings needs ${needsOf(timings).toSeq.sorted}, not every other job ${others.toSeq.sorted}"),
            Option.when(!timings.contains("!cancelled()"))(s"$path: timings is skipped when a job it needs fails"),
            Option.when(!dispatched && !timings.contains("actions: read"))(s"$path: timings cannot read the run's jobs"),
            // `inherit` rather than the secrets by name: report-timings.yml declares no secrets.
            Option.when(!dispatched && !timings.contains("secrets: inherit"))(
              s"$path: timings does not inherit secrets: report-timings gets no Grafana token, nothing reaches the dashboard"),
            jobs.get("report").flatMap(r => Option.when(!needsOf(r).contains("timings"))(s"$path: report runs before timings"))
          ).flatten
      }
    }
    problems shouldBe empty
  }

  "the reusable timings workflow" should "run the report-timings action, able to read the run's jobs" in {
    val reusable = RepoFile.read(Reusable)
    triggers(reusable) shouldBe Set("workflow_call", "workflow_dispatch")
    reusable should include("uses: ./.github/actions/report-timings")
    reusable should include("actions: read")
    reusable should include("grafana-annotation-token: ${{ secrets.GRAFANA_ANNOTATION_TOKEN }}")
  }

  it should "report a dispatched run only once that run has ended, under the run's own id" in {
    val reusable = RepoFile.read(Reusable)
    RepoFile.block(reusable, "workflow_dispatch") should include("run-id:")
    val wait = RepoFile.step(reusable, "Wait for the run it reports to end")
    wait should include("if: inputs.run-id != ''")
    wait should include("""gh run view "$RUN_ID" --json status --jq .status""")
    reusable should include("run-id: ${{ inputs.run-id }}")
    RepoFile.read(".github/actions/report-timings/action.yml") should include("""--run-id "${RUN_ID:-$GITHUB_RUN_ID}"""")
  }

  "the reusable workflows" should "carry no timings job of their own" in {
    RepoFile.workflows().map(f => RepoFile.read(f.getPath)).filter(triggers(_) == Set("workflow_call"))
      .foreach(yml => RepoFile.jobs(yml).keySet should not contain "timings")
  }
}
