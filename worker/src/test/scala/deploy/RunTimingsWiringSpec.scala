package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every run's job and step timings are reported by Report run timings (.github/workflows/report-timings.yml, which runs
 * .github/actions/report-timings: scripts/ci/ci_timings.py) into a log and onto Grafana's "CI — build timings"
 * dashboard. A workflow reported by nothing is charted nowhere -- silently.
 *
 * AFTER THE RUN, NEVER INSIDE IT (the owner, 2026-11-07, "do the structural fix"): a `timings` job inside a run made the
 * run's completion wait for a free runner to report it, and everything waiting on that completion waited too. Landing
 * batch #20 (13 changes, Pre-commit check run 42383678813) sat 27+ min past its last check job behind a `timings /
 * report` QUEUED for a light slot of a saturated ci-runner; Identity run 42370669779 ended its one real job 26 of its 57
 * min before the report could start. So Report run timings starts on `workflow_run: completed` of every workflow that
 * runs on its own, which carry no `timings` job.
 *
 * THE EXCEPTIONS, the workflows only another workflow's GITHUB_TOKEN dispatch starts -- such a run's end raises no
 * `workflow_run`: Identity model convergence DISPATCHES the report for its run as its last job (its end is THE GATE's
 * verdict, 2026-11-06), Verify scrape recording and Convergence bisect CALL it as their last job, `timings`, after
 * every other job but the scheduled-failure `report` (ScheduledRunReportSpec). The reusable `workflow_call`-only
 * workflows (`CI`, `Country convergence leg`) run, and are reported, inside their callers' runs.
 */
class RunTimingsWiringSpec extends AnyFlatSpec with Matchers {
  private val Reusable = ".github/workflows/report-timings.yml"
  private val Call     = s"uses: ./$Reusable"
  private val Dispatch = """gh workflow run report-timings.yml --ref "$GITHUB_REF_NAME" -f run-id="$GITHUB_RUN_ID""""
  // started only by a GITHUB_TOKEN dispatch, so no `workflow_run` reports them: their timings job reports from inside
  private val Dispatched = Set(".github/workflows/identity-model-convergence.yml")
  private val Called     = Set(".github/workflows/verify-scrape-recording.yml", ".github/workflows/convergence-bisect.yml")

  private def triggers(yml: String): Set[String] = {
    val on = RepoFile.block(yml, "on").linesIterator.toSeq
    val unit = on.drop(1).find(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length).getOrElse(0)
    on.collect { case line if line.takeWhile(_ == ' ').length == unit && line.trim.endsWith(":") => line.trim.stripSuffix(":") }.toSet
  }

  private lazy val builds: Seq[(String, String)] =
    RepoFile.workflows().map(f => f.getPath -> RepoFile.read(f.getPath))
      .filterNot { case (path, yml) => triggers(yml) == Set("workflow_call") || path == Reusable }

  private lazy val reusable = RepoFile.read(Reusable)

  /** The `name:` of every workflow whose completed runs start Report run timings. */
  private lazy val reported: Seq[String] = {
    val on = RepoFile.withoutComments(RepoFile.block(reusable, "workflow_run"))
    on.linesIterator.dropWhile(!_.trim.startsWith("workflows:")).drop(1).map(_.trim)
      .takeWhile(_.startsWith("- ")).map(_.stripPrefix("- ").trim).toSeq
  }

  private def needsOf(job: String): Set[String] =
    job.linesIterator.map(_.trim).collectFirst { case s"needs: $n" => n }.getOrElse("")
      .split("[\\[\\], ]+").filter(_.nonEmpty).toSet

  "every workflow that runs on its own" should "be found, or this spec checks nothing" in {
    builds.map(_._1) should contain(".github/workflows/main.yml")
    builds.map(_._1) should contain(".github/workflows/precommit-check.yml")
    builds.map(_._1) should not contain ".github/workflows/ci.yml"
  }

  it should "carry no timings job, its completion starting Report run timings instead" in {
    val problems = builds.filterNot { case (path, _) => Dispatched(path) || Called(path) }.flatMap { case (path, yml) =>
      val name = RepoFile.workflowName(yml)
      Seq(
        Option.when(RepoFile.jobs(yml).contains("timings"))(s"$path: a `timings` job holds its run's end until a runner reports it"),
        Option.when(RepoFile.jobs(yml).values.exists(needsOf(_).contains("timings")))(s"$path: a job needs `timings`"),
        Option.when(!reported.contains(name))(s"$path: `$name` is not in $Reusable's workflow_run workflows: charted nowhere")
      ).flatten
    }
    problems shouldBe empty
  }

  "Report run timings" should "name no workflow that is not one, nor one that reports from inside (it would report twice)" in {
    val names = builds.map { case (path, yml) => path -> RepoFile.workflowName(yml) }
    val inside = names.collect { case (path, name) if Dispatched(path) || Called(path) => name }.toSet
    reported.filterNot(names.map(_._2).toSet) shouldBe empty
    reported.filter(inside) shouldBe empty
    reported.distinct shouldBe reported
  }

  it should "start when such a run completes, report THAT run, and skip a cancelled one" in {
    triggers(reusable) shouldBe Set("workflow_run", "workflow_call", "workflow_dispatch")
    RepoFile.block(reusable, "workflow_run") should include("types: [completed]")
    reusable should include("run-id: ${{ inputs.run-id || github.event.workflow_run.id }}")
    RepoFile.jobs(reusable)("report") should include(
      "if: github.event_name != 'workflow_run' || github.event.workflow_run.conclusion != 'cancelled'")
  }

  "the workflows a GITHUB_TOKEN dispatch starts" should "report their timings last, after every other job but the failure report" in {
    val problems = builds.filter { case (path, _) => Dispatched(path) || Called(path) }.flatMap { case (path, yml) =>
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
    reusable should include("uses: ./.github/actions/report-timings")
    reusable should include("actions: read")
    reusable should include("grafana-annotation-token: ${{ secrets.GRAFANA_ANNOTATION_TOKEN }}")
  }

  it should "report a dispatched run only once that run has ended, under the run's own id" in {
    RepoFile.block(reusable, "workflow_dispatch") should include("run-id:")
    val wait = RepoFile.step(reusable, "Wait for the run it reports to end")
    wait should include("if: inputs.run-id != ''")
    wait should include("""gh run view "$RUN_ID" --json status --jq .status""")
    RepoFile.read(".github/actions/report-timings/action.yml") should include("""--run-id "${RUN_ID:-$GITHUB_RUN_ID}"""")
  }

  "the reusable workflows" should "carry no timings job of their own" in {
    RepoFile.workflows().map(f => RepoFile.read(f.getPath)).filter(triggers(_) == Set("workflow_call"))
      .foreach(yml => RepoFile.jobs(yml).keySet should not contain "timings")
  }
}
