package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * LANDING CHECKS AND MEASUREMENTS RUN ON CI-RUNNER, MAIN STAYS ON GITHUB'S HOSTED RUNNERS (the owner, 2026-11-01).
 * With the repository variable MOVIES_CI_RUNNERS=self-hosted, a landing check's jobs (precommit-check.yml, on a check/ branch)
 * ask for ci-runner's `movies-check` slots, a measurement's convergence legs (any ref off main: a measure/ branch) and every
 * convergence bisect for `movies-measure`, and the light jobs around them for `movies` (either kind of slot) -- the
 * slots are infra's nix/hosts/ci-runner/movies-runners.nix. Main's own runs never leave GitHub's hosted runners, so
 * they stop queueing behind the others for GitHub Free's 20. Unset, every job is hosted again: the fallback while
 * ci-runner is down (GitHubRunnerLabelSpec judges that side).
 */
class SelfHostedRunnersSpec extends AnyFlatSpec with Matchers {
  private val SelfHosted = Map("MOVIES_CI_RUNNERS" -> "self-hosted")
  private val Ours       = Set("movies-check", "movies-measure", "movies")
  private val Hosted     = Set("ubuntu-22.04", "ubuntu-24.04", "ubuntu-26.04", "ubuntu-latest", "macos-latest")
  private val RunsOn     = """(?m)^\s+runs-on:\s*(\$\{\{.*}}|\S+)""".r

  private def ctx(ref: String, vars: Map[String, String] = SelfHosted) =
    RunnerLabel.Context(RunnerLabel.GitHub, ref, vars = vars)

  private def yml(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))

  /** Every job's label in `file`; a job reading its plan job's output is judged by that output's expression. */
  private def labels(file: String, at: RunnerLabel.Context): Map[String, String] = {
    val code = yml(file)
    val jobs = RepoFile.jobs(code)
    def planned(runner: String) = runner match {
      case s"$${{ needs.$job.outputs.$key }}" =>
        RepoFile.block(jobs(job.trim), "outputs").linesIterator.map(_.trim)
          .collectFirst { case l if l.startsWith(s"${key.trim}:") => WorkflowText.stripComment(l.drop(key.trim.length + 1).trim) }
          .getOrElse(runner)
      case other => other
    }
    jobs.flatMap((name, job) => RunsOn.findFirstMatchIn(job).map(m => name -> RunnerLabel(planned(m.group(1).trim), at)))
  }

  private def legRunner(at: RunnerLabel.Context): String = {
    val line = """(?m)^\s+runner: (\$\{\{ inputs\.runner .*}})\s*$""".r
    RunnerLabel(line.findFirstMatchIn(yml("identity-model-convergence.yml")).get.group(1), at)
  }

  "a landing check" should "run every job on ci-runner's movies-check slots" in {
    val check = labels("precommit-check.yml", ctx("refs/heads/check/x"))
    check should not be empty
    all(check.values) shouldBe "movies-check"
  }

  "a measurement" should "run its convergence legs on movies-measure and the rest on ci-runner" in {
    val measure = ctx("refs/heads/measure/agent/abc-123")
    legRunner(measure) shouldBe "movies-measure"
    all(labels("identity-model-convergence.yml", measure).values) should (be("movies") or be("movies-measure"))
    all(labels("convergence-bisect.yml", ctx(RunnerLabel.Main)).values) should (be("movies") or be("movies-measure"))
    all(labels("report-timings.yml", measure).values) shouldBe "movies"
  }

  "Main and convergence on main" should "stay on GitHub's hosted runners even when ci-runner is switched on" in {
    val onMain = ctx(RunnerLabel.Main)
    legRunner(onMain) shouldBe "ubuntu-22.04"
    val main = Seq("main.yml", "ci.yml", "identity-model-convergence.yml", "report-timings.yml")
      .flatMap(f => labels(f, onMain).map((job, label) => s"$f: $job" -> label))
    main should not be empty
    withClue("a job of Main's on a self-hosted label: ")(main.filterNot((_, l) => Hosted(l)) shouldBe empty)
  }

  "with the variable unset" should "run every job on GitHub's hosted runners (the fallback)" in {
    val off = Seq("precommit-check.yml" -> "refs/heads/check/x", "identity-model-convergence.yml" -> "refs/heads/measure/y",
      "convergence-bisect.yml" -> RunnerLabel.Main, "report-timings.yml" -> "refs/heads/measure/y")
      .flatMap((f, ref) => labels(f, ctx(ref, Map.empty)).values)
    off should not be empty
    all(off.map(Hosted)) shouldBe true
    legRunner(ctx("refs/heads/measure/y", Map.empty)) shouldBe "ubuntu-22.04"
    Ours.foreach(l => off should not contain l)
  }
}
