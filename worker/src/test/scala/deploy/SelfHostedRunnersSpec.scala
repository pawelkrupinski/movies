package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * LANDING CHECKS AND MEASUREMENTS RUN ON CI-RUNNER, MAIN ON GITHUB'S HOSTED RUNNERS PLUS WHAT CI-RUNNER HAS FREE (the
 * owner, 2026-11-01; the second half 2026-11-06, which reversed "Main's own runs never leave GitHub's hosted runners").
 * With the repository variable MOVIES_CI_RUNNERS=self-hosted, a landing check's jobs (precommit-check.yml, on a check/ branch)
 * ask for ci-runner's `movies-check` slots, a measurement's convergence legs (any ref off main: a measure/ branch) and every
 * convergence bisect for `movies-measure`, and the light jobs around them for `movies` (either kind of slot) -- the
 * slots are infra's nix/hosts/ci-runner/movies-runners.nix, started per queued job by scripts/ci/runner_scheduler.py, which
 * gives bitcashier priority. Main's jobs run hosted, except the convergence rows the preflight hands `movies-main` --
 * as many as the scheduler last published free, 0 when it is stale -- on top of GitHub Free's 20. Unset, every job is
 * hosted again: the fallback while ci-runner is down (GitHubRunnerLabelSpec judges that side).
 */
class SelfHostedRunnersSpec extends AnyFlatSpec with Matchers {
  private val SelfHosted = Map("MOVIES_CI_RUNNERS" -> "self-hosted")
  private val Ours       = Set("movies-check", "movies-measure", "movies-main", "movies")
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

  "a measurement with MOVIES_MEASURE_RUNNERS=hosted" should "run its legs and bisects on hosted runners, the landing checks still on ci-runner" in {
    // ci-runner's 4 measurement slots queued runs 5-6 hours on 2026-11-02 while the hosted 20 sat mostly idle: the
    // owner's switch moves measurements back without moving the landing checks.
    val both    = SelfHosted + ("MOVIES_MEASURE_RUNNERS" -> "hosted")
    val measure = ctx("refs/heads/measure/agent/abc-123", both)
    legRunner(measure) shouldBe "ubuntu-22.04"
    labels("convergence-bisect.yml", ctx(RunnerLabel.Main, both)).values.toSet should contain ("ubuntu-22.04")
    labels("convergence-bisect.yml", ctx(RunnerLabel.Main, both)).values should not contain ("movies-measure")
    all(labels("precommit-check.yml", ctx("refs/heads/check/x", both)).values) shouldBe "movies-check"
    // a hand-dispatched runner still wins
    legRunner(ctx("refs/heads/measure/y", both).copy(inputs = Map("runner" -> "ubuntu-24.04-arm"))) shouldBe "ubuntu-24.04-arm"
  }

  private def rowRunner(phase: String, rowRunners: String): String = {
    val line = """(?m)^\s+runs-on:\s*(\$\{\{.*matrix\.phase.*}})\s*$""".r
    val at = ctx(RunnerLabel.Main).copy(inputs = Map("runner" -> "ubuntu-22.04", "row-runners" -> rowRunners),
      matrix = Map("phase" -> phase))
    RunnerLabel(line.findFirstMatchIn(yml("country-convergence-leg.yml")).get.group(1), at)
  }

  "main's convergence rows" should "run on ci-runner's movies-main slots where the plan names them, the rest hosted" in {
    val plan = """{"takes-drift-1":"movies-main","convergence":"movies-main"}"""
    rowRunner("takes-drift-1", plan) shouldBe "movies-main"
    rowRunner("convergence", plan) shouldBe "movies-main"
    rowRunner("takes", plan) shouldBe "ubuntu-22.04"
    rowRunner("takes", "{}") shouldBe "ubuntu-22.04"
  }

  "the convergence preflight" should "ask ci-runner's scheduler for free rows only on main with ci-runner on, and pass them on" in {
    val code = yml("identity-model-convergence.yml")
    val step = """(?s)- name: How many rows can ci-runner take\?\n(.*?)\n\s+- name:""".r.findFirstMatchIn(code).get.group(1)
    step should include ("github.ref == 'refs/heads/main' && vars.MOVIES_CI_RUNNERS == 'self-hosted' && inputs.runner == ''")
    step should include ("runner_scheduler.py rows-for-main")
    code should include ("SELF_HOSTED_ROWS: ${{ steps.capacity.outputs.self-hosted-rows || '0' }}")
    code should include ("row-runners:                   ${{ matrix.row-runners || '{}' }}")
  }

  "Main and convergence on main" should "otherwise stay on GitHub's hosted runners when ci-runner is switched on" in {
    val onMain = ctx(RunnerLabel.Main)
    legRunner(onMain) shouldBe "ubuntu-22.04"
    val main = Seq("main.yml", "ci.yml", "identity-model-convergence.yml", "report-timings.yml")
      .flatMap(f => labels(f, onMain).map((job, label) => s"$f: $job" -> label))
    main should not be empty
    withClue("a job of Main's on a self-hosted label: ")(main.filterNot((_, l) => Hosted(l)) shouldBe empty)
  }

  "every workflow that can run on ci-runner" should "set the UTF-8 locale a hosted VM sets and ci-runner's image does not" in {
    // unset, the JVM encodes file names as ASCII: a fixture path with a Polish letter throws InvalidPathException
    // (precommit-check run 41356052566). Workflow-level env does not cross into a called workflow, so each declares it.
    val TopLevelLang = """(?m)^env:\n(?:[ \t]+.*\n|\s*\n)*?[ \t]+LANG: C\.UTF-8\s*$""".r
    val missing = Seq("precommit-check.yml", "identity-model-convergence.yml", "country-convergence-leg.yml",
      "convergence-bisect.yml", "report-timings.yml").filterNot(f => TopLevelLang.findFirstIn(yml(f)).isDefined)
    missing shouldBe empty
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
