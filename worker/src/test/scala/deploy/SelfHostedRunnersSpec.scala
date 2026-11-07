package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * LANDING CHECKS AND MEASUREMENTS RUN ON CI-RUNNER, MAIN STAYS ON GITHUB'S HOSTED RUNNERS (the owner, 2026-11-01; again
 * 2026-11-06, reversing that day's "main's convergence takes ci-runner's free slots": "run main branch convergence and
 * Main workflow always on gh, measurements always on ci" -- so no `movies-main` label, and no MOVIES_MEASURE_RUNNERS
 * escape back to hosted for a measurement).
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

  "a measurement" should "stay on ci-runner whatever MOVIES_MEASURE_RUNNERS says (no way back to hosted, 2026-11-06)" in {
    // the owner: "measurements always on ci" -- the 2026-11-02 escape (MOVIES_MEASURE_RUNNERS=hosted) is gone, so a
    // measurement never queues GitHub's 20 that main's suite runs on
    val both    = SelfHosted + ("MOVIES_MEASURE_RUNNERS" -> "hosted")
    legRunner(ctx("refs/heads/measure/agent/abc-123", both)) shouldBe "movies-measure"
    all(labels("convergence-bisect.yml", ctx(RunnerLabel.Main, both)).values) should (be("movies") or be("movies-measure"))
    Seq("identity-model-convergence.yml", "convergence-bisect.yml", "country-convergence-leg.yml")
      .foreach(f => withClue(s"$f: ")(yml(f) should not include ("MOVIES_MEASURE_RUNNERS")))
    // a hand-dispatched runner still wins
    legRunner(ctx("refs/heads/measure/y", both).copy(inputs = Map("runner" -> "ubuntu-24.04-arm"))) shouldBe "ubuntu-24.04-arm"
  }

  "Main and convergence on main" should "stay on GitHub's hosted runners even when ci-runner is switched on" in {
    val onMain = ctx(RunnerLabel.Main).copy(workflow = "Main")
    legRunner(onMain) shouldBe "ubuntu-22.04"
    // every row of a leg on the leg's one runner: no per-row label (2026-11-06's `row-runners`, retired the same day)
    val leg = yml("country-convergence-leg.yml")
    leg should not include ("row-runners")
    leg should not include ("movies-main")
    yml("identity-model-convergence.yml") should not include ("runner_scheduler.py")
    val main = Seq("main.yml", "ci.yml", "identity-model-convergence.yml", "report-timings.yml")
      .flatMap(f => labels(f, onMain).map((job, label) => s"$f: $job" -> label))
    main should not be empty
    withClue("a job of Main's on a self-hosted label: ")(main.filterNot((_, l) => Hosted(l)) shouldBe empty)
  }

  /** THE GATE's workflows and what they call, the landing check, and Branch CI (it is ci.yml, Main's own suite): they
   *  keep their own rules above. */
  private val GateSide = Set("main.yml", "ci.yml", "identity-model-convergence.yml", "country-convergence-leg.yml",
    "report-timings.yml", "precommit-check.yml", "branch-ci.yml")
  /** What ci-runner's job container cannot give: macOS, and an Android emulator (no /dev/kvm proven in it). */
  private val NeedsHosted = Set("ios.yml: ui-tests", "android.yml: instrumented", "order-independence.yml: swift",
    "order-independence.yml: ios", "order-independence.yml: android-instrumented")

  "every other workflow" should "run on ci-runner, bitcashier first, whatever its ref (the owner, 2026-11-06)" in {
    // "distribute other workflows as well": the nightly explorers, the recording, the bots, Stage NixOS closures ...
    // nothing a push to main waits on takes one of GitHub's 20 that THE GATE runs on
    val onMain = ctx(RunnerLabel.Main)
    val jobs = RepoFile.workflows().map(_.getName).filterNot(GateSide).sorted
      .flatMap(f => labels(f, onMain).map((job, label) => s"$f: $job" -> label))
    jobs.size should be > 60
    withClue("off ci-runner: ")(jobs.filterNot((j, l) => NeedsHosted(j) || l == "movies" || l == "movies-measure") shouldBe empty)
    withClue("needs a hosted machine: ")(jobs.filter((j, _) => NeedsHosted(j)).filterNot((_, l) => Hosted(l)) shouldBe empty)
    // the recording's and the verify's convergence legs: measurement slots
    for (f <- Seq("record-scrape-fixtures.yml", "verify-scrape-recording.yml")) {
      val runner = RepoFile.block(RepoFile.jobs(yml(f))("preflight"), "outputs").linesIterator.map(_.trim)
        .collectFirst { case l if l.startsWith("runner:") => l.drop("runner:".length).trim }.get
      withClue(s"$f: ")(RunnerLabel(runner, onMain) shouldBe "movies-measure")
    }
  }

  "the timings report" should "run where its caller does: hosted for THE GATE's workflows on main, ci-runner otherwise" in {
    def report(workflow: String, ref: String = RunnerLabel.Main) =
      labels("report-timings.yml", ctx(ref).copy(workflow = workflow)).values.toSet
    report("Main") shouldBe Set("ubuntu-26.04")
    report("Identity model convergence") shouldBe Set("ubuntu-26.04")
    report("Identity model convergence", "refs/heads/measure/x") shouldBe Set("movies")
    report("Identity drift explorer") shouldBe Set("movies")
  }

  it should "run where the run it reports did, when a workflow_run starts it (always on main, as Report run timings)" in {
    def reported(name: String, branch: String) =
      labels("report-timings.yml", ctx(RunnerLabel.Main).copy(workflow = "Report run timings",
        workflowRun = Map("name" -> name, "head_branch" -> branch))).values.toSet
    reported("Main", "main") shouldBe Set("ubuntu-26.04")
    reported("Main", "feature") shouldBe Set("movies")
    reported("Pre-commit check", "check/abc") shouldBe Set("movies")
    reported("Identity run", "main") shouldBe Set("movies")
  }

  "every workflow that can run on ci-runner" should "set the UTF-8 locale a hosted VM sets and ci-runner's image does not" in {
    // unset, the JVM encodes file names as ASCII: a fixture path with a Polish letter throws InvalidPathException
    // (precommit-check run 41356052566). Workflow-level env does not cross into a called workflow, so each declares it.
    val TopLevelLang = """(?m)^env:\n(?:[ \t]+.*\n|\s*\n)*?[ \t]+LANG: C\.UTF-8\s*$""".r
    val onCiRunner = RepoFile.workflows().map(_.getName).sorted
      .filter(f => labels(f, ctx("refs/heads/measure/x")).values.exists(Ours) || f == "country-convergence-leg.yml")
    onCiRunner.size should be > 30
    val missing = onCiRunner.filterNot(f => TopLevelLang.findFirstIn(yml(f)).isDefined)
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
