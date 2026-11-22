package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * LANDING CHECKS AND MEASUREMENTS RUN ON CI-RUNNER, MAIN STAYS ON GITHUB'S HOSTED RUNNERS (the owner, 2026-11-01; again
 * 2026-11-06, reversing that day's "main's convergence takes ci-runner's free slots": "run main branch convergence and
 * Main workflow always on gh, measurements always on ci" -- so no `movies-main` label, and no MOVIES_MEASURE_RUNNERS
 * escape back to hosted for a measurement).
 * With the repository variable MOVIES_CI_RUNNERS=self-hosted, a landing check's jobs (precommit-check.yml, on a check/ branch)
 * ask for ci-runner's `movies-check` slots -- but its identity shards, GitHub's hosted runners either way (the owner,
 * 2026-11-21: ci-runner's check CPU was the landing's binding constraint) --, a measurement's convergence legs (any ref off main: a measure/ branch) and every
 * convergence bisect for `movies-measure`, and the light jobs around them for `movies` (either kind of slot) -- the
 * slots are infra's nix/hosts/ci-runner/movies-runners.nix. Main's own runs never leave GitHub's hosted runners, so
 * they stop queueing behind the others for GitHub Free's 20. Unset, every job is hosted again: the fallback while
 * ci-runner is down (GitHubRunnerLabelSpec judges that side).
 */
class SelfHostedRunnersSpec extends AnyFlatSpec with Matchers {
  private val SelfHosted = Map("MOVIES_CI_RUNNERS" -> "self-hosted")
  private val Ours       = Set("movies-check", "movies-check-red-fix", "movies-measure", "movies-explore", "movies")
  private val Hosted     = Set("ubuntu-22.04", "ubuntu-24.04", "ubuntu-26.04", "ubuntu-latest", "macos-latest")
  private val RunsOn     = """(?m)^\s+runs-on:\s*(\$\{\{.*}}|\S+)""".r

  private def ctx(ref: String, vars: Map[String, String] = SelfHosted) =
    RunnerLabel.Context(RunnerLabel.GitHub, ref, vars = vars)

  private def yml(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))

  private val Call     = """(?m)^\s+uses:\s*\./\.github/workflows/(\S+\.yml)\s*$""".r
  private val InputRef = """inputs\.([\w-]+)""".r

  /** A runner expression as its job reads it: one reading its plan job's output, that output's expression. */
  private def planned(jobs: Map[String, String], runner: String): String = runner match {
    case s"$${{ needs.$job.outputs.$key }}" =>
      RepoFile.block(jobs(job.trim), "outputs").linesIterator.map(_.trim)
        .collectFirst { case l if l.startsWith(s"${key.trim}:") => WorkflowText.stripComment(l.drop(key.trim.length + 1).trim) }
        .getOrElse(runner)
    case other => other
  }

  /** Every job's own `runs-on:` expression in `file`, as [[planned]]. */
  private def runsOn(file: String): Map[String, String] = {
    val jobs = RepoFile.jobs(yml(file))
    jobs.flatMap((name, job) => RunsOn.findFirstMatchIn(job).map(m => name -> planned(jobs, m.group(1).trim)))
  }

  /** Every reusable call in `file`: the calling job, the called workflow, and what its `with:` passes, as [[planned]]. */
  private def calls(file: String): Seq[(String, String, Map[String, String])] = {
    val jobs = RepoFile.jobs(yml(file))
    jobs.toSeq.flatMap((name, job) => Call.findFirstMatchIn(job).map { m =>
      val passed =
        if (!job.linesIterator.exists(_.trim == "with:")) Map.empty[String, String]
        else RepoFile.block(job, "with").linesIterator.drop(1).map(l => WorkflowText.stripComment(l).trim)
          .collect { case s"$key:$value" if value.trim.nonEmpty => key.trim -> planned(jobs, value.trim) }.toMap
      (name, m.group(1), passed)
    })
  }

  /** The inputs a workflow's runner choice reads: its own jobs', and its calls' through the values it passes them. */
  private def runnerInputs(file: String): Set[String] = {
    val passedOn = calls(file).flatMap((_, callee, passed) => passed.filter((key, _) => runnerInputs(callee)(key)).values)
    (runsOn(file).values ++ passedOn).flatMap(InputRef.findAllMatchIn(_).map(_.group(1))).toSet
  }

  /** Every job's label in `file`, and every reusable call's jobs under `<calling job> / <job>`, read with the inputs the
   *  call passes: a called workflow runs where its CALLER's ref and inputs put it, not where a dispatch of it would. */
  private def labels(file: String, at: RunnerLabel.Context): Map[String, String] =
    runsOn(file).flatMap { (job, runner) =>
      // a matrix job whose runs-on reads its phase (country-convergence-leg.yml: a recording's consumer rows below its
      // convergence row, Record scrape fixtures #198) is one label per phase: its producer's and a consumer's
      // -- split only where the phases' labels differ, so a job they agree on keeps its one key
      val perPhase = Seq("convergence", "order-independence")
        .map(phase => phase -> RunnerLabel(runner, at.copy(matrix = Map("phase" -> phase))))
      if (perPhase.map(_._2).distinct.size == 1) Seq(job -> perPhase.head._2)
      else perPhase.map((phase, label) => s"$job [$phase]" -> label)
    } ++
      calls(file).flatMap { (job, callee, passed) =>
        val read   = runnerInputs(callee)
        val inputs = passed.collect { case (key, value) if read(key) => key -> RunnerLabel(value, at) }
        labels(callee, at.copy(inputs = inputs)).map((called, label) => s"$job / $called" -> label)
      }

  /** The legs' runner of a suite: Identity model convergence's, or (since 2026-11-08) Identity takes', the same rule. */
  private def legRunner(at: RunnerLabel.Context, suite: String = "identity-model-convergence.yml"): String = {
    val line = """(?m)^\s+runner: (\$\{\{ inputs\.runner .*}})\s*$""".r
    RunnerLabel(line.findFirstMatchIn(yml(suite)).get.group(1), at)
  }

  private val RunId = "44061032302"

  /** A landing check's labels at `ref` (a job asking for several, joined by `,`). */
  private def landing(ref: String, vars: Map[String, String] = SelfHosted): Map[String, String] =
    labels("precommit-check.yml", ctx(ref, vars).copy(runId = RunId))

  /** The identity shards' job: on GitHub's hosted runners whatever MOVIES_CI_RUNNERS says (the owner, 2026-11-21). */
  private val Identity = "check-identity"

  "a landing check" should "run the rest on ci-runner's movies-check slots, pinned to its run, its plan on the light lane" in {
    // OLDEST FIRST (2026-11-17, runner_scheduler.py PIN_PREFIX): the run's pin lets the scheduler start a runner that
    // can take this run's jobs alone, for the oldest queued one; the plan job, on the light lane, is not pinned
    val check = landing("refs/heads/check/x")
    check.keySet shouldBe Set("plan", "check", Identity)
    check("plan") shouldBe "movies"
    check("check") shouldBe s"movies-check,movies-run-$RunId"
  }

  it should "run the identity shards on GitHub's hosted runners, ci-runner on or off (the owner, 2026-11-21)" in {
    // ci-runner's check CPU (~3 check containers) was the landing's binding constraint: the shards queued p50 9.6 / p90
    // 28 min for ~10 min of work; hosted, they take none of its check slots. A plain label, no expression: nothing a
    // run's variables or its plan's outputs say can put them back on ci-runner
    runsOn("precommit-check.yml")(Identity) shouldBe "ubuntu-26.04"
    for (ref <- Seq("refs/heads/check/x", "refs/heads/check/red-x"); vars <- Seq(SelfHosted, Map.empty[String, String]))
      withClue(s"$ref, $vars: ")(landing(ref, vars)(Identity) shouldBe "ubuntu-26.04")
  }

  it should "run a gate-red fix's on the red-fix check lane, served first (runner_scheduler.py CHECK_LANES, 2026-11-17)" in {
    val red = landing("refs/heads/check/red-20261117-110122-movies-x-2ed6cd")
    red("check") shouldBe s"movies-check-red-fix,movies-run-$RunId"
    all(landing("refs/heads/check/red-x", vars = Map.empty).values) should startWith("ubuntu-")
  }

  it should "ask for no pin the scheduler cannot serve: the pin prefix is the scheduler's" in {
    RepoFile.read("scripts/ci/runner_scheduler.py") should include("""PIN_PREFIX = "movies-run-"""")
  }

  "a measurement" should "run its convergence legs on movies-measure and the rest on ci-runner" in {
    val measure = ctx("refs/heads/measure/agent/abc-123")
    legRunner(measure) shouldBe "movies-measure"
    legRunner(measure, "identity-takes.yml") shouldBe "movies-measure"
    all(labels("identity-model-convergence.yml", measure).values) should (be("movies") or be("movies-measure"))
    all(labels("identity-takes.yml", measure).values) should (be("movies") or be("movies-measure"))
    all(labels("convergence-bisect.yml", ctx(RunnerLabel.Main)).values) should (be("movies") or be("movies-measure"))
    all(labels("report-timings.yml", measure).values) shouldBe "movies"
  }

  "a landing check's takes rows" should "queue in the measure lane their plan job names, never hosted (2026-11-16)" in {
    // scripts/ci/measure-lane.sh writes the lane step's label: movies-red-fix or movies-landing where ci-runner's
    // scheduler serves the lanes (runner_scheduler.py MEASURE_LANES), movies-measure otherwise; main never reads it
    val drift = ctx("refs/heads/measure/agent/abc-123-drift-x")
    for (lane <- Seq("movies-red-fix", "movies-landing"))
      legRunner(drift.copy(steps = Map("lane.label" -> lane)), "identity-takes.yml") shouldBe lane
    legRunner(ctx(RunnerLabel.Main).copy(steps = Map("lane.label" -> "movies-landing")), "identity-takes.yml") shouldBe "ubuntu-22.04"
  }

  "a measurement" should "stay on ci-runner whatever MOVIES_MEASURE_RUNNERS says (no way back to hosted, 2026-11-06)" in {
    // the owner: "measurements always on ci" -- the 2026-11-02 escape (MOVIES_MEASURE_RUNNERS=hosted) is gone, so a
    // measurement never queues GitHub's 20 that main's suite runs on
    val both    = SelfHosted + ("MOVIES_MEASURE_RUNNERS" -> "hosted")
    legRunner(ctx("refs/heads/measure/agent/abc-123", both)) shouldBe "movies-measure"
    all(labels("convergence-bisect.yml", ctx(RunnerLabel.Main, both)).values) should (be("movies") or be("movies-measure"))
    Seq("identity-model-convergence.yml", "identity-takes.yml", "convergence-bisect.yml", "country-convergence-leg.yml")
      .foreach(f => withClue(s"$f: ")(yml(f) should not include ("MOVIES_MEASURE_RUNNERS")))
    // a hand-dispatched runner still wins
    legRunner(ctx("refs/heads/measure/y", both).copy(inputs = Map("runner" -> "ubuntu-24.04-arm"))) shouldBe "ubuntu-24.04-arm"
  }

  "Main and convergence on main" should "stay on GitHub's hosted runners even when ci-runner is switched on" in {
    val onMain = ctx(RunnerLabel.Main).copy(workflow = "Main")
    legRunner(onMain) shouldBe "ubuntu-22.04"
    legRunner(onMain, "identity-takes.yml") shouldBe "ubuntu-22.04"
    // every row of a leg on the leg's one runner: no per-row label (2026-11-06's `row-runners`, retired the same day)
    val leg = yml("country-convergence-leg.yml")
    leg should not include ("row-runners")
    leg should not include ("movies-main")
    yml("identity-model-convergence.yml") should not include ("runner_scheduler.py")
    val main = Seq("main.yml", "ci.yml", "identity-model-convergence.yml", "identity-takes.yml", "report-timings.yml")
      .flatMap(f => labels(f, onMain).map((job, label) => s"$f: $job" -> label))
    main should not be empty
    withClue("a job of Main's on a self-hosted label: ")(main.filterNot((_, l) => Hosted(l)) shouldBe empty)
  }

  /** THE GATE's workflows and what they call, the landing check, and Branch CI (it is ci.yml, Main's own suite): they
   *  keep their own rules above. */
  private val GateSide = Set("main.yml", "ci.yml", "identity-model-convergence.yml", "identity-takes.yml", "country-convergence-leg.yml",
    "report-timings.yml", "precommit-check.yml", "branch-ci.yml")
  /** What ci-runner's job container cannot give: macOS, and an Android emulator (no /dev/kvm proven in it). */
  private val NeedsHosted = Set("ios.yml: ui-tests", "android.yml: instrumented", "order-independence.yml: swift",
    "order-independence.yml: ios", "order-independence.yml: android-instrumented",
    // macOS's /bin/bash 3.2, BSD tools and /usr/bin/python3 3.9: the floor the script tools promise
    "tool-self-tests.yml: macos-script-tests",
    // browser timing stress needs steady CPU (the owner, 2026-11-08): on movies-measure's shared host, Nightly stress #29's
    // WebKit phones timed out when a co-tenant starved every worker at once
    "nightly-stress.yml: races", "nightly-stress.yml: webkit-phones", "nightly-stress.yml: report",
    // the copy to GHCR takes skopeo, which github.com's image carries and ci-runner's job image does not
    "mirror-pinned-images.yml: mirror")

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

  /** Record scrape fixtures #198 (run 44423518717): the UK order-independence row, which waits for the corpus its own
   *  convergence row uploads, took a movies-measure runner at 00:10Z while that convergence row waited until 01:41Z, and
   *  idled until its wait failed. The owner (2026-11-19): a consumer lane just below movies-measure. */
  "a recording's consumer rows" should "queue in the consumer lane, below the convergence row whose upload they wait for" in {
    val record = labels("record-scrape-fixtures.yml", ctx(RunnerLabel.Main).copy(steps = Map("lane.label" -> "movies-consumer")))
    val phases = record.filter((job, _) => job.endsWith("]"))
    phases should not be empty
    all(phases.filter((job, _) => job.endsWith("[convergence]")).values) shouldBe "movies-measure"
    all(phases.filter((job, _) => job.endsWith("[order-independence]")).values) shouldBe "movies-consumer"
    // where ci-runner's scheduler serves no consumer lane, measure-lane.sh writes movies-measure: the row still runs
    all(labels("record-scrape-fixtures.yml", ctx(RunnerLabel.Main)).filter((job, _) => job.endsWith("]")).values) shouldBe
      "movies-measure"
  }

  /** THE DRIFT EXPLORER CALLS Identity takes on main (2026-11-15), and a called workflow reads its caller's ref: picked by
   *  the ref alone, its rows took GitHub's hosted runners -- on 2026-11-16 00:35Z 8 of the account's 20, while 14
   *  convergence and 21 takes rows of THE GATE's run 43779901411 queued. No gate verdict, so none of the gate's runners. */
  "the drift explorer's called Identity takes rows" should "run on ci-runner, though called on main" in {
    val explorer = labels("identity-drift-explorer.yml", ctx(RunnerLabel.Main))
    val called   = explorer.filter((job, _) => job.startsWith("drift / "))
    called.keySet should contain allOf ("drift / preflight", "drift / leg / convergence")
    all(called.values) should (be("movies") or be("movies-measure"))
    called("drift / leg / convergence") shouldBe "movies-measure"
    withClue("ci-runner switched off: ")(all(labels("identity-drift-explorer.yml", ctx(RunnerLabel.Main, Map.empty)).values.map(Hosted)) shouldBe true)
  }

  /** THE EXPLORER LANE (the owner, 2026-11-18, "7"): the nightly explorers' rows queue below every optional measurement,
   *  and none starts while a landing check waits for a runner (runner_scheduler.py EXPLORE_LANE) -- on 2026-11-17
   *  21:15-21:40Z the drift explorer's rows refilled the measure CPUs and a landing check could not borrow them. Each
   *  asks for the lane its plan's measure-lane.sh names: movies-explore where ci-runner's scheduler serves it. */
  "the explorers' rows" should "queue in the explorer lane where ci-runner's scheduler serves it, movies-measure otherwise" in {
    val main  = ctx(RunnerLabel.Main)
    val drift = labels("identity-drift-explorer.yml", main.copy(steps = Map("lane.label" -> "movies-explore")))
    drift("drift / leg / convergence") shouldBe "movies-explore"
    yml("identity-takes.yml") should include("INPUT_LANE: ${{ inputs.measurement-lane && 'explore' || inputs.lane }}")
    val walk = labels("identity-family-explorer.yml", main.copy(outputs = Map("plan.lane" -> "movies-explore")))
    walk("walk") shouldBe "movies-explore"
    labels("identity-family-explorer.yml", main)("walk") shouldBe "movies-measure"
    RepoFile.jobs(yml("identity-family-explorer.yml"))("plan") should include("scripts/ci/measure-lane.sh explore")
    RepoFile.read("scripts/ci/runner_scheduler.py") should include("""EXPLORE_LANE = MEASURE_LANES[-1]""")
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
