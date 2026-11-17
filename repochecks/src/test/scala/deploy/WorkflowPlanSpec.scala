package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import WorkflowText.*

/**
 * EVERY WORKFLOW DECISION IS MADE IN ONE PLAN JOB: inputs, refs, run ids and skip decisions are evaluated in ONE place,
 * the workflow's plan job (a root: it needs nothing), and every other job reads them as `needs.<plan>.outputs.<name>`.
 * One place to read what a run decided, and one to change it. `github.event_name` is fixed when the run is created
 * and may be read anywhere.
 *
 * And a step with its own `timeout-minutes` runs bounded (scripts/ci/bounded-step.sh): the runner gives up on a timed-out
 * step without ending it, and its processes race the steps after it.
 */
class WorkflowPlanSpec extends AnyFlatSpec with Matchers {

  private def code(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))
  private lazy val files         = RepoFile.workflows().map(_.getName).sorted

  private def context(read: String) = read.takeWhile(_ != '.')

  /** Fixed at run creation: what started the run. */
  private val RunConstants = Set("github.event_name")

  /** The job-level keys whose values are decisions about the job: whether it runs, where, how long, and with what. */
  private val DecisionKeys =
    Set("if", "runs-on", "with", "strategy", "timeout-minutes", "continue-on-error", "concurrency", "environment")
  private val Expression = """\$\{\{(.*?)}}""".r
  private val Literal    = """'(?:[^']|'')*'""".r
  private val Read       = """(?<![\w.-])(inputs|github|vars|needs|matrix)\.([\w-]+)""".r

  /** The contexts `value` reads, as `<context>.<name>`. An `if:` is an expression whole; any other value only inside
   *  `${{ }}`. String literals are not reads. */
  private def reads(key: String, value: String): Seq[String] = {
    val expressions = if (key == "if") Seq(value) else Expression.findAllMatchIn(value).map(_.group(1)).toSeq
    expressions.flatMap(e => Read.findAllMatchIn(Literal.replaceAllIn(e, "''")).map(m => s"${m.group(1)}.${m.group(2)}"))
  }
  private def readsIn(job: String, among: Set[String]): Seq[(String, String)] =
    keys(job).toSeq.filter((k, _) => among(k)).flatMap((k, v) => reads(k, v).map(k -> _))

  /** Whether a workflow is a reusable one (`on: workflow_call`): its `inputs` are its caller's decisions, handed in. */
  private def isCalled(yml: String): Boolean = RepoFile.withoutComments(yml).linesIterator.exists(_.trim == "workflow_call:")

  /** Decisions made outside the plan: a needing job's decision keys reading inputs or the run's github context, and
   *  more than one root doing so. */
  private def unplanned(file: String): Seq[String] = {
    val jobs = RepoFile.jobs(code(file)).toSeq.sortBy(_._1)
    def decisions(job: String) =
      readsIn(job, DecisionKeys).filter((_, r) => (context(r) == "inputs" || context(r) == "github") && !RunConstants(r))
    val needing = for {
      (name, job) <- jobs if needs(job).nonEmpty
      (k, read)   <- decisions(job)
    } yield s"$file: $name's `$k` reads $read -- evaluate it in the plan job and read needs.<plan>.outputs"
    val planners = jobs.collect { case (name, job) if needs(job).isEmpty && decisions(job).nonEmpty => name }
    needing ++ Option.when(planners.size > 1)(s"$file: ${planners.mkString(", ")} each evaluate decisions; one plan job does")
  }

  "every workflow" should "make its decisions in one plan job" in {
    withClue("decisions outside the plan job: ")(files.filterNot(f => isCalled(code(f))).flatMap(unplanned) shouldBe empty)
  }

  private val Bounded = ". scripts/ci/bounded-step.sh"
  private val Reap    = "scripts/ci/bounded-step.sh --reap"
  private def runsAfterFailure(step: Map[String, String]) =
    step.get("if").exists(c => Seq("always()", "failure()", "cancelled()").exists(c.contains))
  private def isBounded(step: Map[String, String]) =
    step.get("run").exists(_.linesIterator.map(_.trim).filterNot(r => r.isEmpty || r == "|" || r == ">").nextOption().contains(Bounded))
  private def isReap(step: Map[String, String]) = step.get("run").exists(_.contains(Reap)) && runsAfterFailure(step)

  private def unbounded(file: String): Seq[String] = for {
    (name, job) <- RepoFile.jobs(code(file)).toSeq.sortBy(_._1)
    steps        = WorkflowText.steps(job)
    (step, at)  <- steps.zipWithIndex
    if step.contains("timeout-minutes")
    label        = s"$file: $name step ${step.getOrElse("name", s"#$at")}"
    problem     <- {
      val later = steps.drop(at + 1)
      val wrapped =
        if (step.contains("uses")) Some("has a timeout but is an action, which nothing can end: give the job the timeout")
        else if (!isBounded(step)) Some(s"has a timeout but its `run:` does not start with `$Bounded`")
        else if (step.get("shell").exists(s => !s.trim.startsWith("bash"))) Some("is bounded but not run by bash")
        else None
      val chained = Option.when(step.get("continue-on-error").exists(_.trim == "true") && later.nonEmpty &&
                                  !(isReap(later.head) || isBounded(later.head)))(
        "goes on after a timeout (continue-on-error), so the very next step must be bounded or the reap")
      val firstAfterFailure = later.find(runsAfterFailure)
      val hasPost           = steps.take(at).exists(_.contains("uses"))
      val reaped = firstAfterFailure match {
        case Some(s) if !(isReap(s) || isBounded(s)) =>
          Some(s"is followed by `${s.getOrElse("name", s.getOrElse("uses", "?"))}`, which runs after a failure, before `$Reap`")
        case None if hasPost => Some(s"is followed by an earlier action's post step with no `$Reap` (if: always()) before it")
        case _               => None
      }
      wrapped.toSeq ++ chained ++ reaped
    }
  } yield s"$label $problem"

  "a step with its own timeout" should "run bounded, and nothing after it run before what it left is ended" in {
    withClue("the runner gives up on a timed-out step without ending it (scripts/ci/bounded-step.sh): ") {
      files.flatMap(unbounded) shouldBe empty
    }
    // The reap step RUNS it (sourcing does not need the bit): committed without it, every reap step failed with
    // "Permission denied" and turned all of main's convergence legs red (run 3134, 2026-10-27).
    withClue("scripts/ci/bounded-step.sh is run by the reap steps, so it must be executable: ") {
      new java.io.File("scripts/ci/bounded-step.sh").canExecute shouldBe true
    }
  }

  "the evaluation model" should "read the workflows' text the way the rules need" in {
    val job =
      """    leg:
        |        needs: [ preflight, other ]
        |        runs-on: ${{ needs.preflight.outputs.runner }} # a note
        |        if: ${{ !cancelled() && inputs.mode == '' }}
        |        uses: ./.github/workflows/country-convergence-leg.yml
        |        with:
        |            lane: ${{ inputs.mode || 'x' }}
        |        steps:
        |            - name: one
        |              timeout-minutes: 5
        |              run: |
        |                  . scripts/ci/bounded-step.sh
        |                  echo hi # not a comment of the key
        |            - uses: actions/upload-artifact@v6
        |              if: always()""".stripMargin
    needs(job) shouldBe Set("preflight", "other")
    readsIn(job, Set("runs-on")).map(_._2) shouldBe Seq("needs.preflight")
    readsIn(job, Set("if")).map(_._2) shouldBe Seq("inputs.mode")
    readsIn(job, Set("with")).map(_._2) shouldBe Seq("inputs.mode")
    val Seq(one, upload) = WorkflowText.steps(job)
    isBounded(one) shouldBe true
    runsAfterFailure(upload) shouldBe true
    stripComment("a: ${{ 'x # y' }} # z") shouldBe "a: ${{ 'x # y' }}"
  }
}
