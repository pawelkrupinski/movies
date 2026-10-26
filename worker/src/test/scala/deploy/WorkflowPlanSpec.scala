package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ForgejoParse.*

/**
 * EVERY WORKFLOW DECISION IS MADE IN ONE PLAN JOB. Forgejo evaluates a job's own keys in three different contexts
 * (ForgejoParse): whole at run creation, but with no run id; only its needs' outputs at a re-parse; whole again in the
 * runner, for `if:`. A value read in the wrong one is empty, and nothing says so -- matrix legs on the wrong runner
 * twice, takes-base, profile and recording inputs silently dropped, a corpus run id baked in as "" (2026-10-23..26).
 *
 * So inputs, refs, run ids, tiers and skip decisions are evaluated in ONE place: the workflow's plan job, a root (it
 * needs nothing, so it is parsed whole and run with everything), and every other job reads them as
 * `needs.<plan>.outputs.<name>`. `github.server_url` (the forge) and `github.event_name` are fixed when the run is
 * created and read the same everywhere but at a re-parse, which the re-parse rules forbid separately.
 *
 * And a step with its own `timeout-minutes` runs bounded (scripts/ci/bounded-step.sh): the runner gives up on a timed-out
 * step without ending it, and its processes race the steps after it.
 */
class WorkflowPlanSpec extends AnyFlatSpec with Matchers {

  private def code(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))
  private lazy val files         = RepoFile.workflows().map(_.getName).sorted

  /** The contexts a re-parse does not have. */
  private val Lost = Set("inputs", "github", "vars")
  private def context(read: String) = read.takeWhile(_ != '.')

  /** Fixed at run creation, the same in every evaluation but a re-parse: the forge, and what started the run. */
  private val RunConstants = Set("github.server_url", "github.event_name")

  private def matrixCallReads(file: String): Seq[String] = for {
    (name, job) <- RepoFile.jobs(code(file)).toSeq.sortBy(_._1)
    if localCall(job).nonEmpty && hasMatrix(job)
    (_, read)   <- readsIn(job, Set("with"))
    if Lost(context(read))
  } yield s"$file: $name's `with:` reads $read"

  private def reparseReads(file: String): Seq[String] = RepoFile.jobs(code(file)).toSeq.sortBy(_._1).flatMap {
    case (name, job) if reparsed(job) =>
      val own = readsIn(job, ReparsedKeys).collect { case (k, r) if Lost(context(r)) => s"$file: $name's `$k` reads $r" }
      val created = localCall(job).toSeq.flatMap { called =>
        RepoFile.jobs(code(called)).toSeq.sortBy(_._1).flatMap { (inner, innerJob) =>
          readsIn(innerJob, ReparsedKeys).collect {
            case (k, r) if context(r) != "inputs" && Lost(context(r)) => s"$file: $name / $called $inner's `$k` reads $r"
          }
        }
      }
      own ++ created
    case _ => Nil
  }

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

  "a matrix of reusable calls" should "take its `with:` from its rows and its needs' outputs alone" in {
    withClue("a matrix call's `with:` is baked before the run has an id, or re-parsed without inputs and github: ") {
      files.flatMap(matrixCallReads) shouldBe empty
    }
  }

  "a job Forgejo re-parses" should "read no inputs, github or vars where the re-parse evaluates, nor the jobs it creates" in {
    withClue("read empty at the re-parse (ForgejoParse): ")(files.flatMap(reparseReads) shouldBe empty)
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
    steps        = ForgejoParse.steps(job)
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
  }

  "the evaluation model" should "read the workflows' text the way the rules need" in {
    val job =
      """    leg:
        |        needs: [ preflight, other ]
        |        runs-on: ${{ github.server_url == 'https://github.com' && 'x' || needs.preflight.outputs.runner }} # a note
        |        if: ${{ !cancelled() && inputs.tier == '' }}
        |        strategy:
        |            matrix:
        |                code: [us, de]
        |        uses: ./.github/workflows/country-convergence-leg.yml
        |        with:
        |            lane: ${{ inputs.tier || 'measure' }}
        |        steps:
        |            - name: one
        |              timeout-minutes: 5
        |              run: |
        |                  . scripts/ci/bounded-step.sh
        |                  echo hi # not a comment of the key
        |            - uses: actions/upload-artifact@v6
        |              if: always()""".stripMargin
    needs(job) shouldBe Set("preflight", "other")
    reparsed(job) shouldBe true
    hasMatrix(job) shouldBe true
    localCall(job) shouldBe Some("country-convergence-leg.yml")
    readsIn(job, Set("runs-on")).map(_._2) shouldBe Seq("github.server_url", "needs.preflight")
    readsIn(job, Set("if")).map(_._2) shouldBe Seq("inputs.tier")
    readsIn(job, Set("with")).map(_._2) shouldBe Seq("inputs.tier")
    val Seq(one, upload) = ForgejoParse.steps(job)
    isBounded(one) shouldBe true
    runsAfterFailure(upload) shouldBe true
    stripComment("a: ${{ 'x # y' }} # z") shouldBe "a: ${{ 'x # y' }}"
    // A static matrix that reads no needs is complete at run creation: not re-parsed.
    reparsed("    j:\n        needs: [ a ]\n        strategy:\n            matrix:\n                c: [x]\n        runs-on: x") shouldBe false
  }
}
