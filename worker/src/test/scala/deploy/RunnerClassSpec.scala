package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RunnerLabel.{Branch, Context, Forgejo, GitHub, Main}

/**
 * EVERY WORKFLOW RUNS IN ONE TIER, AND THE LABEL IS THE TIER. Off github.com every job runs on ci-runner, whose
 * runners sit in four cgroup slices with CPU weights, not caps (infra nix/hosts/ci-runner/forgejo.nix `ciTiers`,
 * 2026-10-25): deploy 1000, gate 600, check 300, measure 100. A job's share of the host is its tier's, so a workflow
 * asking for the wrong label takes the wrong share -- before the tiers every container weighed the same, and on a busy
 * host Main's convergence rows went from 5.3 to 13.6 min and the recording's Germany leg from 43 to 65-125 min behind
 * measure-branch convergence.
 *
 *  - deploy (`main-lane`): Main, and nix-stage-closures on main. Forgejo has no job priority -- a runner takes the oldest
 *    waiting job its labels match -- so a label only production asks for is the only precedence there is: Main run
 *    #484 (a red-main fix) sat 40+ min behind ~39 identity jobs on 2026-10-22.
 *  - gate (`gate-lane`, `forgejo-recording`): identity model convergence ON MAIN, which gates every push
 *    (scripts/hooks/pre-push's red-build gate), and the nightly recording's legs.
 *  - check (`ubuntu-*`): branch CI, the nightly and housekeeping runs, bots, and an identity landing check's
 *    identity-run and convergence runs (`-f tier=ubuntu-24.04`): a landing check blocks work.
 *  - measure (`measure`): investigation -- measure-branch convergence, identity runs, ablations, bisects. It borrows idle
 *    CPU and never holds a deploy or gate slot.
 *
 * A lane holds only while it stays exclusive: one experiment naming `main-lane` or `gate-lane` makes it a second shared
 * queue. And github.com has none of these runners but `ubuntu-*`, so every other label is asked for only off it --
 * a job asking there would wait forever.
 *
 * The labels are EVALUATED (RunnerLabel), per forge and per ref, through every reusable call's `lane:` / `runner:`,
 * so an expression that reads right and picks wrong -- identity model convergence's legs asked for `main-lane` only
 * when `!inputs.runner`, never true while the input defaulted to ubuntu-24.04 -- fails here.
 */
class RunnerClassSpec extends AnyFlatSpec with Matchers {

  private enum Tier(val labels: Set[String]) {
    case Deploy  extends Tier(Set("main-lane"))
    case Gate    extends Tier(Set("gate-lane", "forgejo-recording"))
    case Check   extends Tier(Set("ubuntu-26.04", "ubuntu-24.04", "ubuntu-latest"))
    case Measure extends Tier(Set("measure"))
  }
  import Tier.*

  /** A workflow's tier for its runs on main and on any other branch: the same one but for the two gating workflows,
   *  whose runs on main are production's and whose branch runs (a measurement's measure/ branch, a PR) are not. */
  private final case class Classed(onMain: Tier, elsewhere: Tier)
  private def always(tier: Tier) = Classed(tier, tier)

  private val Classes: Map[String, Classed] = Map(
    "main.yml"                       -> always(Deploy),
    "nix-stage-closures.yml"         -> Classed(onMain = Deploy, elsewhere = Check),
    "identity-model-convergence.yml" -> Classed(onMain = Gate, elsewhere = Measure),
    "record-scrape-fixtures.yml"     -> always(Gate),
    "verify-scrape-recording.yml"    -> always(Gate)
  ) ++ Seq(
    "android.yml", "archive-recording.yml", "branch-ci.yml", "convergence-daily.yml", "country-fixture-artifact.yml",
    "discover-cinemas.yml", "filmweb-diff.yml", "identity-ci-staleness.yml", "identity-findings-report.yml", "ios.yml",
    "nightly-stress.yml", "nix-update.yml", "prune-actions-caches.yml", "retire-venues.yml", "roster-audit.yml",
    "scala-steward.yml", "security-headers.yml"
  ).map(_ -> always(Check)) ++ Seq(
    "convergence-bisect.yml", "convergence-fill.yml", "decoration-discovery.yml", "identity-capture.yml",
    "identity-decorations.yml", "identity-lean-whole.yml", "identity-live-drift.yml", "identity-refit.yml",
    "identity-rule-ablation.yml", "identity-run.yml", "order-independence.yml", "page-test-repeat.yml"
  ).map(_ -> always(Measure))

  /** Reusable workflows: they have no tier of their own and run in their caller's, through its `lane:` / `runner:`. */
  private val Called = Set("ci.yml", "report-timings.yml", "country-convergence-leg.yml")

  /** Labels only ci-runner has: github.com would leave a job asking for one queued forever. */
  private val ForgejoOnly = Set(Deploy, Gate, Measure).flatMap(_.labels)

  private def code(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))

  private val RunsOn    = """runs-on:\s*(\$\{\{[^}]*}}|\S+)""".r.unanchored
  private val LocalCall = """uses:\s*\./\.github/workflows/(\S+)""".r.unanchored
  private val Passed    = """^\s+(lane|runner):\s*(.+?)\s*$""".r

  private val Needs     = """^\s*needs:\s*\[?([^\]]*)]?\s*$""".r
  private val NeedsRead = """needs\.([\w-]+)\.outputs\.([\w-]+)""".r
  private val Output    = """^\s+([\w-]+):\s*(.+?)\s*$""".r

  /** Every job `file` runs in `ctx` with the label it asks for, reusable calls followed into the called workflow
   *  with the caller's `lane:` / `runner:` evaluated as its inputs. */
  private def labels(file: String, ctx: Context): Seq[(String, String)] = {
    val jobs = RepoFile.jobs(code(file))
    jobs.toSeq.sortBy(_._1).flatMap { case (name, job) =>
      job match {
        case LocalCall(called) =>
          // A call passing nothing (a check-tier workflow's timings) leaves the called jobs on their own label.
          val passed = if (job.linesIterator.exists(_.trim == "with:")) RepoFile.block(job, "with") else ""
          val at     = callContext(job, passed, jobs, ctx)
          val inputs = passed.linesIterator.collect { case Passed(key, value) => key -> RunnerLabel(value, at) }.toMap
          labels(called, at.copy(inputs = inputs)).map { case (inner, label) => s"$name / $inner" -> label }
        case RunsOn(runner) => Seq(name -> RunnerLabel(runner, ctx))
        case _              => fail(s"$file: job `$name` has neither `runs-on:` nor a local `uses:`")
      }
    }
  }

  /** What Forgejo evaluates a reusable call's `with:` in. A MATRIX call that needs another job is expanded only once
   *  that job is done, and that re-parse (services/actions/job_emitter.go, `tryHandleIncompleteMatrix`) hands it the
   *  needed jobs' outputs and nothing else: no `inputs`, no `github` -- run 2783 on 2026-10-26 sent main's convergence
   *  legs to `measure` through an `inputs.tier` that read empty there. Such a call takes its tier from a needed job's
   *  output, evaluated in that job, where `inputs` and `github` are whole. */
  private def callContext(job: String, passed: String, jobs: Map[String, String], ctx: Context): Context = {
    val needs = job.linesIterator.collectFirst { case Needs(list) => list.split(",").map(_.trim).filter(_.nonEmpty).toSet }
    val labelReads = passed.linesIterator.collect { case Passed(_, value) => value }.mkString("\n")
    val outputs = NeedsRead.findAllMatchIn(labelReads).map { m =>
      val (needed, key) = (m.group(1), m.group(2))
      withClue(s"`with:` reads $needed's `$key` but does not need $needed: ")(needs.toSeq.flatten should contain (needed))
      val declared = RepoFile.block(jobs(needed), "outputs").linesIterator.collect { case Output(k, v) => k -> v }.toMap
      s"$needed.$key" -> RunnerLabel(declared.getOrElse(key, fail(s"$needed declares no output `$key`")), ctx)
    }.toMap
    val reparsed = ctx.server != GitHub && needs.nonEmpty && job.linesIterator.exists(_.trim == "matrix:")
    if (reparsed) Context(server = "", ref = "", outputs = outputs) else ctx.copy(outputs = outputs)
  }

  /** Forgejo hands a dispatched run's `github.ref` to `runs-on` as the short `main` (run 1697 on 2026-10-25 asked main's
   *  convergence onto `measure` behind six measurements), a pushed one's as `refs/heads/main`: both are main. */
  private val ShortMain = "main"
  private val Refs = Seq(Main, ShortMain, Branch)

  /** The inputs main's own dispatch of `file` passes: Main's kick-convergence hands the convergence suite its tier
   *  (`-f tier=…`), since Forgejo evaluates a dispatched run's `runs-on` before `github.ref` is set. Read off the
   *  script, so a kick that stops passing it turns this spec red rather than parking main's run on `measure`. */
  private def mainDispatch(file: String): Map[String, String] =
    if (file != "identity-model-convergence.yml") Map.empty
    else MainConvergenceDispatches.head._2.map("tier" -> _).toMap

  /** Every dispatch of main's convergence, with the tier it passes: Main's kick, and each workflow that runs it on main
   *  (convergence-daily.yml). Main's runs share one lane, so a dispatch without the tier both waits on `measure` and
   *  replaces the push's gate-lane run (run 2829, 2026-10-26). */
  private lazy val MainConvergenceDispatches: Seq[(String, Option[String])] = {
    val Tier  = """-f tier=(\S+)""".r.unanchored
    def tier(line: String) = line match { case Tier(t) => Some(t); case _ => None }
    val kick  = RepoFile.read(".github/scripts/kick-convergence.sh").linesIterator.filter(_.trim.startsWith("dispatch_workflow "))
    val calls = for {
      f    <- RepoFile.workflows().toSeq.sortBy(_.getName)
      line <- code(f.getName).linesIterator
      if line.contains("workflow run identity-model-convergence.yml --ref main")
    } yield f.getName -> tier(line)
    kick.map(l => "kick-convergence.sh" -> tier(l)).toSeq ++ calls
  }

  "every workflow" should "be classed in exactly one tier, or be a reusable workflow its caller's tier runs" in {
    val files = RepoFile.workflows().map(_.getName).toSet
    withClue("workflows with no tier (class each one in Classes, or in Called if it is reusable): ") {
      (files -- Classes.keySet -- Called) shouldBe empty
    }
    withClue("classed workflows that no longer exist: ")((Classes.keySet ++ Called -- files) shouldBe empty)
    withClue("a workflow both classed and reusable: ")((Classes.keySet intersect Called) shouldBe empty)
  }

  "every job off github.com" should "ask for its workflow's tier, on main and on any other branch" in {
    val off = for {
      (file, classed) <- Classes.toSeq.sortBy(_._1)
      ref             <- Refs
      tier             = if (ref == Main || ref == ShortMain) classed.onMain else classed.elsewhere
      jobs             = labels(file, Context(Forgejo, ref, if (ref == Branch) Map.empty else mainDispatch(file)))
      _                = withClue(s"$file has no job to class: ")(jobs should not be empty)
      (job, label)    <- jobs
      if !tier.labels(label)
    } yield s"$file on $ref: $job asks for `$label`, not $tier (${tier.labels.toSeq.sorted.mkString(", ")})"
    withClue("jobs that would take another tier's share of ci-runner, or hold its slots: ")(off shouldBe empty)
  }

  "every dispatch of main's convergence" should "pass main's tier" in {
    MainConvergenceDispatches.map(_._1) should contain ("convergence-daily.yml")
    withClue("dispatches of main's convergence without the gate tier: ") {
      MainConvergenceDispatches.filterNot(_._2.contains("gate-lane")) shouldBe empty
    }
  }

  /** The tier a landing check's CI runs take, read off the scripts that dispatch them: identity-landing-check's
   *  `landing_tier`, passed as `--tier` to identity-ci-run and identity-measure, which hand it to identity-run.yml and
   *  identity-model-convergence.yml as their `tier` input. None when any link of that chain is missing -- a landing
   *  check then lands on `measure`, behind bisects and ablations (hours of queue on 2026-10-26). */
  private lazy val LandingTier: Option[String] = {
    def shell(path: String) = RepoFile.read(path).linesIterator.filterNot(_.trim.startsWith("#")).toSeq
    val check     = shell("scripts/identity-landing-check")
    val defined   = check.collectFirst { case s"landing_tier=$label" if label.nonEmpty => label.trim }
    val calls     = Seq("/identity-ci-run}\"", "flags=(--head").map(call => check.filter(_.contains(call)))
    val passed    = calls.forall(lines => lines.nonEmpty && lines.forall(_.contains("--tier \"$landing_tier\"")))
    val forwarded = Seq("scripts/identity-ci-run", "scripts/identity-measure").forall { script =>
      val lines = shell(script)
      lines.exists(_.trim.startsWith("--tier)")) && lines.exists(_.contains("-f tier=\"$tier\""))
    }
    defined.filter(_ => passed && forwarded)
  }

  "a landing check's runs" should "take the check tier, every job, and a plain measurement's the measure tier" in {
    val tier = withClue("identity-landing-check passes no tier through identity-ci-run and identity-measure: ") {
      LandingTier.getOrElse(fail("no landing tier"))
    }
    val off = for {
      file              <- Seq("identity-model-convergence.yml", "identity-run.yml")
      (expected, input) <- Seq(Check -> Map("tier" -> tier), Measure -> Map.empty[String, String])
      jobs               = labels(file, Context(Forgejo, Branch, input))
      _                  = withClue(s"$file has no job to class: ")(jobs should not be empty)
      (job, label)      <- jobs
      if !expected.labels(label)
    } yield s"$file ${if (input.isEmpty) "measuring" else "landing"}: $job asks for `$label`, not $expected"
    withClue("a landing check blocks work, so its jobs take the check tier; a measurement's take measure: ")(off shouldBe empty)
  }

  "every job on github.com" should "ask for a runner github.com has" in {
    val stranded = for {
      file         <- Classes.keySet.toSeq.sorted
      ref          <- Refs
      (job, label) <- labels(file, Context(GitHub, ref))
      if ForgejoOnly(label)
    } yield s"$file on $ref: $job asks for `$label`"
    withClue("github.com has no such runner; the job would wait forever: ")(stranded shouldBe empty)
  }

  "main-lane and gate-lane" should "be named only by the workflows whose tier they are" in {
    def namers(label: String) = (Classes.keySet ++ Called).filter(f => code(f).contains(s"'$label'"))
    val owners = Map(
      "main-lane" -> Classes.collect { case (f, c) if c.onMain == Deploy || c.elsewhere == Deploy => f }.toSet,
      "gate-lane" -> Classes.collect { case (f, c) if c.onMain == Gate || c.elsewhere == Gate => f }.toSet
    )
    for ((label, own) <- owners)
      withClue(s"$label is its tier's; an experiment there turns it back into a shared queue: ") {
        (namers(label) -- own) shouldBe empty
      }
  }

  "the label evaluation" should "read the expressions the workflows write" in {
    val pick = "${{ github.server_url == 'https://github.com' && 'ubuntu-24.04' || github.ref == 'refs/heads/main' " +
      "&& 'gate-lane' || 'measure' }}"
    RunnerLabel(pick, Context(GitHub, Main)) shouldBe "ubuntu-24.04"
    RunnerLabel(pick, Context(Forgejo, Main)) shouldBe "gate-lane"
    RunnerLabel(pick, Context(Forgejo, Branch)) shouldBe "measure"
    // The input decides first when a dispatch names one, and an empty one falls through.
    RunnerLabel(s"$${{ inputs.runner || 'x' }}", Context(Forgejo, Main, Map("runner" -> "arm"))) shouldBe "arm"
    RunnerLabel(s"$${{ inputs.runner || 'x' }}", Context(Forgejo, Main, Map("runner" -> ""))) shouldBe "x"
    RunnerLabel(s"$${{ !inputs.runner && 'a' || 'b' }}", Context(Forgejo, Main)) shouldBe "a"
    RunnerLabel(s"$${{ needs.preflight.outputs.runner }}", Context("", "", outputs = Map("preflight.runner" -> "gate-lane"))) shouldBe
      "gate-lane"
    RunnerLabel("ubuntu-26.04", Context(GitHub, Main)) shouldBe "ubuntu-26.04"
    an[AssertionError] should be thrownBy RunnerLabel(s"$${{ matrix.os }}", Context(GitHub, Main))
  }
}
