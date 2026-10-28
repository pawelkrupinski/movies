package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A hung CI row is cut at a bound set from what the rows really take, not at the runner's default 360 minutes.
 *
 * Measured over every Main and branch-CI run to 2026-10-26 (the Forgejo DB's ~6.5 days): the longest GREEN ci.yml row
 * took 31 min (`test (worker-identity)`, 1857 s; p99 of every row under 21 min), while a hung `integration-test (rest)`
 * held its runner 59 min before failing. And `identity-run`'s longest green job ran 162 min (run 965, a five-country
 * command whose own step took 9515 s), so its bound stays above that rather than at the ~45 min its mean (10 min)
 * suggested: a bound below a legitimate run is a red run nobody caused.
 *
 * The rule ablation's `ablate` row (identity-rule-ablation.yml): its longest green ablation step took 1416 s (run 560)
 * and its longest green job 1699 s (run 525), against a 30-minute job bound -- so Identity rule ablation #2376, under a
 * loaded ci-runner, was cut at 1613 s into the step. And a JOB timeout cancels the `if: always()` upload of the row's
 * state too, so the round it resolved was lost with its log. So the step carries its own bound (bounded-step.sh), above
 * its longest green run, and the job's leaves the layout before it and the reap and upload after it room to run.
 */
class CiRowTimeoutSpec extends AnyFlatSpec with Matchers {

  private def timeoutOf(block: String): Option[Int] =
    RepoFile.withoutComments(block).linesIterator.map(_.trim).collectFirst {
      case s"timeout-minutes: $n" => n.takeWhile(_.isDigit).toInt
    }

  private val LongestGreenRowMinutes         = 31
  private val LongestGreenIdentityRunMinutes = 162
  private val LongestGreenAblationStepMinutes = 24
  /** The ablate row's steps before and after its ablation: the layout took up to 472 s (run 335), setup, Mongo, the
   *  reap and the upload a few minutes more. */
  private val AblationRowOverheadMinutes = 10

  "every ci.yml row" should "carry a timeout above its longest green run and well under the 360-minute default" in {
    val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/ci.yml")) - "scope"
    jobs should not be empty
    jobs.foreach { case (job, block) =>
      withClue(s"$job: ") {
        val t = timeoutOf(block).getOrElse(fail(s"$job has no timeout-minutes"))
        t should be > LongestGreenRowMinutes + 5
        t should be <= 45
      }
    }
  }

  "identity-run's run job" should "be cut well before 600 minutes, but never before a legitimate run ends" in {
    val run = RepoFile.jobs(RepoFile.read(".github/workflows/identity-run.yml"))("run")
    val t   = timeoutOf(run).getOrElse(fail("identity-run's run job has no timeout-minutes"))
    t should be >= LongestGreenIdentityRunMinutes * 6 / 5
    t should be <= 240
  }

  private lazy val ablate      = RepoFile.jobs(RepoFile.read(".github/workflows/identity-rule-ablation.yml"))("ablate")
  private def ablateStep(name: String) =
    ForgejoParse.steps(ablate).find(_.get("name").exists(_.startsWith(name))).getOrElse(fail(s"the ablate row has no step `$name`"))

  "the rule ablation's ablate row" should "cut its ablation step above its longest green run, leaving the job room to upload what it resolved" in {
    val step = ablateStep("Ablate").get("timeout-minutes").map(_.trim.takeWhile(_.isDigit).toInt)
      .getOrElse(fail("the ablation step has no timeout-minutes of its own: the JOB's timeout cancels the upload after it"))
    val job  = timeoutOf(ablate.linesIterator.takeWhile(!_.trim.startsWith("steps:")).mkString("\n"))
      .getOrElse(fail("the ablate row has no timeout-minutes"))
    step should be >= LongestGreenAblationStepMinutes * 7 / 5
    job should be >= step + AblationRowOverheadMinutes
    job should be <= 60
  }

  it should "upload each configuration's done marker, and a resumed run restore the round it was cut in" in {
    // IdentityRuleAblation skips a configuration whose done-<cc> is there: a cut row's resolved configurations are kept
    ForgejoParse.steps(ablate).flatMap(_.get("with")).mkString("\n") should include("ablation/round-*/*/*/done-*")
    ablateStep("Carry on").getOrElse("run", "") should include("round-")
  }

  it should "stop the ablation at its own deadline, inside the step's bound, so an unfinished round ends green and resumable" in {
    // Identity rule ablation #2400: the default two recordings need ~60 minutes; the step's 34 cut them, and red
    val step     = ablateStep("Ablate")
    val deadline = step.getOrElse("run", "") match {
      case s"$_--deadline-minutes $n $_" => n.takeWhile(_.isDigit).toInt
      case _                             => fail("the ablation runs with no --deadline-minutes: an unfinished round is cut by the step's bound")
    }
    val bound = step.get("timeout-minutes").map(_.trim.takeWhile(_.isDigit).toInt).getOrElse(fail("no step bound"))
    // the deadline is read at each configuration (up to ~2 minutes apart), then sbt gets 30 s to end and the report is written
    deadline should be <= bound - 4
    deadline should be >= LongestGreenAblationStepMinutes
  }

  "the rule ablation's collect job" should "merge the state each ablate row uploads, where the upload puts it" in {
    // the upload's paths share the workspace, not ablation/ (ablation.out sits beside it): `shards/*/state.json` matched
    // nothing, and every collect since went red with no report
    val collect = RepoFile.jobs(RepoFile.read(".github/workflows/identity-rule-ablation.yml"))("collect")
    ForgejoParse.steps(ablate).flatMap(_.get("with")).mkString("\n") should include("ablation.out")
    ForgejoParse.steps(collect).flatMap(_.get("run")).mkString("\n") should include("--merge shards/*/ablation/state.json")
  }
}
