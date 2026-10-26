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
 */
class CiRowTimeoutSpec extends AnyFlatSpec with Matchers {

  private def timeoutOf(block: String): Option[Int] =
    RepoFile.withoutComments(block).linesIterator.map(_.trim).collectFirst {
      case s"timeout-minutes: $n" => n.takeWhile(_.isDigit).toInt
    }

  private val LongestGreenRowMinutes         = 31
  private val LongestGreenIdentityRunMinutes = 162

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
}
