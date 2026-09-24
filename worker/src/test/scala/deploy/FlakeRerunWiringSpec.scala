package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every CI test job that can fail on a test reruns its failed tests and says DETERMINISTIC or
 * FLAKY, and main.yml ledgers the flaky ones — without the rerun ever turning a job green.
 *
 * Eight Main and four Android runs in the four weeks to 2026-09-24 went green on a rerun of the
 * SAME commit: flakes, each one forgotten the moment the rerun passed. The reruns now happen
 * inside the failing job (scripts/ci/rerun-failed-sbt.sh for sbt, `--last-failed`
 * three times for Playwright), and what they find is recorded. This pins the wiring: a new sbt test job, or a
 * rerun step that drifted into running on success or into deciding the job's outcome, fails here.
 */
class FlakeRerunWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val ciYml   = RepoFile.read(".github/workflows/ci.yml")
  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")
  private lazy val pageAction = RepoFile.read(".github/actions/run-page-test/action.yml")

  /** A step that runs an sbt TEST command: `sbt testUnitNoE2e`, `sbt itAll`, `sbt ${{ matrix.cmd }}`. */
  private val SbtTestRun = """(?m)run: sbt (test\w*|itAll|\$\{\{ matrix\.cmd \}\})\s*$""".r.unanchored

  private lazy val sbtTestJobs = RepoFile.jobs(ciYml).filter { case (_, body) => SbtTestRun.matches(body) }

  "ci.yml" should "have sbt test jobs, or this spec checks nothing" in {
    sbtTestJobs.keySet should contain allOf ("test", "integration-test", "e2e")
  }

  it should "rerun every sbt test job's failed tests, only once the suite has failed" in {
    sbtTestJobs.foreach { case (name, body) =>
      withClue(s"$name: ") {
        body should include("scripts/ci/rerun-failed-sbt.sh")
        val rerun = body.linesIterator.dropWhile(!_.contains("Rerun the failed")).take(3).mkString("\n")
        rerun should include("if: failure() && steps.")
        rerun should include(".outcome == 'failure'")
      }
    }
  }

  it should "never let a rerun decide the job — no continue-on-error on the suite it reruns" in {
    sbtTestJobs.foreach { case (name, body) =>
      withClue(s"$name: ")(body should not include "continue-on-error")
    }
  }

  it should "upload each job's flaky tests under a flaky-* name for the ledger" in {
    sbtTestJobs.foreach { case (name, body) =>
      withClue(s"$name: ")(body should include("name: flaky-"))
    }
  }

  "run-page-test" should "rerun a failed Playwright record's failed tests three times while the server is up" in {
    pageAction should include("for k in 1 2 3")
    pageAction should include("--last-failed --retries 0")
    pageAction should include("flake_verdict.py\" --by-project verdict")
    pageAction should include("name: flaky-page-")
  }

  it should "rerun the Scala PageTest specs too" in {
    pageAction should include("rerun-failed-sbt.sh target/test-reports/page")
  }

  "main.yml" should "ledger the flaky tests from a failed ci run, on main, in a job of its own" in {
    val ledger = RepoFile.jobs(mainYml).getOrElse("flake-ledger", fail("main.yml has no flake-ledger job"))
    ledger should include("needs: ci")
    ledger should include("needs.ci.result == 'failure'")
    ledger should include("github.ref == 'refs/heads/main'")
    ledger should include("issues: write")
    ledger should include("pattern: flaky-*")
    ledger should include("scripts/ci/flake-ledger.sh")
  }

  "the test jobs" should "not hold issues: write — only the ledger job, which runs no project code, does" in {
    RepoFile.jobs(ciYml).foreach { case (name, body) => withClue(s"$name: ")(body should not include "issues: write") }
  }
}
