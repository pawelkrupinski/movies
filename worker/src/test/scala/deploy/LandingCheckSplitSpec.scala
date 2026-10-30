package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE SPLIT CHECK (scripts/hooks/pre-push, PREPUSH_ONLY / PREPUSH_ELSEWHERE): the landing check's jobs share the hook's
 * selected checks and run at once. In one job they ran one after another under one 80-minute cap, and landing check run
 * 4025 (a 124-path batch) ran out of it with three checks still to go after the page area's 37 minutes.
 */
class LandingCheckSplitSpec extends AnyFlatSpec with Matchers {
  private lazy val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/precommit-check.yml"))

  "the landing check" should "run the page area in a job of its own, with a cap of its own, after every other check" in {
    jobs("check") should include("SHARE: rest")
    jobs("check-page") should include("SHARE: page")
    Seq("check", "check-page").foreach { job =>
      jobs(job) should include("export PREPUSH_ONLY=page")
      jobs(job) should include("export PREPUSH_ELSEWHERE=page")
      jobs(job) should include("timeout --kill-after=60 80m scripts/hooks/pre-push")
    }
    // beside the rest on the check tier its WebKit rows starved and ran out of the 80 minutes (run 4059)
    jobs("check-page") should include("needs: [ check ]")
  }

  "the pre-push hook" should "run only the checks its share names, and say which it leaves to the other jobs" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("split_out() {")
    hook should include("left to the landing check's other job(s):")
  }
}
