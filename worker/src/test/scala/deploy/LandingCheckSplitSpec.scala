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

  "the landing check" should "run the page area and the old refs in jobs of their own, each with a cap of its own" in {
    Map("check" -> "rest", "check-oldrefs" -> "oldrefs", "check-page" -> "page").foreach { case (job, share) =>
      jobs(job) should include(s"SHARE: $share")
      jobs(job) should include("""if [ "$SHARE" = rest ]; then export PREPUSH_ELSEWHERE="page oldrefs"; else export PREPUSH_ONLY="$SHARE"; fi""")
      jobs(job) should include("timeout --kill-after=60 80m scripts/hooks/pre-push")
    }
    // the old refs from the trees Main keeps: cold, they pushed the rest past its 80 minutes (run 4083)
    jobs("check-oldrefs") should include("KINOWO_IDENTITY_CACHE: ${{ runner.temp }}/kinowo-identity")
    // beside the rest on the check tier its WebKit rows starved and ran out of the 80 minutes (run 4059)
    jobs("check-page") should include("needs: [ check, check-oldrefs, heavy ]")
  }

  /** 8 of Main's 14 code reds in the five days to 2026-10-30 were in integration-test or e2e rows nothing ran before the
   *  push (ci-failures-2026-10-30 §2.1): the landing check runs the ones the change can move, as Main runs them. */
  "the landing check" should "run every heavy row the change can move, one after another in one job" in {
    val heavy = jobs("heavy")
    jobs("plan") should include("PREPUSH_HEAVY_ROWS=1 scripts/hooks/pre-push --plan")
    heavy should include("ROWS: ${{ needs.plan.outputs.heavy }}")
    heavy should include("PREPUSH_HEAVY_ROWS=1 PREPUSH_ONLY=\"$ROWS\"")
    heavy should not include "matrix"
    heavy should include("scripts/ci/start-mongo-replset.sh")
    heavy should include("runs-on: ${{ github.server_url == 'https://github.com' && 'ubuntu-22.04' || 'ubuntu-24.04' }}")
  }

  "the heavy rows" should "run Main's own commands" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("check_itrest() { heavy_row itrest itAll IT_ROW=rest; }")
    hook should include("check_itidentity() { heavy_row itidentity itAll IT_ROW=identity; }")
    hook should include("check_e2e() { heavy_row e2e e2eRest; }")
    hook should include("check_e2ecorpus() { heavy_row e2ecorpus e2eCorpus; }")
    val ci = RepoFile.read(".github/workflows/ci.yml")
    ci should include("run: sbt itAll")
    ci should include("cmd: e2eRest")
    ci should include("cmd: e2eCorpus")
    RepoFile.commandAlias("itAll") should not be empty
  }

  "the pre-push hook" should "run only the checks its share names, and say which it leaves to the other jobs" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("split_out() {")
    hook should include("left to the landing check's other job(s):")
  }
}
