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
    Map("check" -> "rest", "check-identity" -> "identity", "check-oldrefs" -> "oldrefs", "check-page" -> "page").foreach { case (job, share) =>
      jobs(job) should include(s"SHARE: $share")
      jobs(job) should include("""scripts/ci/precommit-share.sh env "$SHARE" >> "$GITHUB_ENV"""")
      jobs(job) should include("""timeout --kill-after=60 80m scripts/hooks/pre-push "$PREPUSH_MODE" "$PREPUSH_RANGE"""")
    }
    val share = RepoFile.read("scripts/ci/precommit-share.sh")
    share should include("""elsewhere="page oldrefs"; [ "$mode" = --fast-only ] || elsewhere+=" identity"""")
    share should include("PREPUSH_LIST=1")
    jobs("check-oldrefs") should include("KINOWO_IDENTITY_CACHE: ${{ runner.temp }}/kinowo-identity")
    jobs("check-page") should include("needs: [ plan, check, check-identity, check-oldrefs, heavy ]")
    jobs("check-page") should include("needs.check-identity.result != 'failure' && needs.check-identity.result != 'cancelled'")
  }

  /** A job sets up sbt, Node and the tools only when the hook would run a check of its share there (the hook's own
   *  selection, PREPUSH_LIST=1: precommit-share.sh); its hook step always runs, so a selection the cheap step missed
   *  fails there rather than going unchecked. The jobs the plan does not name (old refs, page, heavy rows) are skipped. */
  "every check job" should "set up only for a share with checks, its hook step never skipped" in {
    Seq("check", "check-identity", "check-oldrefs", "check-page").foreach { job =>
      RepoFile.step(jobs(job), "The pre-push hook's checks of the change") should not include "if:"
      jobs(job) should include("if: steps.share.outputs.checks != ''")
    }
    jobs("check-oldrefs") should include("if: ${{ needs.plan.outputs.oldrefs == 'true' }}")
    jobs("check-page") should include("needs.plan.outputs.page == 'true'")
  }

  /** The identity module's tests ran after the sbt check in the rest's job: 292 s then 585 s, a 15.9-minute job (run
   *  41276112019; docs/investigations/2026-11-01-twenty-runners.md), then in one job of their own (~12.5 min). Three
   *  shards beside the rest, created only for a change the plan gives an identity check, each a third of the selected specs (scripts/ci/shard_tests.py). */
  "the identity check" should "run in three shards beside the rest, only when the plan names it" in {
    val identity = jobs("check-identity")
    identity should include("needs: [ plan ]")
    identity should include("if: ${{ needs.plan.outputs.identity == 'true' }}")
    identity should include("shard: [1, 2, 3]")
    identity should include("PREPUSH_IDENTITY_SHARD: ${{ matrix.shard }}/3")
    RepoFile.read("scripts/hooks/pre-push") should include("scripts/ci/shard_tests.py --shard \"$PREPUSH_IDENTITY_SHARD\"")
  }

  /** 8 of Main's 14 code reds in the five days to 2026-10-30 were in integration-test or e2e rows nothing ran before the
   *  push (ci-failures-2026-10-30 §2.1): the landing check runs the ones the change can move, as Main runs them -- in
   *  two jobs, `itrest` alone and the others together (one job ran them one after another, 9-10 min). */
  "the landing check" should "run every heavy row the change can move, in two jobs" in {
    val heavy = jobs("heavy")
    jobs("plan") should include("scripts/ci/precommit-share.sh checks plan")
    RepoFile.read("scripts/ci/precommit-share.sh") should include("""plan) vars+=("PREPUSH_HEAVY_ROWS=1")""")
    heavy should include("rows: ${{ fromJSON(needs.plan.outputs.heavy-groups) }}")
    heavy should include("ROWS: ${{ matrix.rows }}")
    heavy should include("PREPUSH_HEAVY_ROWS=1 PREPUSH_ONLY=\"$ROWS\"")
    heavy should include("scripts/ci/start-mongo-replset.sh")
    heavy should include("runs-on: ${{ vars.MOVIES_CI_RUNNERS == 'self-hosted' && 'movies-check' || 'ubuntu-22.04' }}")
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
