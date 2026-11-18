package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE SPLIT CHECK (scripts/hooks/pre-push, PREPUSH_ONLY / PREPUSH_ELSEWHERE): the landing check's jobs share the hook's
 * selected checks and run at once. In one job they ran one after another under one 80-minute cap, and landing check run
 * 4025 (a 124-path batch) ran out of it with three checks still to go after the page area's 37 minutes (the page area
 * has since moved to Main: below).
 */
class LandingCheckSplitSpec extends AnyFlatSpec with Matchers {
  private lazy val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/precommit-check.yml"))

  /** The check's verdict is its check jobs' (the VERDICT_JOBS line, read by agentctl and the hook's remote pass): a job
   *  the line does not name would be judged only if it failed, so every job of the workflow is a check the line names. */
  "the landing check" should "name every one of its jobs among the verdict's" in {
    val line = RepoFile.read(".github/workflows/precommit-check.yml").linesIterator.map(_.trim)
      .collectFirst { case s"VERDICT_JOBS: $globs" => globs }.getOrElse(fail("no VERDICT_JOBS line"))
    val globs = line.split(",").map(_.trim).filter(_.nonEmpty).toSeq
    def matches(glob: String, name: String) =
      name.matches(glob.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*"))
    val names = jobs.map { case (id, job) =>
      job.linesIterator.map(_.trim).collectFirst { case s"name: $n" => n }.getOrElse(id)
    }
    names should not be empty
    names.filterNot(n => globs.exists(matches(_, n))) shouldBe empty
    // an expanded matrix name is one too
    Seq("check (identity 1/2)").filterNot(n => globs.exists(matches(_, n))) shouldBe empty
  }

  "the landing check" should "run identity and the old refs in jobs of their own, each with a cap of its own" in {
    Map("check" -> "rest", "check-identity" -> "identity", "check-oldrefs" -> "oldrefs").foreach { case (job, share) =>
      jobs(job) should include(s"SHARE: $share")
      jobs(job) should include("""scripts/ci/precommit-share.sh env "$SHARE" >> "$GITHUB_ENV"""")
      jobs(job) should include("timeout --kill-after=60 80m ")
      jobs(job) should include("""scripts/hooks/pre-push "$PREPUSH_MODE" "$PREPUSH_RANGE"""")
    }
    val share = RepoFile.read("scripts/ci/precommit-share.sh")
    // old refs are another job's in every mode; identity only for a change's own checks (--checks-only): the
    // fast-only and verify-only sets carry no identity area, and verify-only's identity unit tests ride the rest's sbt
    share should include("""elsewhere="oldrefs"; [ "$mode" != --checks-only ] || elsewhere+=" identity"""")
    share should include("PREPUSH_LIST=1")
    jobs("check-oldrefs") should include("KINOWO_IDENTITY_CACHE: ${{ runner.temp }}/kinowo-identity")
  }

  /** The owner, 2026-11-17 ("B1"): the page area and `e2e (rest)` run only after landing, on Main's own rows -- in a week
   *  of landing checks the page area failed 0 of 14 (317 runner-minutes, ~23 min each) and e2e 0 of 95 (303). Neither
   *  the landing check, its plan, the hook's areas nor its heavy rows may bring them back before the push; Main still
   *  runs both, and THE GATE reads their rows (.github/gating-checks.tsv). */
  "the landing check" should "leave the page tests and e2e (rest) to Main, after landing" in {
    jobs.keySet should not contain "check-page"
    jobs("plan") should not include "outputs.page"
    jobs("check") should include("grep -qxE 'itrest|itidentity'")
    RepoFile.read("scripts/ci/precommit-share.sh") should not include "|page|"
    RepoFile.read("scripts/hooks/local-tests.txt").linesIterator.filter(_.startsWith("page ")).toSeq shouldBe empty
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should not include "check_page()"
    hook should not include "check_e2e()"
    hook.linesIterator.map(_.trim).filter(_.endsWith("echo e2e")).toSeq shouldBe empty
    val gate = RepoFile.read(".github/gating-checks.tsv")
    gate should include("main.yml\te2e (rest)")
    gate should include("main.yml\tpage-tests-chrome (stable)")
    val ci = RepoFile.read(".github/workflows/ci.yml")
    ci should include("cmd: e2eRest")
  }

  /** A job sets up sbt, Node and the tools only when the hook would run a check of its share there (the hook's own
   *  selection, PREPUSH_LIST=1: precommit-share.sh); its hook step always runs, so a selection the cheap step missed
   *  fails there rather than going unchecked. The jobs the plan does not name (old refs, identity) are skipped. */
  "every check job" should "set up only for a share with checks, its hook step never skipped" in {
    Seq("check", "check-identity", "check-oldrefs").foreach { job =>
      RepoFile.step(jobs(job), "The pre-push hook's checks of the change") should not include "if:"
      jobs(job) should include("if: steps.share.outputs.checks != ''")
    }
    jobs("check-oldrefs") should include("if: ${{ needs.plan.outputs.oldrefs == 'true' }}")
  }

  /** The identity module's tests ran after the sbt check in the rest's job: 292 s then 585 s, a 15.9-minute job (run
   *  41276112019; docs/investigations/2026-11-01-twenty-runners.md), then in one job of their own (~12.5 min), then three
   *  shards. TWO since 2026-11-17: an identity landing ran 7 check jobs on ci-runner's ~4 check slots, in waves (3 days to
   *  11-17: 44 identity landings, land -> LANDED p50 54 min, runner wait p50 25 min; 7 jobs against the others' 3), and
   *  the third shard's ~3 min of specs cost a whole job's setup and a slot. Created only for a change the plan gives an
   *  identity check, each half the selected specs (scripts/ci/shard_tests.py). */
  "the identity check" should "run in two shards beside the rest, only when the plan names it" in {
    val identity = jobs("check-identity")
    identity should include("needs: [ plan ]")
    identity should include("if: ${{ needs.plan.outputs.identity == 'true' }}")
    identity should include("shard: [1, 2]")
    identity should include("PREPUSH_IDENTITY_SHARD: ${{ matrix.shard }}/2")
    identity should include("name: check (identity ${{ matrix.shard }}/2)")
    RepoFile.read("scripts/hooks/pre-push") should include("scripts/ci/shard_tests.py --shard \"$PREPUSH_IDENTITY_SHARD\"")
  }

  /** 8 of Main's 14 code reds in the five days to 2026-10-30 were in integration-test or e2e rows nothing ran before the
   *  push (ci-failures-2026-10-30 §2.1): the landing check runs the ones the change can move, as Main runs them -- in the
   *  rest's job since 2026-11-18. A job of their own was created only after the plan and then waited for a slot (one
   *  run's `heavy rows` queued 10:09-10:45Z, p50 ~11 min over 2026-11-17); behind the rest's sbt check they also share
   *  its compile. Their MongoDB is named to them alone: the basic set's e2e specs run with none, as Main's do. */
  "the landing check" should "run every heavy row the change can move in the rest's job, with no slot of its own" in {
    jobs.keySet should not contain "heavy"
    jobs("plan") should not include "heavy="
    val rest = jobs("check")
    RepoFile.read("scripts/ci/precommit-share.sh") should include("""vars+=("PREPUSH_HEAVY_ROWS=1" "PREPUSH_ELSEWHERE=$elsewhere")""")
    rest should include("scripts/ci/start-mongo-replset.sh")
    rest should include("|| 'ubuntu-22.04' }}")
    rest should include("PREPUSH_HEAVY_MONGODB_URI: mongodb://127.0.0.1:27017/kinowo?directConnection=true")
    rest.linesIterator.map(_.trim).filter(_.startsWith("MONGODB_URI:")).toSeq shouldBe empty
    RepoFile.read("scripts/hooks/pre-push") should include("""local uri="${PREPUSH_HEAVY_MONGODB_URI:-${MONGODB_URI:-}}"""")
  }

  "the heavy rows" should "run Main's own commands" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("    heavy_row itrest itAll IT_ROW=rest\n}")
    hook should include("check_itidentity() { heavy_row itidentity itAll IT_ROW=identity; }")
    hook should include("check_e2ecorpus() { heavy_row e2ecorpus e2eCorpus; }")
    val ci = RepoFile.read(".github/workflows/ci.yml")
    ci should include("run: sbt itAll")
    ci should include("cmd: e2eRest")
    ci should include("cmd: e2eCorpus")
    RepoFile.commandAlias("itAll") should not be empty
  }

  /** The rest's hook ran `scripttests` (143-186 s) and then `sbt` (96-385 s), one after the other, in 5 of 5 sampled runs
   *  (2026-11-18): they share nothing, so the job runs the hook twice at once (scripts/ci/hook-beside.sh), the script
   *  self-tests alone beside every other check of the share. */
  "the rest's job" should "run the script self-tests beside its other checks, not after them" in {
    val step = RepoFile.step(jobs("check"), "The pre-push hook's checks of the change")
    step should include("""timeout --kill-after=60 80m scripts/ci/hook-beside.sh scripttests -- scripts/hooks/pre-push "$PREPUSH_MODE" "$PREPUSH_RANGE"""")
    step should include("CHECKS: ${{ steps.share.outputs.checks }}")
    RepoFile.read("scripts/ci/hook-beside.sh") should include("""PREPUSH_ONLY="$check" "$@"""")
  }

  "the pre-push hook" should "run only the checks its share names, and say which it leaves to the other jobs" in {
    val hook = RepoFile.read("scripts/hooks/pre-push")
    hook should include("split_out() {")
    hook should include("left to the landing check's other job(s):")
  }
}
