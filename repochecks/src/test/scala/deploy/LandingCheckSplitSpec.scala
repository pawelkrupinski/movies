package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE SPLIT CHECK (scripts/lib/pre_push_hook/pre_push_push.py split, PREPUSH_ONLY / PREPUSH_ELSEWHERE): the landing check's jobs share the hook's
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
    Seq("check (identity 1/4)").filterNot(n => globs.exists(matches(_, n))) shouldBe empty
  }

  "the landing check" should "run identity in jobs of its own, each with a cap of its own" in {
    Map("check" -> "rest", "check-identity" -> "identity").foreach { case (job, share) =>
      jobs(job) should include(s"SHARE: $share")
      jobs(job) should include("""scripts/ci/precommit-share.sh env "$SHARE" >> "$GITHUB_ENV"""")
      jobs(job) should include("timeout --kill-after=60 80m ")
      jobs(job) should include("""scripts/hooks/pre-push "$PREPUSH_MODE" "$PREPUSH_RANGE"""")
    }
    val share = RepoFile.read("scripts/ci/precommit_share.py") // precommit-share.sh is a thin exec of it
    // identity is another job's only for a change's own checks (--checks-only): the fast-only and verify-only sets
    // carry no identity area, and verify-only's identity unit tests ride the rest's sbt
    share should include("""elsewhere = "identity" if mode == CHECKS_ONLY else """"")
    share should include(""""PREPUSH_LIST": "1"""")
  }

  /** The owner, 2026-11-21 ("(5)"): identity-slice's old-ref compile caught 1 change in 27 landing checks, at a p90 of
   *  20 min. It runs after landing, as Main's `old-ref-compile` row (a gate row: .github/gating-checks.tsv), never
   *  before: no landing-check job, no hook check, no land-precheck rule. */
  "the landing check" should "leave identity-slice's old-ref compile to Main, after landing" in {
    jobs.keySet should not contain "check-oldrefs"
    jobs("plan") should not include "outputs.oldrefs"
    RepoFile.read("scripts/lib/pre_push_hook/checks/pre_push_registry.py") should not include "\"oldrefs\""
    RepoFile.read("scripts/land-precheck") should not include " oldrefs\""
    RepoFile.jobs(RepoFile.read(".github/workflows/main.yml"))("old-ref-compile") should include(
      "bash scripts/identity-slice-old-ref-test.sh")
    RepoFile.read(".github/gating-checks.tsv") should include("main.yml\tidentity-slice old-ref compile\t")
  }

  /** The owner, 2026-11-17 ("B1"): the page area and `e2e (rest)` run only after landing, on Main's own rows -- in a week
   *  of landing checks the page area failed 0 of 14 (317 runner-minutes, ~23 min each) and e2e 0 of 95 (303). Neither
   *  the landing check, its plan, the hook's areas nor its heavy rows may bring them back before the push; Main still
   *  runs both, and THE GATE reads their rows (.github/gating-checks.tsv). */
  "the landing check" should "leave the page tests and e2e (rest) to Main, after landing" in {
    jobs.keySet should not contain "check-page"
    jobs("plan") should not include "outputs.page"
    jobs("check") should include("grep -qxE 'itrest|itidentity'")
    RepoFile.read("scripts/ci/precommit_share.py") should include("""SHARES = ("rest", "identity", "plan")""")
    RepoFile.read("scripts/hooks/local-tests.txt").linesIterator.filter(_.startsWith("page ")).toSeq shouldBe empty
    // every check is Python's since the hook's stage 6: no `page` or `e2e` check registered, none planned
    val checks = RepoFile.read("scripts/lib/pre_push_hook/checks/pre_push_registry.py")
    checks should not include "\"page\""
    checks should not include "\"e2e\""
    RepoFile.read("scripts/lib/pre_push_hook/plan/pre_push_plan.py") should not include "rows.append(\"e2e\")"
    val gate = RepoFile.read(".github/gating-checks.tsv")
    gate should include("main.yml\te2e (rest)")
    gate should include("main.yml\tpage-tests-chrome (stable)")
    val ci = RepoFile.read(".github/workflows/ci.yml")
    ci should include("cmd: e2eRest")
  }

  /** A job sets up sbt, Node and the tools only when the hook would run a check of its share there (the hook's own
   *  selection, PREPUSH_LIST=1: precommit-share.sh); its hook step always runs, so a selection the cheap step missed
   *  fails there rather than going unchecked. The jobs the plan does not name (identity) are skipped. */
  "every check job" should "set up only for a share with checks, its hook step never skipped" in {
    Seq("check", "check-identity").foreach { job =>
      RepoFile.step(jobs(job), "The pre-push hook's checks of the change") should not include "if:"
      jobs(job) should include("if: steps.share.outputs.checks != ''")
    }
    jobs("check-identity") should include("if: ${{ needs.plan.outputs.identity == 'true' }}")
  }

  /** The identity module's tests ran after the sbt check in the rest's job: 292 s then 585 s, a 15.9-minute job (run
   *  41276112019; docs/investigations/2026-11-01-twenty-runners.md), then in one job of their own (~12.5 min), then three
   *  shards, then two (2026-11-17: an identity landing ran 7 check jobs on ci-runner's ~4 check slots, in waves). FOUR
   *  since 2026-11-21 (the owner's "(3)"): two shards ran ~13 min each over ~5,500 tests, the landing's longest job; a
   *  quarter runs ~6.5 min, and the identity landings are 41% of all (2026-11-19..21). Created only for a
   *  change the plan gives an identity check, each a quarter of the selected specs (scripts/ci/shard_tests.py). */
  "the identity check" should "run in four shards beside the rest, only when the plan names it" in {
    val identity = jobs("check-identity")
    identity should include("needs: [ plan ]")
    identity should include("if: ${{ needs.plan.outputs.identity == 'true' }}")
    identity should include("shard: [1, 2, 3, 4]")
    identity should include("PREPUSH_IDENTITY_SHARD: ${{ matrix.shard }}/4")
    identity should include("name: check (identity ${{ matrix.shard }}/4)")
    RepoFile.read("scripts/lib/pre_push_hook/checks/pre_push_area.py") should include(""""scripts/ci/shard_tests.py", "--shard", shard""")
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
    RepoFile.read("scripts/ci/precommit_share.py") should include("""[("PREPUSH_HEAVY_ROWS", "1"), ("PREPUSH_ELSEWHERE", elsewhere)]""")
    rest should include("scripts/ci/start-mongo-replset.sh")
    rest should include("|| 'ubuntu-22.04' }}")
    rest should include("PREPUSH_HEAVY_MONGODB_URI: mongodb://127.0.0.1:27017/kinowo?directConnection=true")
    rest.linesIterator.map(_.trim).filter(_.startsWith("MONGODB_URI:")).toSeq shouldBe empty
    RepoFile.read("scripts/lib/pre_push_hook/checks/pre_push_heavy.py") should include("""return split_value(c, "PREPUSH_HEAVY_MONGODB_URI") or c.repo.env.get("MONGODB_URI", "")""")
  }

  "the heavy rows" should "run Main's own commands" in {
    val rows = RepoFile.read("scripts/lib/pre_push_hook/checks/pre_push_heavy.py")
    rows should include("""return heavy_row(c, "itrest", "itAll", "IT_ROW=rest")""")
    rows should include("""return heavy_row(c, "itidentity", "itAll", "IT_ROW=identity")""")
    rows should include("""return heavy_row(c, "e2ecorpus", "e2eCorpus")""")
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
    // ... with run-script-tests' tree check off: it moves what it did not find aside, and the rest's itrest had just
    // fetched the GitOps manifests (landing checks 44984605717, 44998261508: the deploy specs found no infra/kubernetes)
    RepoFile.read("scripts/ci/hook-beside.sh") should include("""PREPUSH_ONLY="$check" RUN_SCRIPT_TESTS_TREE_CHECK=0 "$@"""")
  }

  "the pre-push hook" should "run only the checks its share names, and say which it leaves to the other jobs" in {
    // the run is Python's since the hook's stage 2 (shell-to-Python migration, section 10): pre_push_push.split
    val push = RepoFile.read("scripts/lib/pre_push_hook/pre_push_push.py")
    push should include("def split(say: Say, env: Mapping[str, str], checks: List[str]) -> List[str]:")
    push should include("left to the landing check's other job(s): ")
  }
}
