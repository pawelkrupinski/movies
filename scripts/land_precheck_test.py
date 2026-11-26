#!/usr/bin/env python3
"""scripts/land-precheck over a scratch repository: the rebase dry run names its conflicts, the hook's commit-rule and
linter checks run alone (PREPUSH_ONLY) over the range, the registry's lints for the pushed paths run in one sbt-warm
call, and the verdict is 0 (pass), 1 (a check refused) or 2 (could not check: agentctl queues the change anyway). Ported
from land-precheck-test.sh case for case, plus the command line (--base, --head, -h, a missing commit); the same cases
hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/land_precheck.py", "scripts/land-precheck")
# The stub hook: its plan modes answer, its checks record what they were asked and fail on $HOOK_FAIL
HOOK = r'''#!/usr/bin/env bash
case "$1" in
    --lint-plan) grep -q '\.scala$' && printf 'worker:tools.ALintSpec\ncommon:tools.BLintSpec\n'
                 [ "${PREPUSH_GITOPS:-}" = 1 ] && printf 'worker:deploy.AGitopsSpec\n'; exit 0 ;;
esac
echo "hook $* only=$PREPUSH_ONLY reuse=${PREPUSH_NO_REUSE:-}" >> "$STUB_LOG"
[ -z "${HOOK_FAIL:-}" ] || { echo "driftguards: abc123: Drift-Slice missing" >&2; exit 1; }
'''
# The stub sbt-warm: records its command and its governor class, fails on $SBT_FAIL with $SBT_OUT
SBT_WARM = r'''#!/usr/bin/env bash
echo "sbt-warm $*" >> "$STUB_LOG"
echo "class=${KINOWO_GOV_CLASS:-}" >> "$STUB_LOG"
[ -z "${SBT_OUT:-}" ] || echo "$SBT_OUT"
[ -z "${SBT_FAIL:-}" ]
'''
RUN_SCRIPT_TESTS = r'''#!/usr/bin/env bash
echo "run-script-tests $*" >> "$STUB_LOG"
[ -z "${SCRIPTTESTS_FAIL:-}" ] || { echo "FAIL 3s scripts/x-test.sh"; exit 1; }
'''
# infra/bin/fetch-gitops: records the call and where it puts the manifests, fails on $GITOPS_FAIL
FETCH_GITOPS = r'''#!/usr/bin/env bash
echo "fetch-gitops dest=${KINOWO_GITOPS_DEST:-}" >> "$STUB_LOG"
[ -z "${GITOPS_FAIL:-}" ] || { echo "could not reach github.com" >&2; exit 1; }
'''
STEP = r"^precheck (rebase|rules|lints) PASS \([0-9]+ s\)$"


class LandPrecheckTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.log = self.path("log")
        self.origin = self.repo("origin", "--bare", "-b", "main")
        self.checkout = self.repo("repo", "-b", "main")
        for name, text in (("scripts/hooks/pre-push", HOOK), ("scripts/sbt-warm", SBT_WARM),
                           ("scripts/run-script-tests", RUN_SCRIPT_TESTS), ("infra/bin/fetch-gitops", FETCH_GITOPS)):
            os.chmod(self.write(f"repo/{name}", text), 0o755)
        self.write("repo/notes.md", "one\n")
        self.commit_tree(self.checkout, "base")
        self.git(self.checkout, "remote", "add", "origin", self.origin)
        self.git(self.checkout, "push", "-q", "origin", "HEAD:main")
        self.git(self.checkout, "fetch", "-q", "origin")
        self.git(self.checkout, "checkout", "-qb", "change")
        self.write("repo/worker/A.scala", "object A\n")
        self.commit_tree(self.checkout, "a scala change")
        open(self.log, "w").close()

    def precheck(self, *args: str, **env: str) -> cli_harness.Done:
        done = self.run_tool(*args, env={"STUB_LOG": self.log, **env}, cwd=self.checkout)
        self.output = done.output
        return done

    def logged(self) -> str:
        with open(self.log) as f:
            return f.read()

    def log_lines(self, prefix: str) -> list:
        return [line for line in self.logged().splitlines() if line.startswith(prefix)]

    def reset_log(self) -> None:
        open(self.log, "w").close()

    def count(self, pattern: str, text: Optional[str] = None) -> int:
        return len(re.findall(pattern, self.output if text is None else text, re.M))

    # a clean change passes, every step named and timed
    def test_a_clean_change_passes_every_step_named_and_timed(self) -> None:
        done = self.precheck()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(3, self.count(STEP))
        self.assertRegex(done.stdout.splitlines()[-1], r"^PRECHECK PASS \([0-9]+ s\)$")

    def test_the_hook_runs_once_alone_on_the_rule_checks_never_reusing_a_pass(self) -> None:
        self.precheck()
        hooks = self.log_lines("hook")
        self.assertEqual(1, len(hooks), self.logged())
        self.assertRegex(hooks[0], r"--checks-only [0-9a-f]+\.\.[0-9a-f]+ only=allowlists driftguards newchecks "
                                   r"judgements shellcheck actionlint reuse=1")
        self.assertNotRegex(self.logged(), "trailers|redgate")

    def test_the_selected_lints_run_in_one_sbt_warm_call_admitted_as_a_landing_checks_jvm(self) -> None:
        self.precheck()
        self.assertEqual(["sbt-warm common/Test/testOnly tools.BLintSpec; "
                          "worker/Test/testOnly tools.ALintSpec deploy.AGitopsSpec"], self.log_lines("sbt-warm"))
        self.assertEqual(["class=check"], self.log_lines("class="))

    def test_a_governor_class_already_set_is_kept(self) -> None:
        self.precheck(KINOWO_GOV_CLASS="measure")
        self.assertEqual(["class=measure"], self.log_lines("class="))

    def test_the_bounds_end_a_run_past_them_as_unchecked(self) -> None:
        self.write("repo/scripts/hooks/pre-push", HOOK.replace("exit 0 ;;\nesac", "exit 0 ;;\nesac\nsleep 30"))
        done = self.precheck(PRECHECK_RULES_SECONDS="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(1, self.count(r"^precheck rules UNCHECKED"))
        self.assertRegex(done.stdout.splitlines()[-1], r"^PRECHECK UNCHECKED: rules \([0-9]+ s\)$")
        self.assertEqual(1, self.count(r"^precheck lints PASS"), "the lints still run after an unchecked rules step")

    # a refusal
    def test_a_failing_commit_rule_is_exit_1_with_the_hooks_own_line(self) -> None:
        done = self.precheck(HOOK_FAIL="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, self.count("precheck rules FAIL"))
        self.assertEqual(1, self.count("Drift-Slice missing"))
        self.assertEqual(1, self.count(r"^    driftguards: abc123: Drift-Slice missing$"), "indented four spaces")
        self.assertRegex(done.stdout.splitlines()[-1], r"^PRECHECK FAIL: rules \([0-9]+ s\)$")

    def test_a_refused_change_runs_nothing_slow_the_lints_are_skipped_as_refused_already(self) -> None:
        self.precheck(HOOK_FAIL="1")
        self.assertEqual([], self.log_lines("sbt-warm"))
        self.assertEqual(1, self.count(r"^precheck lints SKIP \(0 s\): refused already by rules$"))
        self.assertEqual([], self.log_lines("fetch-gitops"))

    def test_a_failing_lint_is_exit_1(self) -> None:
        done = self.precheck(SBT_FAIL="1", SBT_OUT="[info] *** FAILED *** x")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, self.count("precheck lints FAIL"))
        self.assertRegex(done.stdout.splitlines()[-1], r"^PRECHECK FAIL: lints \([0-9]+ s\)$")

    def test_the_fails_lines_are_the_failures_never_the_specs_that_passed(self) -> None:
        spec_out = "\n".join([
            "[info] ASpec:",
            "[info] - should pass first",
            "[info] - should take temp files through the helper *** FAILED ***",
            '[info]   List("x.sh:3") was not empty (ASpec.scala:88)',
            "[info]   at some.Frame(Frame.scala:1)",
            "[info] - should pass after",
            "[error] Failed tests:",
            "[error] \tdeploy.ASpec"])
        self.precheck(SBT_FAIL="1", SBT_OUT=spec_out)
        self.assertEqual(1, self.count(r"should take temp files through the helper \*\*\* FAILED"))
        self.assertEqual(1, self.count(r"was not empty \(ASpec.scala:88\)"))
        self.assertEqual(1, self.count("deploy.ASpec"))
        self.assertEqual(0, self.count("should pass|some.Frame"))
        self.assertEqual(1, self.count(r"^    \[info\] - should take temp files through the helper \*\*\* FAILED"),
                         "the detail lines indented four spaces")

    def test_a_clue_of_several_lines_is_kept_whole_with_no_stack_frame_after_it(self) -> None:
        spec_out = "\n".join([
            "[info] - should neither sleep to wait nor time anything *** FAILED ***",
            "[info]   These Python-test lines sleep to wait or time something, or allowlist the line with why:",
            "[info]   android/scripts/x_test.py:106: a line named why",
            '[info]   Vector("android/scripts/x_test.py:106: a line named why") was not empty (ASpec.scala:211)',
            "[info]   at some.Frame(Frame.scala:1)",
            "[info] - should pass after"])
        self.precheck(SBT_FAIL="1", SBT_OUT=spec_out)
        self.assertEqual(1, self.count(r"^    \[info\]   android/scripts/x_test.py:106: a line named why$"))
        self.assertEqual(1, self.count(r"was not empty \(ASpec.scala:211\)"))
        self.assertEqual(0, self.count("should pass|some.Frame"))

    def test_a_long_clue_keeps_at_most_twenty_lines_of_it(self) -> None:
        clue = [f"[info]   reason {n}" for n in range(30)]
        self.precheck(SBT_FAIL="1", SBT_OUT="\n".join(["[info] - should x *** FAILED ***", *clue]))
        self.assertEqual(20, self.count(r"^    \[info\]   reason "))

    def test_sbt_output_with_no_recognised_failure_is_shown_whole(self) -> None:
        self.precheck(SBT_FAIL="1", SBT_OUT="something odd happened")
        self.assertEqual(1, self.count(r"^precheck lints FAIL"))
        self.assertEqual(1, self.count(r"^    something odd happened$"))

    def test_sbt_that_could_not_run_is_exit_2_could_not_check(self) -> None:
        done = self.precheck(SBT_FAIL="1", SBT_OUT="sbt server did not start within 90 seconds")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(1, self.count("precheck lints UNCHECKED"))
        self.assertRegex(done.stdout.splitlines()[-1], r"^PRECHECK UNCHECKED: lints \([0-9]+ s\)$")

    def test_a_failed_test_beside_a_connection_line_is_still_a_refusal(self) -> None:
        done = self.precheck(SBT_FAIL="1", SBT_OUT="[info] - x *** FAILED ***\nserver disconnected")
        self.assertEqual(1, done.code, done.output)

    def test_a_lint_run_past_its_bound_is_unchecked(self) -> None:
        self.write("repo/scripts/sbt-warm", SBT_WARM.replace('echo "sbt-warm', "sleep 30\necho \"sbt-warm"))
        done = self.precheck(PRECHECK_LINT_SECONDS="1")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual(1, self.count("precheck lints UNCHECKED"))

    def test_the_steps_that_ran_are_both_named_when_one_refuses_and_none_unchecked(self) -> None:
        done = self.precheck(SBT_FAIL="1", SBT_OUT="[info] - x *** FAILED ***")
        self.assertEqual(1, self.count(r"^precheck rebase PASS"))
        self.assertEqual(1, self.count(r"^precheck rules PASS"))
        self.assertEqual(1, done.code)

    # a changed script: its self-tests are left to the landing check
    def test_a_changed_script_runs_no_self_test(self) -> None:
        self.write("repo/scripts/ci/x.sh", "echo x\n")
        self.commit_tree(self.checkout, "a script change")
        done = self.precheck()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.log_lines("run-script-tests"))
        self.assertEqual(0, self.count("scripttests"))

    def test_a_change_selecting_no_lint_skips_the_lints(self) -> None:
        self.git(self.checkout, "reset", "-q", "--hard", "HEAD~1")
        self.write("repo/notes.md", "two\n")
        self.commit_tree(self.checkout, "notes only")
        done = self.precheck(GITOPS_FAIL="1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.count(r"^precheck lints SKIP \([0-9]+ s\)$"))
        self.assertEqual([], self.log_lines("sbt-warm"))

    # the rebase dry run
    def conflicting_change(self) -> str:
        self.git(self.checkout, "checkout", "-q", "-b", "other", "origin/main")
        self.write("repo/notes.md", "two\n")
        self.git(self.checkout, "commit", "-qam", "main's notes")
        self.git(self.checkout, "push", "-q", "origin", "HEAD:main")
        self.git(self.checkout, "checkout", "-q", "change")
        self.write("repo/notes.md", "three\n")
        self.git(self.checkout, "commit", "-qam", "the change's notes")
        self.git(self.checkout, "fetch", "-q", "origin")
        return self.git(self.checkout, "rev-parse", "HEAD")

    def test_a_conflict_with_origin_main_is_exit_1_the_file_named(self) -> None:
        before = self.conflicting_change()
        done = self.precheck()
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, self.count("conflicts with origin/main in: notes.md "))
        self.assertEqual(1, self.count(r"^precheck rebase FAIL"))
        self.assertIn("rebase onto origin/main and resolve them before landing", done.output)
        self.assertEqual(before, self.git(self.checkout, "rev-parse", "HEAD"), "the branch is left as it was")
        self.assertEqual("change", self.git(self.checkout, "rev-parse", "--abbrev-ref", "HEAD").strip())
        self.assertEqual("", self.git(self.checkout, "status", "--porcelain"), "nothing done on the tree")

    def test_a_refused_rebase_ends_the_precheck_no_sbt_run(self) -> None:
        self.conflicting_change()
        done = self.precheck()
        self.assertEqual([], self.log_lines("sbt-warm"))
        self.assertEqual(1, self.count(r"^precheck lints SKIP \(0 s\): refused already by rebase$"))
        self.assertRegex(done.stdout.splitlines()[-1], r"^PRECHECK FAIL: rebase \([0-9]+ s\)$")

    def test_a_change_already_on_the_base_passes_the_rebase_without_a_replay(self) -> None:
        done = self.precheck("--base", "HEAD")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.count(r"^precheck rebase PASS"))

    # the GitOps manifests
    def test_the_gitops_manifests_are_fetched_once_into_the_checkouts_infra_kubernetes(self) -> None:
        self.precheck()
        fetched = self.log_lines("fetch-gitops")
        self.assertEqual(1, len(fetched), self.logged())
        self.assertRegex(fetched[0], r"^fetch-gitops dest=.*/infra/kubernetes$")
        self.assertEqual(["sbt-warm common/Test/testOnly tools.BLintSpec; "
                          "worker/Test/testOnly tools.ALintSpec deploy.AGitopsSpec"], self.log_lines("sbt-warm"))

    def test_a_failed_fetch_leaves_the_gitops_reading_specs_to_the_hook(self) -> None:
        done = self.precheck(GITOPS_FAIL="1")
        self.assertEqual(1, self.count(r"^precheck lints PASS"), done.output)
        self.assertEqual(["sbt-warm common/Test/testOnly tools.BLintSpec; worker/Test/testOnly tools.ALintSpec"],
                         self.log_lines("sbt-warm"))

    def test_an_inherited_prepush_gitops_selects_nothing_when_the_fetch_fails(self) -> None:
        self.precheck(PREPUSH_GITOPS="1", GITOPS_FAIL="1")
        self.assertEqual(["sbt-warm common/Test/testOnly tools.BLintSpec; worker/Test/testOnly tools.ALintSpec"],
                         self.log_lines("sbt-warm"))

    # the command line
    def test_base_and_head_name_the_range_the_hook_checks(self) -> None:
        head = self.git(self.checkout, "rev-parse", "HEAD").strip()
        base = self.git(self.checkout, "rev-parse", "origin/main").strip()
        done = self.precheck("--base", base, "--head", head)
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"--checks-only {base}..{head} ", self.logged())

    def test_the_help_prints_the_header_and_exits_0(self) -> None:
        for flag in ("-h", "--help"):
            done = self.precheck(flag)
            self.assertEqual(0, done.code, done.output)
            self.assertIn("THE FAST PRECHECK", done.stdout)
            self.assertIn("scripts/land-precheck [--base <ref>] [--head <ref>]", done.stdout)
            self.assertIn("Bounds: PRECHECK_RULES_SECONDS (300), PRECHECK_LINT_SECONDS (300).", done.stdout)
            self.assertNotIn("set -uo", done.stdout)

    def test_an_unknown_argument_is_a_usage_error_2(self) -> None:
        done = self.precheck("--frobnicate")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual("land-precheck: unknown argument --frobnicate\nland-precheck: see land-precheck --help\n",
                         done.stderr)
        self.assertEqual("", self.logged())

    def test_a_flag_without_its_value_is_a_usage_error_2(self) -> None:
        done = self.precheck("--base")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("land-precheck: --base needs a value", done.stderr)

    def test_a_missing_commit_is_a_failure_named(self) -> None:
        done = self.precheck("--base", "no-such-ref")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("land-precheck: no commit no-such-ref\n", done.stderr)
        done = self.precheck("--head", "no-such-ref")
        self.assertEqual("land-precheck: no commit no-such-ref\n", done.stderr)

    def test_it_runs_from_a_subdirectory_of_the_checkout(self) -> None:
        os.makedirs(self.path("repo/worker"), exist_ok=True)
        done = self.run_tool(env={"STUB_LOG": self.log}, cwd=self.path("repo/worker"))
        self.assertEqual(0, done.code, done.output)

    def test_it_leaves_no_temporary_file_behind(self) -> None:
        tmp = self.path("tmp")
        os.makedirs(tmp)
        self.run_tool(env={"STUB_LOG": self.log, "TMPDIR": tmp}, cwd=self.checkout)
        self.assertEqual([], os.listdir(tmp))


if __name__ == "__main__":
    unittest.main()
