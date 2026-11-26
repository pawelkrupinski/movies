#!/usr/bin/env python3
"""scripts/fail-before, black-box, against scratch repositories: a commit adding a failing-first test plus its fix is
yes/yes; a commit whose test already passes at the parent is "FAIL-BEFORE: no"; one with no test-side file is refused;
which src/ directories are test-side is read from the commit's build.sbt; a run past the deadline is no verdict and ends
with its children; --ci hands both runs to scripts/ci-sbt (stubbed here) and the auto route reads the governor's
`gov route`; and no worktree or work directory is left. Ported from fail-before-test.sh case for case, plus the usage
and setup refusals; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/fail_before.py", "scripts/fail-before")
# ci-sbt: runs the "sbt command" as a script in the commit's tree (as CI runs it in a checkout of it), logging its call
CI_SBT = '''
c=""; l=""
while [ $# -gt 0 ]; do case "$1" in --commit) c=$2; shift 2 ;; --log) l=$2; shift 2 ;; --) shift; break ;; *) shift ;; esac; done
echo "$c $*" >> "$CI_RUNS"
[ -z "${NO_VERDICT:-}" ] || exit 2
t=$(mktemp -d); git archive "$c" | tar -x -C "$t"; (cd "$t" && bash $1) > "$l" 2>&1; rc=$?; rm -rf "$t"
[ "$rc" -eq 0 ] && exit 0; exit 1
'''
GOVERNOR = '''
[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores route width"; exit 0; }
[ "$1 $2" = "gov route" ] && { echo "$ROUTE: stub" >&2; [ "$ROUTE" = ci ]; exit; }
exit 1
'''


def line(output: str, prefix: str) -> str:
    return "".join(re.findall(rf"^{prefix}(.*)$", output, re.M))


class FailBeforeTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.repository = self.repo("repo")
        self.write_commit({"greet.sh": "echo broken\n"}, "base")

    def write_commit(self, files: Dict[str, str], message: str) -> str:
        for name, text in files.items():
            self.write(f"repo/{name}", text)
        self.git(self.repository, "add", "-A")
        self.git(self.repository, "commit", "-qm", message)
        return self.git(self.repository, "rev-parse", "HEAD").strip()

    def fail_before(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        extra: Dict[str, Optional[str]] = {"KINOWO_FAIL_BEFORE_DIR": self.path("work")}
        extra.update(env or {})
        return self.run_tool(*args, env=extra, cwd=self.repository)

    def verdict(self, done: cli_harness.Done) -> str:
        return f"{done.code} {line(done.stdout, 'FAIL-BEFORE: ')} {line(done.stdout, 'PASS-AFTER: ')}"

    def fix(self) -> None:
        self.write_commit({"greet.sh": "echo hello\n", "greet-test.sh": 'test "$(bash greet.sh)" = hello\n'}, "fix")

    def test_a_fix_with_its_failing_first_test_is_yes_yes(self) -> None:
        self.fix()
        done = self.fail_before("--", "bash", "greet-test.sh")
        self.assertEqual("0 yes yes", self.verdict(done), done.output)
        self.assertIn("--- BEFORE (exit 1), last lines ---", done.stdout)
        self.assertIn("--- AFTER (exit 0), last lines ---", done.stdout)
        self.assertRegex(done.stdout, r"fail-before: [0-9a-f]{40} \(1 test-side file\(s\) copied onto [0-9a-f]+\)")

    def test_a_shell_line_is_one_argument_run_as_written(self) -> None:
        self.fix()
        done = self.fail_before("--", 'test "$(bash greet.sh)" = hello && bash greet-test.sh')
        self.assertEqual("0 yes yes", self.verdict(done), done.output)

    def test_the_commit_under_test_may_be_named(self) -> None:
        self.fix()
        sha = self.git(self.repository, "rev-parse", "HEAD").strip()
        self.write_commit({"unrelated.txt": "x\n"}, "unrelated")
        done = self.fail_before(sha, "--", "bash", "greet-test.sh")
        self.assertEqual("0 yes yes", self.verdict(done), done.output)
        self.assertIn(f"fail-before: {sha} ", done.stdout)

    def test_both_trees_are_seeded_from_the_callers_own_build_of_the_commit_where_it_has_one(self) -> None:
        self.fix()
        self.write_commit({"seed-test.sh": 'test "$KINOWO_SBT_WARM_SEED" = "$EXPECTED_SEED"; s=$?; '
                                           'test "$(bash greet.sh)" = hello && exit $s; exit 1\n'}, "a test reading the seed")
        done = self.fail_before("--files", "seed-test.sh", "--", "bash", "seed-test.sh", env={"EXPECTED_SEED": ""})
        self.assertEqual("yes", line(done.stdout, "PASS-AFTER: "), done.output)
        os.makedirs(self.path("repo", "worker", "target", "scala-3.x", "zinc"))
        done = self.fail_before("--files", "seed-test.sh", "--", "bash", "seed-test.sh",
                                env={"EXPECTED_SEED": self.repository})
        self.assertEqual("yes", line(done.stdout, "PASS-AFTER: "), done.output)

    def test_a_test_green_at_the_parent_is_fail_before_no(self) -> None:
        self.write_commit({"exists-test.sh": "test -f greet.sh\n"}, "a vacuous test")
        done = self.fail_before("--", "bash", "exists-test.sh")
        self.assertEqual("1 no yes", self.verdict(done), done.output)

    def test_a_commit_with_no_test_side_file_is_refused_unless_files_names_one(self) -> None:
        self.write_commit({"greet.sh": "echo hello world\n", "probe.txt": "marker\n"}, "production only")
        done = self.fail_before("--", "true")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("fail-before: ", done.stderr)
        self.assertIn("changes no test-side file (see --files): nothing to prove", done.stderr)
        done = self.fail_before("--files", "probe.txt", "--", 'test "$(bash greet.sh)" = "hello world"')
        self.assertEqual(0, done.code, done.output)

    def test_which_src_directories_are_test_side_comes_from_the_commits_build_sbt(self) -> None:
        self.write_commit({"build.sbt": 'lazy val PageTest = config("page") extend Test\n'
                                        'lazy val Fixtures = config("fixtures") extend Compile\n',
                           "worker/src/fixtures/scala/Archive.scala": "archive=broken\n"}, "a build with fixtures")
        self.write_commit({"worker/src/fixtures/scala/Archive.scala": "archive=fixed\n",
                           "worker/src/test/scala/ArchiveSpec.sh": "grep -q archive=fixed worker/src/fixtures/scala/Archive.scala\n"},
                          "a fix in src/fixtures, its test in src/test")
        done = self.fail_before("--", "test -f worker/src/test/scala/ArchiveSpec.sh && bash worker/src/test/scala/ArchiveSpec.sh")
        self.assertEqual("0 yes yes", self.verdict(done), "src/fixtures is code under test, kept at the parent\n" + done.output)
        self.write_commit({"worker/src/page/scala/PageSpec.sh": "exit 1\n",
                           "worker/src/fixtures/scala/Archive.scala": "archive=fixed again\n"},
                          "a test in src/page, a configuration extending Test")
        done = self.fail_before("--", "test ! -f worker/src/page/scala/PageSpec.sh || bash worker/src/page/scala/PageSpec.sh")
        self.assertEqual("yes", line(done.stdout, "FAIL-BEFORE: "), done.output)

    def test_a_configuration_extending_test_through_another_and_a_source_directory_override_are_test_side(self) -> None:
        self.write_commit({"build.sbt": 'lazy val Slow = config("slow") extend Quick\nlazy val Quick = config("quick") extend(Test)\n'
                                        'Slow / scalaSource := baseDirectory.value / "src" / "heavy"\n'}, "a build")
        self.write_commit({"w/src/quick/a.sh": "x\n", "w/src/slow/b.sh": "x\n", "w/src/heavy/c.sh": "x\n",
                           "w/src/main/d.sh": "x\n"}, "test-side and not")
        for path, copied in (("w/src/quick/a.sh", "yes"), ("w/src/slow/b.sh", "yes"), ("w/src/heavy/c.sh", "yes"),
                             ("w/src/main/d.sh", "no")):
            done = self.fail_before("--", f"test ! -f {path}")
            self.assertEqual(copied, line(done.stdout, "FAIL-BEFORE: "), f"{path}\n{done.output}")

    def test_several_arguments_are_one_command_each_kept_whole(self) -> None:
        self.write_commit({"greet.sh": "echo hello world again\n",
                           "args-test.sh": 'test "$#" = 1 && test "$1" = "two words" && '
                                           'test "$(bash greet.sh)" = "hello world again"\n'}, "a test taking an argument with a space")
        done = self.fail_before("--", "bash", "args-test.sh", "two words")
        self.assertEqual(0, done.code, done.output)

    def test_a_test_side_file_under_a_non_ascii_path_is_carried_over(self) -> None:
        self.write_commit({"greet.sh": "echo hallo\n", "test/resources/köln/case-test.sh": 'test "$(bash greet.sh)" = hallo\n'},
                          "a test under a non-ASCII path")
        done = self.fail_before("--", "bash", "test/resources/köln/case-test.sh")
        self.assertEqual(0, done.code, done.output)

    def test_a_run_past_the_deadline_is_no_verdict_and_ends_with_its_children(self) -> None:
        grandchild = self.path("grandchild")
        self.write_commit({"hang-test.sh": 'sleep 86400 & echo $! >> "$GRANDCHILD"; wait\n'}, "a test that hangs")
        open(grandchild, "w").close()
        done = self.fail_before("--", "bash", "hang-test.sh",
                                env={"KINOWO_FAIL_BEFORE_TIMEOUT": "1", "GRANDCHILD": grandchild})
        self.assertEqual((2, 0, True), (done.code, done.output.count("FAIL-BEFORE: yes"), "timed out" in done.output),
                         done.output)
        self.assertIn("a run timed out after 1s (exit 124): no verdict", done.stderr)
        self.assertEqual(0, cli_harness.still_running(grandchild))
        self.assertEqual(1, len(self.read("grandchild").split()), "AFTER ran although BEFORE had no verdict")

    def ci(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        self.fakes.script("ci-sbt", CI_SBT)
        runs = self.path("ci-runs")
        open(runs, "a").close()
        extra: Dict[str, Optional[str]] = {"KINOWO_CI_SBT": os.path.join(self.fakes.bin, "ci-sbt"), "CI_RUNS": runs}
        extra.update(env or {})
        return self.fail_before(*args, env=extra)

    def runs(self) -> List[str]:
        return [row for row in self.read("ci-runs").splitlines() if row]

    def test_ci_runs_both_sides_on_ci_before_being_the_parent_with_the_test_side_files_laid_over_it(self) -> None:
        self.write_commit({"greet.sh": "echo ciao\n", "ci-test.sh": 'test "$(bash greet.sh)" = ciao\n'}, "a fix proved on CI")
        fix = self.git(self.repository, "rev-parse", "HEAD").strip()
        done = self.ci("--ci", "--", "scripts/sbt-warm", "ci-test.sh")
        self.assertEqual("0 yes yes", self.verdict(done), done.output)
        self.assertIn("on CI: sbt ci-test.sh", done.stdout)
        self.assertEqual(2, len(self.runs()))
        self.assertEqual(1, self.runs().count(f"{fix} ci-test.sh"))
        before = [row.split()[0] for row in self.runs() if not row.startswith(fix)][0]
        self.assertEqual("echo broken", self.git(self.repository, "show", f"{before}:greet.sh").strip())
        self.assertIn("ciao", self.git(self.repository, "show", f"{before}:ci-test.sh"))
        self.assertEqual(self.git(self.repository, "rev-parse", f"{fix}^"), self.git(self.repository, "rev-parse", f"{before}^"))

    def test_ci_a_test_green_at_the_parent_is_fail_before_no(self) -> None:
        self.write_commit({"ci-vacuous-test.sh": "test -f greet.sh\n"}, "a vacuous test on CI")
        done = self.ci("--ci", "--", 'scripts/sbt-warm "ci-vacuous-test.sh"')
        self.assertEqual("1 no yes", self.verdict(done), done.output)

    def test_ci_a_run_with_no_verdict_is_no_verdict_never_fail_before_yes(self) -> None:
        self.write_commit({"ci-vacuous-test.sh": "test -f greet.sh\n"}, "a vacuous test on CI")
        done = self.ci("--ci", "--", "scripts/sbt-warm", "ci-vacuous-test.sh", env={"NO_VERDICT": "1"})
        self.assertEqual((2, 0), (done.code, done.output.count("FAIL-BEFORE: yes")), done.output)
        self.assertIn("a CI run gave no verdict (BEFORE exit 2, AFTER exit 2", done.stderr)

    def test_ci_refuses_a_command_that_is_not_one_sbt_warm_call(self) -> None:
        self.write_commit({"ci-vacuous-test.sh": "test -f greet.sh\n"}, "a vacuous test")
        for command in ("scripts/sbt-warm a && rm -rf x", "bash a.sh", "scripts/sbt-warm --stop x"):
            done = self.ci("--ci", "--", command)
            self.assertEqual(2, done.code, done.output)
            self.assertIn("--ci runs a command that is one scripts/sbt-warm call", done.stderr)
        self.assertEqual([], self.runs())

    def governed(self, route: str, *command: str) -> cli_harness.Done:
        self.fakes.script("agentctl", GOVERNOR)
        return self.ci(*command, env={"KINOWO_GOVERNOR": "1", "KINOWO_GOVERNOR_BIN": os.path.join(self.fakes.bin, "agentctl"),
                                      "KINOWO_JVM_GATE_OS": "Darwin", "ROUTE": route, "KINOWO_GOV_CLASS": "agent"})

    def test_auto_runs_on_ci_when_the_governor_would_hold_it_and_here_when_it_says_local(self) -> None:
        self.write_commit({"ci-vacuous-test.sh": "test -f greet.sh\n"}, "a vacuous test")
        done = self.governed("ci", "--", "scripts/sbt-warm", "ci-vacuous-test.sh")
        self.assertEqual(2, len(self.runs()), done.output)
        self.assertIn("the governor would hold two local runs now and ci-runner's measure lane is free", done.stderr)
        self.fresh_runs()
        done = self.governed("local", "--", "test -f ci-vacuous-test.sh")
        self.assertEqual([], self.runs(), done.output)

    def fresh_runs(self) -> None:
        open(self.path("ci-runs"), "w").close()

    def test_a_route_that_is_neither_auto_local_nor_ci_is_a_usage_error(self) -> None:
        self.write_commit({"t-test.sh": "true\n"}, "t")
        done = self.fail_before("--", "true", env={"KINOWO_FAIL_BEFORE_ROUTE": "sometimes"})
        self.assertEqual(2, done.code, done.output)
        self.assertIn("fail-before: KINOWO_FAIL_BEFORE_ROUTE is auto, local or ci, not sometimes", done.stderr)

    def test_usage_errors_exit_2(self) -> None:
        for args, said in ((("--bogus",), "unknown option --bogus"), (("--files",), "--files needs a value"),
                           ((), "usage: scripts/fail-before [<commit>] [--files <glob>]... [--ci | --local] -- <test command...>"),
                           (("HEAD",), "usage: scripts/fail-before")):
            done = self.fail_before(*args)
            self.assertEqual(2, done.code, done.output)
            self.assertIn(f"fail-before: {said}", done.stderr)
            if "unknown" in said:
                self.assertIn("fail-before: see fail-before --help", done.stderr)

    def test_help_prints_the_header(self) -> None:
        done = self.fail_before("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(done.stdout.startswith("Purpose: Proves the commit gate: a test that fails before the change and passes after it\n\n"
                                               "Proves the commit gate: a test that FAILS before the change and PASSES after it.\n"),
                        done.stdout[:200])
        self.assertIn("scripts/fail-before [<commit>] [--files <glob>]... [--ci | --local] -- <test command...>", done.stdout)

    def test_setup_errors_exit_2(self) -> None:
        done = self.run_tool("--", "true", cwd=self.work)
        self.assertEqual((2, True), (done.code, "fail-before: not inside a git checkout" in done.stderr), done.output)
        done = self.fail_before("nonesuch", "--", "true")
        self.assertEqual((2, True), (done.code, "fail-before: no such commit: nonesuch" in done.stderr), done.output)
        done = self.fail_before(self.git(self.repository, "rev-list", "--max-parents=0", "HEAD").strip(), "--", "true")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("has no parent", done.stderr)

    def test_a_hooks_git_variables_do_not_redirect_it(self) -> None:
        self.fix()
        elsewhere = self.repo("elsewhere")
        done = self.fail_before("--", "bash", "greet-test.sh", env={"GIT_DIR": os.path.join(elsewhere, ".git")})
        self.assertEqual("0 yes yes", self.verdict(done), done.output)

    def test_no_worktree_or_work_directory_is_left(self) -> None:
        self.fix()
        self.fail_before("--", "bash", "greet-test.sh")
        self.write_commit({"exists-test.sh": "test -f greet.sh\n"}, "a vacuous test")
        self.fail_before("--", "bash", "exists-test.sh")
        self.assertEqual(1, len(self.git(self.repository, "worktree", "list").strip().splitlines()))
        self.assertEqual([], os.listdir(self.path("work")))


if __name__ == "__main__":
    unittest.main()
