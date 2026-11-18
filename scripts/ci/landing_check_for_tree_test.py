#!/usr/bin/env python3
"""landing_check_for_tree.py: a tree a full landing check passed names that run; another tree, a fast-only batch check,
a run that did not succeed, or gh failing names none -- and Tool self-tests runs.
Run: python3 scripts/ci/landing_check_for_tree_test.py"""
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import landing_check_for_tree as t  # noqa: E402

TOOL = os.path.join(os.path.dirname(os.path.abspath(__file__)), "landing_check_for_tree.py")
TREE, OTHER = "a" * 40, "b" * 40


class PassedForTest(unittest.TestCase):

    def test_a_full_check_that_passed_this_tree_names_its_run(self):
        rows = [("1", "check/20261118-x", OTHER, "success"), ("2", "check/verify-abc-y", TREE, "success")]
        self.assertEqual(t.passed_for(TREE, rows), "2")

    def test_no_check_of_this_tree_names_none(self):
        self.assertIsNone(t.passed_for(TREE, [("1", "check/x", OTHER, "success")]))

    def test_the_fast_only_batch_check_ran_no_script_tests(self):
        for ref in ("check/fast-20261118-x", "check/red-fast-20261118-x"):
            self.assertIsNone(t.passed_for(TREE, [("1", ref, TREE, "success")]), ref)
        self.assertEqual(t.passed_for(TREE, [("1", "check/red-20261118-x", TREE, "success")]), "1")

    def test_a_run_that_did_not_succeed_names_none(self):
        self.assertIsNone(t.passed_for(TREE, [("1", "check/x", TREE, "failure")]))


class CommandLineTest(unittest.TestCase):

    def run_with_gh(self, gh_body, tree=TREE):
        with tempfile.TemporaryDirectory() as d:
            gh = os.path.join(d, "gh")
            with open(gh, "w") as fh:
                fh.write("#!/bin/sh\n" + gh_body)
            os.chmod(gh, 0o755)
            env = dict(os.environ, PATH=d + os.pathsep + os.environ["PATH"])
            return subprocess.run([sys.executable, TOOL, tree], capture_output=True, text=True, env=env)

    def test_the_run_is_printed(self):
        out = self.run_with_gh(f"printf '7\\tcheck/x\\t{TREE}\\tsuccess\\n'\n")
        self.assertEqual((out.returncode, out.stdout.strip()), (0, "7"))

    def test_none_exits_1(self):
        self.assertEqual(self.run_with_gh(f"printf '7\\tcheck/x\\t{OTHER}\\tsuccess\\n'\n").returncode, 1)

    def test_gh_failing_exits_2_and_says_so(self):
        out = self.run_with_gh("echo 'HTTP 403' >&2; exit 1\n")
        self.assertEqual(out.returncode, 2)
        self.assertIn("the tests run", out.stderr)

    def test_no_tree_sha_exits_2(self):
        self.assertEqual(self.run_with_gh("exit 0\n", tree="main").returncode, 2)


if __name__ == "__main__":
    unittest.main()
