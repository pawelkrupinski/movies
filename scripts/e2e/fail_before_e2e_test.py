#!/usr/bin/env python3
"""END TO END: scripts/fail-before, the commit gate's proof (CLAUDE.md, "Tests: the commit gate"), over a REAL git
repository -- its two throwaway worktrees made and removed in that repository's own .git, the test command run in
each -- with nothing stubbed (no sbt: the toy project's test is a plain python3 one). scripts/e2e/e2e_world.py.

  - a fix committed with its test: FAIL-BEFORE: yes, PASS-AFTER: yes, exit 0, and the caller's tree and worktree list
    are as they were;
  - a test that passes at the parent too (it asserts nothing the commit changed): FAIL-BEFORE: no, exit 1.
Mutation (scripts/e2e/mutations): the default test-side globs widened to every file, so BEFORE gets the fix too."""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402

BUGGY = "def add(a, b):\n    return a - b\n"
FIXED = "def add(a, b):\n    return a + b\n"
TEST = ("import unittest\nfrom calc import add\n\n\nclass AddTest(unittest.TestCase):\n"
        "    def test_adds(self):\n        self.assertEqual({expected}, add(2, 3))\n\n\nunittest.main()\n")


class FailBeforeE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.toy_repo("calc", {"calc.py": BUGGY, "README.md": "a toy\n"})
        self.tree = self.repo.worktree("fix")

    def fail_before(self) -> e2e_world.cli_harness.Done:
        return self.w.run([self.w.tool("scripts/fail-before"), "--", "python3 -B calc_test.py"], self.tree)

    def test_a_fix_with_its_test_fails_before_and_passes_after(self) -> None:
        self.w.write(self.tree, "calc.py", FIXED)
        self.w.write(self.tree, "calc_test.py", TEST.format(expected=5))
        self.w.commit(self.tree, "add adds")
        worktrees = self.w.git(self.repo.clone, "worktree", "list", "--porcelain")
        done = self.fail_before()
        self.assertRan(done, 0, "FAIL-BEFORE: yes", "PASS-AFTER: yes")
        self.assertEqual("", self.w.git(self.tree, "status", "--porcelain"), "the caller's tree was touched")
        self.assertEqual(worktrees, self.w.git(self.repo.clone, "worktree", "list", "--porcelain"),
                         "a throwaway worktree was left in the repository")

    def test_a_test_passing_at_the_parent_proves_nothing(self) -> None:
        self.w.write(self.tree, "calc.py", FIXED)
        self.w.write(self.tree, "calc_test.py", TEST.format(expected="add(2, 3)"))
        self.w.commit(self.tree, "a test asserting nothing the change did")
        self.assertRan(self.fail_before(), 1, "FAIL-BEFORE: no", "PASS-AFTER: yes")


if __name__ == "__main__":
    unittest.main()
