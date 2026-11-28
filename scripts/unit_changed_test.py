#!/usr/bin/env python3
"""scripts/unit-changed, black-box (cli_harness), over a scratch repository: the paths committed since the merge base,
the uncommitted and the untracked go to the hook's --unit-plan, and its commands to one sbt-warm call; --plan prints
them; nothing touched runs nothing; a bad command line exits 2. Ported from unit-changed-test.sh case for case; the
same cases hold the shell tool and its Python rewrite. Run: python3 scripts/unit_changed_test.py"""
from __future__ import annotations

import os
import shutil
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

PLAN_STUB = """#!/bin/bash
[ "$1" = --unit-plan ] || exit 9
while IFS= read -r path; do echo "path $path" >> "$PLAN_LOG"; case "$path" in */src/*) echo "${path%%/*}/Test/test" ;; esac; done | sort -u
"""
SBT_STUB = """#!/bin/bash
printf "sbt-warm"; printf " [%s]" "$@"; echo
"""


class UnitChangedTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        origin = self.repo("origin.git", "--bare")
        self.repo_dir = self.repo("repo", "-b", "main")
        self.copy_scripts("repo")
        shutil.copy2(os.path.join(cli_harness.ROOT, "scripts", "unit-changed"), self.path("repo/scripts/unit-changed"))
        for name, text in (("scripts/hooks/pre-push", PLAN_STUB), ("scripts/sbt-warm", SBT_STUB)):
            self.write(f"repo/{name}", text)
            os.chmod(self.path("repo", name), 0o755)
        self.write("repo/a.txt", "a\n")
        self.commit_tree(self.repo_dir, "base")
        self.git(self.repo_dir, "remote", "add", "origin", origin)
        self.git(self.repo_dir, "push", "-q", "origin", "HEAD:main")
        self.git(self.repo_dir, "fetch", "-q", "origin")
        self.log = self.path("plan.log")

    def unit_changed(self, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, tool=self.path("repo/scripts/unit-changed"), cwd=self.repo_dir,
                             env={"PLAN_LOG": self.log})

    def test_nothing_touched_says_so_and_runs_nothing(self) -> None:
        done = self.unit_changed()
        self.assertEqual(0, done.code, done.output)
        self.assertIn("no unit tests to run", done.stdout)
        self.assertNotIn("sbt-warm", done.stdout)

    def test_committed_staged_and_untracked_paths_run_each_modules_command_in_one_call(self) -> None:
        self.write("repo/common/src/A.scala", "x\n")
        self.commit_tree(self.repo_dir, "committed")
        self.write("repo/worker/src/B.scala", "y\n")
        self.git(self.repo_dir, "add", "worker")
        self.write("repo/web/src/C.scala", "z\n")
        done = self.unit_changed()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("sbt-warm [common/Test/test] [web/Test/test] [worker/Test/test]", done.stdout.splitlines()[-1])
        self.assertEqual(3, self.read("plan.log").count("path "), "the plan handed each path once")

    def test_plan_prints_the_commands_and_runs_nothing(self) -> None:
        for module in ("common", "web", "worker"):
            self.write(f"repo/{module}/src/A.scala", "x\n")
        done = self.unit_changed("--plan")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("common/Test/test\nweb/Test/test\nworker/Test/test\n", done.stdout)

    def test_an_unknown_argument_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.unit_changed("--nonsense").code)

    def test_a_base_with_no_merge_base_is_refused(self) -> None:
        done = self.unit_changed("--base", "no-such-ref")
        self.assertNotEqual(0, done.code)
        self.assertIn("no merge base of no-such-ref and HEAD", done.stderr)


if __name__ == "__main__":
    unittest.main()
