#!/usr/bin/env python3
"""tool_git.py: git in one repository through the runner -- `out` its stdout or None when git failed, `ok` its exit,
`options` (`-c key=value`) before the repository, a bound passed to the runner."""
from __future__ import annotations

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cli_harness  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402


class GitTest(unittest.TestCase):
    def test_out_ok_and_options(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            env = cli_harness.isolated_env(os.environ)
            git = tool_git.Git(tool_process.SubprocessRunner(), env, d)
            self.assertTrue(git.ok("init", "-q"))
            self.assertEqual("x\n", git.out("config", "--get", "kinowo.flag", quiet=True) if git.call(
                "config", "kinowo.flag", "x").code == 0 else None)
            self.assertIsNone(git.out("rev-parse", "--verify", "-q", "HEAD", quiet=True))
            self.assertFalse(git.ok("rev-parse", "--verify", "-q", "HEAD", quiet=True))
            done = git.call("config", "--get", "kinowo.other", options=("-c", "kinowo.other=y"), timeout=30)
            self.assertEqual((0, "y\n"), (done.code, done.stdout))

    def test_acts_on_its_root_under_a_hooks_git_dir_and_prints_paths_as_themselves(self) -> None:
        with tempfile.TemporaryDirectory() as hook, tempfile.TemporaryDirectory() as d:
            env = cli_harness.isolated_env(os.environ)
            tool_git.Git(tool_process.SubprocessRunner(), env, hook).ok("init", "-q")
            under_hook = {**env, "GIT_DIR": os.path.join(hook, ".git"), "GIT_WORK_TREE": hook}
            git = tool_git.Git(tool_process.SubprocessRunner(), under_hook, d)
            self.assertTrue(git.ok("init", "-q"))
            with open(os.path.join(d, "przemyśl.tsv"), "w") as f:
                f.write("x\n")
            self.assertTrue(git.ok("add", "przemyśl.tsv"))
            self.assertEqual("przemyśl.tsv\n", git.out("diff", "--cached", "--name-only"))
            self.assertEqual("", tool_git.Git(tool_process.SubprocessRunner(), env, hook).out("status", "--porcelain"))

    def test_line_and_lines_answer_stripped_stdout_or_nothing_when_git_failed(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            git = tool_git.Git(tool_process.SubprocessRunner(), cli_harness.isolated_env(os.environ), d)
            self.assertIsNone(git.line("rev-parse", "--verify", "-q", "HEAD"))
            self.assertEqual([], git.lines("ls-files"))
            self.assertTrue(git.ok("init", "-q"))
            for name in ("a", "b"):
                open(os.path.join(d, name), "w").close()
            self.assertEqual(["a", "b"], git.lines("ls-files", "--others"))
            self.assertEqual("a", git.line("ls-files", "--others", "a"))

    def test_text_raises_outside_the_expected_codes_saying_gits_stderr(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            git = tool_git.Git(tool_process.SubprocessRunner(), cli_harness.isolated_env(os.environ), d)
            with self.assertRaises(tool_git.Failed) as caught:
                git.text("rev-parse", "HEAD")
            self.assertRegex(str(caught.exception), r"^git rev-parse HEAD: fatal: not a git repository")
            self.assertEqual("", git.text("rev-parse", "HEAD", ok=(128,)))
            self.assertEqual("fatal", caught.exception.stderr[:5])


if __name__ == "__main__":
    unittest.main()
