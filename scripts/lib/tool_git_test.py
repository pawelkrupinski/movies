#!/usr/bin/env python3
"""tool_git.py: git in one repository through the runner -- `out` its stdout or None when git failed, `ok` its exit,
`options` (`-c key=value`) before the repository, a bound passed to the runner."""
from __future__ import annotations

import os
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
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


if __name__ == "__main__":
    unittest.main()
