#!/usr/bin/env python3
"""tool_github.py: every gh call bounded (the runner given the timeout); `read` None for a failed call, never an empty
answer; gh's stderr passed on unless quiet; `download` one asset to its path, False when absent or failed."""
from __future__ import annotations

import contextlib
import io
import os
import sys
import tempfile
import unittest
from typing import List

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tool_github  # noqa: E402
import tool_process  # noqa: E402


class FakeRunner:
    """Answers each gh call with `code`, writing the downloaded file when asked to succeed."""

    def __init__(self, code: int, stdout: str = "", stderr: str = "") -> None:
        self.code, self.stdout, self.stderr = code, stdout, stderr
        self.commands: List[tool_process.Command] = []

    def run(self, command: tool_process.Command) -> int:
        return self.capture(command).code

    def capture(self, command: tool_process.Command) -> tool_process.Captured:
        self.commands.append(command)
        argv = list(command.argv)
        if self.code == 0 and "-O" in argv:
            open(argv[argv.index("-O") + 1], "w").close()
        return tool_process.Captured(self.code, self.stdout, self.stderr)


class GitHubTest(unittest.TestCase):
    def test_read_answers_or_none_and_is_bounded(self) -> None:
        runner = FakeRunner(0, "abc\n")
        self.assertEqual("abc\n", tool_github.GitHub(runner, {}).read(["api", "x"], timeout=7))
        self.assertEqual((["gh", "api", "x"], 7), (list(runner.commands[0].argv), runner.commands[0].timeout))
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            self.assertIsNone(tool_github.GitHub(FakeRunner(1, "", "HTTP 502\n"), {}).read(["api", "x"], timeout=7))
        self.assertEqual("HTTP 502\n", err.getvalue())
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            self.assertFalse(tool_github.GitHub(FakeRunner(1, "", "HTTP 502\n"), {}).succeeds(["api", "x"], timeout=7))
        self.assertEqual("", err.getvalue(), "succeeds is quiet by default")

    def test_download_says_whether_the_asset_is_there(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            dest = os.path.join(d, "sub", "a.gz")
            runner = FakeRunner(0)
            self.assertTrue(tool_github.GitHub(runner, {}).download("store", "a.gz", dest, timeout=9))
            self.assertEqual(["gh", "release", "download", "store", "-p", "a.gz", "-O", dest, "--clobber"],
                             list(runner.commands[0].argv))
            self.assertFalse(tool_github.GitHub(FakeRunner(1), {}).download("store", "b.gz", os.path.join(d, "b"), 9))


if __name__ == "__main__":
    unittest.main()
