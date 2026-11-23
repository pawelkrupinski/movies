#!/usr/bin/env python3
"""tool_github.py: every gh call bounded (the runner given the timeout); `read` None for a failed call, never an empty
answer; gh's stderr passed on unless quiet; `download` one asset to its path, False when absent or failed; `asset_names` the listing or None; `fetch_gzipped` a
whole gzip unpacked, False otherwise."""
from __future__ import annotations

import contextlib
import gzip
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

    def test_asset_names_are_the_listed_lines_or_none(self) -> None:
        runner = FakeRunner(0, "a.gz\nb.tsv\n\n")
        self.assertEqual(["a.gz", "b.tsv"], tool_github.GitHub(runner, {}).asset_names("store", timeout=5))
        self.assertEqual((["gh", "release", "view", "store", "--json", "assets", "--jq", ".assets[].name"], 5),
                         (list(runner.commands[0].argv), runner.commands[0].timeout))
        self.assertEqual([], tool_github.GitHub(FakeRunner(0, ""), {}).asset_names("store", timeout=5))
        self.assertIsNone(tool_github.GitHub(FakeRunner(1), {}).asset_names("store", timeout=5, quiet=True))

    def test_fetch_gzipped_unpacks_a_whole_gzip_only(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "result.tsv")

            class Packing(FakeRunner):
                def __init__(self, payload: bytes) -> None:
                    super().__init__(0)
                    self.payload = payload

                def capture(self, command: tool_process.Command) -> tool_process.Captured:
                    argv = list(command.argv)
                    with open(argv[argv.index("-O") + 1], "wb") as f:
                        f.write(self.payload)
                    return tool_process.Captured(0, "", "")

            whole = gzip.compress(b"row\n")
            self.assertTrue(tool_github.GitHub(Packing(whole), {}).fetch_gzipped("store", "r.gz", path, timeout=9))
            self.assertEqual((b"row\n", False), (open(path, "rb").read(), os.path.exists(path + ".gz")))
            self.assertFalse(tool_github.GitHub(Packing(whole[:-6]), {}).fetch_gzipped("store", "r.gz", path, 9),
                             "a truncated gzip is no result, never an EOFError")
            self.assertFalse(tool_github.GitHub(Packing(gzip.compress(b"")), {}).fetch_gzipped("store", "r.gz", path, 9))
            self.assertFalse(tool_github.GitHub(FakeRunner(1), {}).fetch_gzipped("store", "r.gz", path, 9))


if __name__ == "__main__":
    unittest.main()
