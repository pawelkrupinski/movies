#!/usr/bin/env python3
"""tool_cli.py: a tool's `say` prints `<tool>: <line>` on stderr, never on stdout (the tool's output)."""
from __future__ import annotations

import io
import os
import sys
import unittest
from contextlib import redirect_stderr, redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tool_cli  # noqa: E402


class SayTest(unittest.TestCase):
    def test_say_prefixes_the_tool_on_stderr(self) -> None:
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            say = tool_cli.say_as("ci-wait")
            say("run 7 queued")
            say("run 7 green")
        self.assertEqual(("", "ci-wait: run 7 queued\nci-wait: run 7 green\n"), (out.getvalue(), err.getvalue()))


class ParseCallTest(unittest.TestCase):
    def call(self, parse):
        err = io.StringIO()
        with redirect_stderr(err):
            return tool_cli.parse_call("t", "usage\n", parse), err.getvalue()

    def test_a_parsed_call_has_no_exit(self) -> None:
        self.assertEqual(((None, 5), ""), self.call(lambda: 5))

    def test_help_ends_the_run_with_exit_0(self) -> None:
        self.assertEqual(((0, None), ""), self.call(lambda: None))

    def test_a_wrong_call_is_a_usage_failure_exit_2(self) -> None:
        def wrong():
            raise tool_cli.UsageError("--x needs a value")
        self.assertEqual(((2, None), "t: --x needs a value\nt: see t --help\n"), self.call(wrong))

    def test_a_refused_call_says_its_line_and_usage_exit_2(self) -> None:
        def refuse():
            raise tool_cli.Refused("no", with_usage=True)
        self.assertEqual(((2, None), "no\nusage\n"), self.call(refuse))


if __name__ == "__main__":
    unittest.main()
