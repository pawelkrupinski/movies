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


if __name__ == "__main__":
    unittest.main()
