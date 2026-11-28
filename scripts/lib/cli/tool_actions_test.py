#!/usr/bin/env python3
"""tool_actions.py: annotations and groups as workflow-command lines on stdout; an output and a summary appended to
$GITHUB_OUTPUT / $GITHUB_STEP_SUMMARY, and nowhere when those are unset or empty."""
from __future__ import annotations

import io
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from cli import tool_actions  # noqa: E402


class ActionsTest(unittest.TestCase):
    def test_workflow_commands(self) -> None:
        out = io.StringIO()
        actions = tool_actions.Actions({}, out)
        actions.annotate("error", "Order verdict (swift)", "REGRESSION")
        actions.group("control pass")
        actions.end_group()
        self.assertEqual("::error title=Order verdict (swift)::REGRESSION\n::group::control pass\n::endgroup::\n",
                         out.getvalue())

    def test_outputs_and_summaries_append_and_go_nowhere_off_actions(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            output, summary = os.path.join(d, "output"), os.path.join(d, "summary")
            actions = tool_actions.Actions({"GITHUB_OUTPUT": output, "GITHUB_STEP_SUMMARY": summary}, io.StringIO())
            actions.output("verdict", "pass")
            actions.output("seed", "42")
            actions.summary("### one\n")
            tool_actions.Actions({"GITHUB_ENV": os.path.join(d, "env")}, io.StringIO()).export("RUN", "42")
            with open(os.path.join(d, "env")) as f:
                self.assertEqual("RUN=42\n", f.read())
            with open(output) as f:
                self.assertEqual("verdict=pass\nseed=42\n", f.read())
            with open(summary) as f:
                self.assertEqual("### one\n", f.read())
            tool_actions.Actions({"GITHUB_OUTPUT": ""}, io.StringIO()).output("x", "y")
            self.assertEqual(["env", "output", "summary"], sorted(os.listdir(d)))


if __name__ == "__main__":
    unittest.main()
