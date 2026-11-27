#!/usr/bin/env python3
"""baseline_ratchet.check: a checkout with no baseline file is a clean message and a failed check, never a traceback
(agentctl calls the engine's check directly)."""
from __future__ import annotations

import contextlib
import io
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import baseline_ratchet  # noqa: E402


def spec(baseline: str) -> baseline_ratchet.Spec:
    return baseline_ratchet.Spec(
        title="DEMO", tool="demo", baseline=baseline, header="# demo", hint="fix it", update_command="demo --update",
        held_rows="rows", trailers={"Complexity": ("cognitive",)}, held=lambda _path: True)


class NoBaselineTest(unittest.TestCase):
    def test_check_without_a_baseline_says_so_and_fails(self) -> None:
        with tempfile.TemporaryDirectory() as work:
            err = io.StringIO()
            with contextlib.redirect_stderr(err):
                ok = baseline_ratchet.check(spec(os.path.join(work, "nowhere.tsv")), {("limit", "cognitive"): 10.0}, [])
        self.assertFalse(ok)
        self.assertIn("demo: no baseline", err.getvalue())
        self.assertIn("demo --update", err.getvalue())


if __name__ == "__main__":
    unittest.main()
