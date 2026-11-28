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


class UpdateRisesTest(unittest.TestCase):
    """`update` records a rise like a fall, but says every rise (row, old, new) and fails: the push would refuse it."""

    def update(self, old: str, current: dict) -> tuple:
        with tempfile.TemporaryDirectory() as work:
            path = os.path.join(work, "baseline.tsv")
            with open(path, "w") as f:
                f.write(old)
            err = io.StringIO()
            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(err):
                ok = baseline_ratchet.update_saying_rises(spec(path), current)
            with open(path) as f:
                return ok, err.getvalue(), f.read()

    OLD = "limit\tcognitive\t10\ncognitive\ta.py:f\t12\n"

    def test_a_raised_row_is_said_and_update_fails(self) -> None:
        ok, err, written = self.update(self.OLD, {("limit", "cognitive"): 10.0, ("cognitive", "a.py:f"): 14.0})
        self.assertFalse(ok)
        self.assertIn("cognitive a.py:f raised 12 -> 14", err)
        self.assertIn("cognitive\ta.py:f\t14", written)

    def test_a_new_row_past_the_limit_is_a_rise(self) -> None:
        ok, err, _ = self.update(self.OLD, {("limit", "cognitive"): 10.0, ("cognitive", "a.py:f"): 12.0,
                                            ("cognitive", "b.py:g"): 11.0})
        self.assertFalse(ok)
        self.assertIn("cognitive b.py:g raised <=10 -> 11", err)

    def test_a_fall_is_recorded_quietly_and_succeeds(self) -> None:
        ok, err, written = self.update(self.OLD, {("limit", "cognitive"): 10.0, ("cognitive", "a.py:f"): 11.0})
        self.assertTrue(ok)
        self.assertEqual("", err)
        self.assertIn("cognitive\ta.py:f\t11", written)


if __name__ == "__main__":
    unittest.main()
