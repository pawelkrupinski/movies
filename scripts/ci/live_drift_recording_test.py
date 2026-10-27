#!/usr/bin/env python3
"""live_drift_recording.py: the recording the live drift check compares (and the findings report reports) when its trigger succeeded, failed, ran off main or
was dispatched -- and the failure when no recording is fresh.
Run: python3 scripts/ci/live_drift_recording_test.py"""
import contextlib
import datetime
import io
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import live_drift_recording as pick  # noqa: E402

NOW = datetime.datetime(2026, 10, 25, 4, 0, tzinfo=datetime.timezone.utc)
RECORDINGS = [(2144, NOW - datetime.timedelta(hours=30)), (2000, NOW - datetime.timedelta(hours=54))]


def run(argv, recordings=RECORDINGS):
    out, err = io.StringIO(), io.StringIO()
    asked = []

    def listing():
        asked.append(True)
        return recordings
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        code = pick.main(argv, now=NOW, listing=listing)
    return code, out.getvalue().strip(), err.getvalue(), bool(asked)


class LiveDriftRecordingTest(unittest.TestCase):
    def test_a_recording_that_succeeded_on_main_is_compared_without_listing(self):
        self.assertEqual(run(["--trigger", "2507", "--conclusion", "success", "--branch", "main"])[:2], (0, "2507"))
        self.assertFalse(run(["--trigger", "2507", "--conclusion", "success", "--branch", "main"])[3])

    def test_a_failed_recording_is_passed_over_for_the_newest_successful_one(self):
        code, out, err, _ = run(["--trigger", "2507", "--conclusion", "failure", "--branch", "main"])
        self.assertEqual((code, out), (0, "2144"))
        self.assertIn("::warning::recording 2507 ended failure: comparing the newest successful recording on main instead, 2144", err)

    def test_a_recording_off_main_is_passed_over_too(self):
        code, out, err, _ = run(["--trigger", "2507", "--conclusion", "success", "--branch", "measure/x"])
        self.assertEqual((code, out), (0, "2144"))
        self.assertIn("ran off main (measure/x)", err)

    def test_no_successful_recording_within_48_hours_fails_visibly(self):
        code, out, err, _ = run(["--trigger", "2507", "--conclusion", "failure"], recordings=RECORDINGS[1:])
        self.assertEqual((code, out), (1, ""))
        self.assertIn("::error::recording 2507 ended failure, and no successful recording on main is newer than 48 h; "
                      "the newest successful one is 2000", err)
        self.assertEqual(run(["--trigger", "2507", "--conclusion", "cancelled"], recordings=[])[0], 1)

    def test_a_dispatched_run_compares_the_recording_it_names_whatever_its_age(self):
        code, out, _, asked = run(["--trigger", "1476"])
        self.assertEqual((code, out, asked), (0, "1476", False))

    def test_a_run_id_must_be_a_number(self):
        self.assertEqual(run(["--trigger", "latest"])[0], 1)

    def test_no_recording_named_takes_the_newest_successful_one(self):
        code, out, err, asked = run([])
        self.assertEqual((code, out, asked), (0, "2144", True))
        self.assertIn("::warning::no recording named: comparing the newest successful recording on main instead, 2144", err)
        self.assertEqual(run([], recordings=[])[0], 1)

    def test_no_age_limit_takes_a_recording_older_than_48_hours(self):
        # the findings report (run 3139 after recording 3136 failed): an older recording's findings are still the newest
        self.assertEqual(run(["--trigger", "3136", "--conclusion", "failure", "--max-age-hours", "0"],
                             recordings=RECORDINGS[1:])[:2], (0, "2000"))
        code, _, err, _ = run(["--trigger", "3136", "--conclusion", "failure", "--max-age-hours", "0"], recordings=[])
        self.assertEqual(code, 1)
        self.assertIn("no successful recording on main; there is no successful one", err)

    def test_the_newest_is_taken_by_creation_time_not_listing_order(self):
        self.assertEqual(pick.choose("9", "failure", "main", sorted(RECORDINGS, key=lambda r: r[1]), NOW)[0], 2144)


if __name__ == "__main__":
    unittest.main()
