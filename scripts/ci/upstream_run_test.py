#!/usr/bin/env python3
"""upstream_run.py: the producing run a workflow_run-triggered workflow reads when its trigger succeeded, failed, ran
off main, was dispatched or is absent -- and the failure when no run is fresh.
Run: python3 scripts/ci/upstream_run_test.py"""
import contextlib
import datetime
import io
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import upstream_run as pick  # noqa: E402

NOW = datetime.datetime(2026, 10, 25, 4, 0, tzinfo=datetime.timezone.utc)
RUNS = [(2144, NOW - datetime.timedelta(hours=30)), (2000, NOW - datetime.timedelta(hours=54))]
W = ["--workflow", "Record scrape fixtures"]


def run(argv, runs=RUNS):
    out, err = io.StringIO(), io.StringIO()
    asked = []

    def listing():
        asked.append(True)
        return runs
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        code = pick.main(W + argv, now=NOW, listing=listing)
    return code, out.getvalue().strip(), err.getvalue(), bool(asked)


class UpstreamRunTest(unittest.TestCase):
    def test_a_run_that_succeeded_on_main_is_read_without_listing(self):
        code, out, _, asked = run(["--trigger", "2507", "--conclusion", "success", "--branch", "main"])
        self.assertEqual((code, out, asked), (0, "2507", False))

    def test_a_failed_run_is_passed_over_for_the_newest_successful_one(self):
        code, out, err, _ = run(["--trigger", "2507", "--conclusion", "failure", "--branch", "main"])
        self.assertEqual((code, out), (0, "2144"))
        self.assertIn("::warning::Record scrape fixtures run 2507 ended failure: "
                      "reading the newest successful run on main instead, 2144", err)

    def test_a_run_off_main_is_passed_over_too(self):
        code, out, err, _ = run(["--trigger", "2507", "--conclusion", "success", "--branch", "measure/x"])
        self.assertEqual((code, out), (0, "2144"))
        self.assertIn("ran off main (measure/x)", err)

    def test_no_successful_run_within_48_hours_fails_visibly(self):
        code, out, err, _ = run(["--trigger", "2507", "--conclusion", "failure"], runs=RUNS[1:])
        self.assertEqual((code, out), (1, ""))
        self.assertIn("::error::Record scrape fixtures run 2507 ended failure, and no successful run on main is newer "
                      "than 48 h; the newest successful one is 2000", err)
        self.assertEqual(run(["--trigger", "2507", "--conclusion", "cancelled"], runs=[])[0], 1)

    def test_a_dispatched_run_reads_the_run_it_names_whatever_its_age(self):
        code, out, _, asked = run(["--trigger", "1476"])
        self.assertEqual((code, out, asked), (0, "1476", False))

    def test_a_run_id_must_be_a_number(self):
        self.assertEqual(run(["--trigger", "latest"])[0], 1)

    def test_no_run_named_takes_the_newest_successful_one(self):
        code, out, err, asked = run([])
        self.assertEqual((code, out, asked), (0, "2144", True))
        self.assertIn("::warning::no Record scrape fixtures run named: reading the newest successful run on main instead, 2144", err)
        self.assertEqual(run([], runs=[])[0], 1)

    def test_no_age_limit_takes_a_run_older_than_48_hours(self):
        # the findings report (run 3139 after recording 3136 failed): an older recording's findings are still the newest
        self.assertEqual(run(["--trigger", "3136", "--conclusion", "failure", "--max-age-hours", "0"], runs=RUNS[1:])[:2],
                         (0, "2000"))
        code, _, err, _ = run(["--trigger", "3136", "--conclusion", "failure", "--max-age-hours", "0"], runs=[])
        self.assertEqual(code, 1)
        self.assertIn("no successful run on main; there is no successful one", err)

    def test_skip_passes_a_failed_run_over_for_nothing_but_still_reads_a_good_one(self):
        code, out, err, asked = run(["--trigger", "3136", "--conclusion", "failure", "--on-fallback", "skip"])
        self.assertEqual((code, out, asked), (0, "", False))
        self.assertIn("::notice::Record scrape fixtures run 3136 ended failure: nothing to read", err)
        self.assertEqual(run(["--trigger", "3137", "--conclusion", "success", "--on-fallback", "skip"])[:2], (0, "3137"))
        self.assertEqual(run(["--trigger", "1476", "--on-fallback", "skip"])[:2], (0, "1476"))

    def test_the_newest_is_taken_by_creation_time_not_listing_order(self):
        self.assertEqual(pick.choose("9", "failure", "main", sorted(RUNS, key=lambda r: r[1]), NOW)[0], 2144)


if __name__ == "__main__":
    unittest.main()
