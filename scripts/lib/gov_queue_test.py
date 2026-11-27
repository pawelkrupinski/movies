#!/usr/bin/env python3
"""gov_queue.py: a waiting run's place in the governor's queue and its usual wait, read from `gov status` and `gov stats`."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gov_queue  # noqa: E402

STATUS = """governor: 5 admitted, 15.0/77 GB committed, 15/18 cores charged, 4 GB still growing; free 60.7 GB
  agent    2.0 GB    5 cores  script-tests run-script-tests  pid 81397  since 2026-11-27T09:57:04Z  /w
  WAITING agent    2.0 GB    8 cores  run-script-tests  pid 88751
  WAITING agent    6.0 GB    2 cores  bitcashier-core-wt-ru15  pid 98267
  WAITING agent    0.0 GB  3.5 cores  sbt-warm job  pid 21801
pressure: none
"""
STATS = """check slots: cap 3
  class admits  p50 wait  p90 wait waited>1m
  check    612        0s        3s        22
  agent   4751        0s        7s       208
"""


class DescribeTest(unittest.TestCase):
    def test_the_place_is_the_pids_among_the_waiting(self) -> None:
        self.assertEqual((1, 3), gov_queue.position(STATUS, 88751))
        self.assertEqual((3, 3), gov_queue.position(STATUS, 21801))
        self.assertIsNone(gov_queue.position(STATUS, 81397))  # admitted: no longer waiting

    def test_the_line_names_place_load_and_the_usual_wait(self) -> None:
        line = gov_queue.describe("run-script-tests", STATUS, STATS, 98267, "agent")
        self.assertEqual("run-script-tests: queued at the governor, 2nd of 3 waiting (5 admitted, 15/18 cores charged); "
                         "the agent class's admissions wait 0s at the median, 7s at the 90th percentile; "
                         "ending it now loses its place", line)

    def test_no_line_for_a_run_the_governor_does_not_list(self) -> None:
        self.assertIsNone(gov_queue.describe("run-script-tests", STATUS, STATS, 1, "agent"))

    def test_a_class_without_stats_still_gets_its_place(self) -> None:
        line = gov_queue.describe("run-script-tests", STATUS, "", 88751, "measure")
        self.assertIsNotNone(line)
        self.assertNotIn("median", line or "")

    def test_ordinals(self) -> None:
        self.assertEqual(["1st", "2nd", "3rd", "4th", "11th", "12th", "21st"],
                         [gov_queue.ordinal(n) for n in (1, 2, 3, 4, 11, 12, 21)])


class ReportOnceTest(unittest.TestCase):
    def test_a_line_is_said_once_until_the_place_changes(self) -> None:
        said: List[str] = []
        answers = {"status": STATUS, "stats": STATS}
        report = gov_queue.QueueReport("t", lambda args: answers[args[0]], 88751, "agent", said.append, 1)
        last: Optional[str] = report.report_once(None)
        last = report.report_once(last)
        self.assertEqual(1, len(said))
        answers["status"] = STATUS.replace("  WAITING agent    2.0 GB    8 cores  run-script-tests  pid 88751\n", "")
        report.report_once(last)
        self.assertEqual(1, len(said), "admitted or unlisted: nothing more is said")

    def test_an_unreadable_governor_says_nothing(self) -> None:
        said: List[str] = []
        gov_queue.QueueReport("t", lambda args: None, 1, "agent", said.append, 1).report_once(None)
        self.assertEqual([], said)


if __name__ == "__main__":
    unittest.main()
