#!/usr/bin/env python3
"""scripts/coverage-touched, black-box (cli_harness), over a scratch repository and the stub sbt
(scripts/touched-tree-stub.sh; touched_tree_fixture.py): in a worktree of its own at the head, it turns coverage on for
the touched methods' module only, runs only the reaching specs, reads the module's scoverage report and lists each
touched method's statement coverage, the uncovered first; a failing spec still reports, a range touching no production
method says so, a failed coverageReport fails, the usage errors exit 2, and the worktree goes afterwards -- also when
an INT or TERM ends the run. Run: python3 scripts/coverage_touched_test.py"""
from __future__ import annotations

import os
import signal
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from touched_tree_fixture import SOURCE, TouchedTreeCase  # noqa: E402


def section(lines: List[str], start: str, end: str) -> List[str]:
    """The lines from the one starting `start` to the one starting `end` (sed -n '/^start/,/^end/p')."""
    begin = next(i for i, line in enumerate(lines) if line.startswith(start))
    rest = lines[begin + 1:]
    stop = next((i for i, line in enumerate(rest) if line.startswith(end)), len(rest))
    return lines[begin:begin + 2 + stop]


class CoverageTouchedTest(TouchedTreeCase):
    NAME = "coverage-touched"
    PYTHON = "coverage_touched.py"
    EDITS = (("_.trim", "_.trim.toLowerCase"), ("genre.capitalize", "genre.trim.capitalize"))

    def setUp(self) -> None:
        super().setUp()
        self.write("specs.txt", "scrapers services.cinemas.GenresSpec\nworker services.WorkerGenresSpec\n")
        self.write("statements", "5 3\n5 1\n7 0\n")

    def test_a_change_lists_each_touched_method_uncovered_first(self) -> None:
        done = self.run_in_repo("main~2..main~1", "--out", self.path("keep"),
                                env={"STUB_STATEMENTS": self.path("statements")})
        self.assertEqual(0, done.code, done.output)
        lines = done.output.splitlines()
        below = section(lines, "## below the bar", "## covered")
        self.assertEqual(1, sum(f"0.0% (0/1)  {SOURCE}:Genres.label" in line for line in below), done.output)
        covered = section(lines, "## covered", "\0")
        self.assertEqual(1, sum(f"100.0% (2/2)  {SOURCE}:Genres.parse" in line for line in covered), done.output)

    def test_coverage_on_for_the_touched_module_only_and_only_the_reaching_specs_run(self) -> None:
        done = self.run_in_repo("main~2..main~1", env={"STUB_STATEMENTS": self.path("statements")})
        self.assertEqual(0, done.code, done.output)
        tests = [line for line in self.stub_calls() if "/testOnly" in line]
        words = [word for line in tests for word in line.split("|")]
        self.assertEqual(1, words.count("set scrapers/coverageEnabled := true"), tests)
        self.assertEqual([], [w for w in words if w.startswith("set worker/coverageEnabled")], tests)
        self.assertIn("scrapers/testOnly services.cinemas.GenresSpec", words)
        self.assertIn("worker/testOnly services.WorkerGenresSpec", words)
        reports = [c for c in self.stub_calls() if c.endswith("|set scrapers/coverageEnabled := true|scrapers/coverageReport")]
        self.assertEqual(1, len(reports), self.stub_calls())

    def test_a_worktree_of_its_own_removed_and_the_report_kept_under_out(self) -> None:
        done = self.run_in_repo("main~2..main~1", "--out", self.path("keep"),
                                env={"STUB_STATEMENTS": self.path("statements")})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.trees_left())
        for kept in ("report.md", "scoverage-scrapers.xml"):
            self.assertTrue(os.path.isfile(self.path("keep", kept)), kept)
        self.assertIn("coverage-touched: 2 touched method(s) in scrapers - 2 spec(s) reach them", done.output)
        self.assertEqual([], os.listdir(self.path("tmp")))

    def test_a_failing_reaching_spec_still_reports(self) -> None:
        done = self.run_in_repo("main~2..main~1", env={"STUB_STATEMENTS": self.path("statements"), "STUB_TEST_RC": "1"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, done.output.count("Some reaching specs FAILED"), done.output)

    def test_below_moves_the_bar(self) -> None:
        done = self.run_in_repo("main~2..main~1", "--below", "40", env={"STUB_STATEMENTS": self.path("statements")})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("1 below 40% statement coverage", done.output)

    def test_no_spec_reaches_the_methods(self) -> None:
        self.write("specs.txt", "")
        done = self.run_in_repo("main~2..main~1", env={"STUB_STATEMENTS": self.path("statements")})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("No spec reaches the touched methods: every one is unpinned.", done.output)
        self.assertEqual([], [line for line in self.stub_calls() if "/testOnly" in line])

    def test_a_range_touching_no_production_method(self) -> None:
        done = self.run_in_repo("main~1..main")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("coverage-touched: the range touches no production method", done.output)

    def test_a_failed_coverage_report_exits_1_naming_it(self) -> None:
        done = self.run_in_repo("main~2..main~1", env={"STUB_STATEMENTS": self.path("statements"),
                                                        "STUB_FAIL_ON": "coverageReport"})
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, sum(line.startswith("coverage-touched: coverageReport failed")
                                for line in done.output.splitlines()), done.output)
        self.assertEqual([], self.trees_left())

    def test_usage_errors_exit_2(self) -> None:
        for args in (("main~2..main~1", "--below", "lots"), ("main~2",), (), ("main~2..main~1", "--bogus"),
                     ("main~2..main~1", "main~1..main"), ("main~2..main~1", "--below"), ("nothere..main",),
                     ("..main",)):
            done = self.run_in_repo(*args)
            self.assertEqual(2, done.code, (args, done.output))
            self.assertIn("coverage-touched: see coverage-touched --help", done.stderr, args)
        self.assertEqual([], self.trees_left())

    def test_help_prints_the_header(self) -> None:
        self.assert_help("COVERAGE OF THE METHODS A CHANGE TOUCHES (scoverage; a report, never a gate): which touched "
                         "production methods the")

    def test_term_cleans_up(self) -> None:
        self.assert_signal_cleans_up(signal.SIGTERM, "/testOnly", "main~2..main~1")

    def test_int_cleans_up(self) -> None:
        self.assert_signal_cleans_up(signal.SIGINT, "touchedSpecs", "main~2..main~1")


if __name__ == "__main__":
    unittest.main()
