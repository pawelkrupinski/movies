#!/usr/bin/env python3
"""flake_verdict.py over hand-made JUnit in both shapes it reads: ScalaTest's (one suite per
file, machine name in `hostname`) and Playwright's (one file, the project in `hostname`, a test
repeated once per --repeat-each). Run: python3 scripts/ci/flake_verdict_test.py"""
import io
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import flake_verdict  # noqa: E402


def suite(cases, hostname="runner-1", name="S"):
    body = "".join(
        f'<testcase classname="{c}" name="{n}">{"<failure/>" if bad else ""}{"<skipped/>" if bad is None else ""}</testcase>'
        for c, n, bad in cases)
    return f'<testsuite name="{name}" hostname="{hostname}">{body}</testsuite>'


class FlakeVerdictTest(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())
        os.environ.pop("GITHUB_STEP_SUMMARY", None)
        flake_verdict.BY_PROJECT = False

    def write(self, rel, xml):
        p = self.dir / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(xml)
        return str(p)

    def verdict(self, *extra):
        out = io.StringIO()
        flaky = self.dir / "flaky.tsv"
        with redirect_stdout(out):
            flake_verdict.main(["x", *extra, "verdict", "--label", "unit tests",
                                "--original", str(self.dir / "orig"), "--rerun", str(self.dir / "rerun"),
                                "--flaky-out", str(flaky)])
        return out.getvalue(), (flaky.read_text() if flaky.exists() else "")

    def test_a_test_that_passes_any_rerun_is_flaky_and_one_that_never_does_is_deterministic(self):
        self.write("orig/TEST-A.xml", suite([("A", "flaky one", True), ("A", "broken one", True), ("A", "fine", False)]))
        for k, flaky_passes in enumerate([False, True, True]):
            self.write(f"rerun/1-{k}.xml", suite([("A", "flaky one", not flaky_passes)]))
            self.write(f"rerun/2-{k}.xml", suite([("A", "broken one", True)]))
        table, ledger = self.verdict()
        self.assertIn("| **FLAKY** | A › flaky one | 2/3 |", table)
        self.assertIn("| **DETERMINISTIC** | A › broken one | 0/3 |", table)
        self.assertNotIn("fine", table)
        self.assertEqual(ledger, "unit tests\tA\tflaky one\t2/3\n")

    def test_a_failed_test_with_no_rerun_says_so_rather_than_guessing(self):
        self.write("orig/TEST-A.xml", suite([("A", "t", True)]))
        (self.dir / "rerun").mkdir()
        table, ledger = self.verdict()
        self.assertIn("| **NOT RERUN** | A › t | 0/0 |", table)
        self.assertEqual(ledger, "")

    def test_playwright_repeats_in_one_file_count_per_project(self):
        self.write("orig/junit-1.xml", "<testsuites>" + suite([("card.spec.ts", "taps", True)], "webkit-iphone-se")
                   + suite([("card.spec.ts", "taps", False)], "webkit-iphone-13") + "</testsuites>")
        self.write("rerun/rerun-1.xml", "<testsuites>" + suite(
            [("card.spec.ts", "taps", True), ("card.spec.ts", "taps", False), ("card.spec.ts", "taps", True)],
            "webkit-iphone-se") + "</testsuites>")
        table, ledger = self.verdict("--by-project")
        self.assertIn("| **FLAKY** | webkit-iphone-se › card.spec.ts › taps | 1/3 |", table)
        self.assertEqual(ledger, "unit tests\twebkit-iphone-se card.spec.ts\ttaps\t1/3\n")

    def test_skipped_tests_are_not_runs(self):
        self.write("orig/TEST-A.xml", suite([("A", "t", True)]))
        self.write("rerun/1.xml", suite([("A", "t", True), ("A", "t", None)]))
        table, _ = self.verdict()
        self.assertIn("| **DETERMINISTIC** | A › t | 0/1 |", table)

    def test_failed_lists_each_failure_once_with_an_empty_project_kept(self):
        self.write("orig/TEST-A.xml", suite([("A", "t", True), ("A", "t", True), ("A", "u", False)]))
        out = io.StringIO()
        with redirect_stdout(out):
            flake_verdict.main(["x", "failed", str(self.dir / "orig")])
        self.assertEqual(out.getvalue(), "\x1fA\x1ft\n")

    def test_the_verdict_table_reaches_the_job_summary(self):
        summary = self.dir / "summary.md"
        os.environ["GITHUB_STEP_SUMMARY"] = str(summary)
        self.write("orig/TEST-A.xml", suite([("A", "t", True)]))
        self.write("rerun/1.xml", suite([("A", "t", False)]))
        self.verdict()
        self.assertIn("**FLAKY**", summary.read_text())
        self.assertIn("a rerun is never a fix", summary.read_text())


if __name__ == "__main__":
    unittest.main()
