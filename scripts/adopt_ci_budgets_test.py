#!/usr/bin/env python3
"""Unit tests for adopt_ci_budgets.py (scripts/adopt-ci-budgets): a run's figures read from its TSV files, each ratchet
set to its median in the unit the file writes it in, a figure out of its band left unless --out-of-band, ceilings and
operation counts never touched, and a run id fetched through a stub `gh`. The last test runs the wrapper.

    python3 scripts/adopt_ci_budgets_test.py
"""
import io
import os
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import adopt_ci_budgets as adopt  # noqa: E402

MB, KB = 1024 * 1024, 1024

SOURCE = '''object PerformanceBudgets {
  val ListingCold = bytes("listing render, cold caches", measured = mb(20.4))
  val Landing = bytes("landing render", measured = kb(1040))
  val Blob = bytes("listing render, response blob hit", measured = kb(7.0))
  val WholeTick = bytes(
    "whole identity projection tick, nothing moved", measured = mb(36.1))
  val Rebuilds = operations("schedules rebuilt by a repeated render", limit = 0)
  val Giant = ceiling("refusing a giant progressive JPEG", limit = mb(4))
}
'''


def row(name, actual, measured, band=(0.85, 1.15)):
    return f"{name}\t{actual}\t{measured}\t{int(measured * band[0])}\t{int(measured * band[1]) + 1}\n"


class AdoptTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = self.tmp.name
        self.file = os.path.join(self.dir, "PerformanceBudgets.scala")
        with open(self.file, "w") as f:
            f.write(SOURCE)

    def tearDown(self):
        self.tmp.cleanup()

    def tsv(self, rel, *rows):
        path = os.path.join(self.dir, "run", rel, "performance-budgets.tsv")
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "a") as f:
            f.write("".join(rows))
        return path

    def run_main(self, *args):
        out = io.StringIO()
        with redirect_stdout(out):
            code = adopt.main([*args, "--file", self.file])
        with open(self.file) as f:
            return code, out.getvalue(), f.read()

    def test_each_ratchet_takes_its_median_in_its_own_unit(self):
        cold = int(20.4 * MB)
        self.tsv("performance-budgets-page-chrome/target",
                 row("listing render, cold caches", int(21.0 * MB), cold),
                 row("listing render, cold caches", int(21.4 * MB), cold),
                 row("listing render, cold caches", int(20.9 * MB), cold),
                 row("landing render", int(1012.4 * KB), 1040 * KB))
        self.tsv("performance-budgets-e2e-corpus/target",
                 row("whole identity projection tick, nothing moved", int(35.01 * MB), int(36.1 * MB)))
        code, out, src = self.run_main(os.path.join(self.dir, "run"))
        self.assertEqual(code, 0)
        self.assertIn('bytes("listing render, cold caches", measured = mb(21.0))', src)
        self.assertIn('bytes("landing render", measured = kb(1012))', src)
        self.assertIn('"whole identity projection tick, nothing moved", measured = mb(35.0))', src)
        self.assertIn("ADOPTED          listing render, cold caches: mb(20.4) -> mb(21.0)", out)
        self.assertIn("NOT MEASURED     listing render, response blob hit", out)

    def test_small_figures_keep_their_precision(self):
        self.assertEqual(adopt.written(int(1.264 * MB), "mb"), "mb(1.26)")
        self.assertEqual(adopt.written(int(15.56 * MB), "mb"), "mb(15.6)")
        self.assertEqual(adopt.written(int(2.94 * KB), "kb"), "kb(2.9)")
        self.assertEqual(adopt.written(int(663.4 * KB), "kb"), "kb(663)")

    def test_a_figure_out_of_its_band_is_left_unless_adopted_on_purpose(self):
        self.tsv("a", row("landing render", 1300 * KB, 1040 * KB))
        code, out, src = self.run_main(os.path.join(self.dir, "run"))
        self.assertEqual(code, 0)
        self.assertIn("measured = kb(1040)", src)
        self.assertIn("OUT OF BAND      landing render: kb(1040) -> kb(1300) (+25.0%", out)
        code, out, src = self.run_main(os.path.join(self.dir, "run"), "--out-of-band")
        self.assertIn("measured = kb(1300)", src)

    def test_ceilings_and_operation_counts_are_never_touched(self):
        self.tsv("a", "schedules rebuilt by a repeated render\t0\t0\t0\t0\n",
                 f"refusing a giant progressive JPEG\t{MB}\t{4 * MB}\t0\t{4 * MB}\n",
                 "listing render, response blob hit\tnot-a-number\t1\t1\t1\n")
        code, out, src = self.run_main(os.path.join(self.dir, "run"))
        self.assertEqual(code, 1)
        self.assertEqual(src, SOURCE)

    def test_dry_run_writes_nothing(self):
        self.tsv("a", row("landing render", 1000 * KB, 1040 * KB))
        code, out, src = self.run_main(os.path.join(self.dir, "run"), "--dry-run")
        self.assertEqual((code, src), (0, SOURCE))
        self.assertIn("kb(1040) -> kb(1000)", out)

    def test_a_run_id_is_fetched_through_gh(self):
        stub = os.path.join(self.dir, "gh")
        log = os.path.join(self.dir, "calls")
        figures = row("landing render", 1000 * KB, 1040 * KB).replace("\t", "\\t").rstrip("\n")
        with open(stub, "w") as f:
            f.write(f'''#!/usr/bin/env bash
echo "$*" >> {log}
while [ $# -gt 0 ]; do [ "$1" = -D ] && dir=$2; shift; done
mkdir -p "$dir/performance-budgets-unit-worker/web/target"
printf '{figures}\\n' > "$dir/performance-budgets-unit-worker/web/target/performance-budgets.tsv"
''')
        os.chmod(stub, 0o755)
        os.environ["KINOWO_GH"] = stub
        try:
            code, out, src = self.run_main("2428", "--repo", "o/r")
        finally:
            del os.environ["KINOWO_GH"]
        self.assertEqual(code, 0)
        self.assertIn("measured = kb(1000)", src)
        with open(log) as f:
            call = f.read()
        self.assertRegex(call, r"^run download 2428 -p performance-budgets-\* -D \S+ -R o/r$")

    def test_neither_a_run_nor_a_path_is_a_usage_error(self):
        code, _, _ = self.run_main("nope")
        self.assertEqual(code, 2)

    def test_the_wrapper_prints_its_header_and_runs_the_logic(self):
        wrapper = os.path.join(HERE, "adopt-ci-budgets")
        help_text = subprocess.run([wrapper, "--help"], capture_output=True, text=True, check=True).stdout
        self.assertIn("scripts/adopt-ci-budgets <run id>", help_text)
        self.tsv("a", row("landing render", 1000 * KB, 1040 * KB))
        done = subprocess.run([wrapper, os.path.join(self.dir, "run"), "--file", self.file, "--dry-run"],
                              capture_output=True, text=True)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertIn("kb(1040) -> kb(1000)", done.stdout)


if __name__ == "__main__":
    unittest.main()
