#!/usr/bin/env python3
"""identity_boot_cost.py: a phase whose bytes a family rose past the tolerance fails, one within it or falling passes,
a phase missing from the measure fails, time is reported but never held, a moved family count is said, and `update`
rewrites the baseline from the measure keeping its tolerance."""

import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import identity_boot_cost as cost  # noqa: E402


def write(path, text):
    with open(path, "w", encoding="utf-8") as f:
        f.write(text)
    return path


BASELINE = "# comment\ntolerance\t5\nphase\tresolve\t10\t1000000\t2000000\nphase\tboot\t10\t2000000\t3000000\n" \
           "phase\tboot-steady\t10\t100000\t50000\n"


def rows(resolve=10_000_000, boot=20_000_000, steady=1_000_000, boot_families=10, nanos=20_000_000, drop=()):
    lines = ["boot\tpl-a\t4\t12000000\t9000000", "boot\tpl-b\t6\t8000000\t11000000"]
    for phase, total, families in (("resolve", resolve, 10), ("boot", boot, boot_families), ("boot-steady", steady, 10)):
        if phase not in drop:
            lines.append(f"{phase}\t*\t{families}\t{total}\t{nanos}")
    return "\n".join(lines) + "\n"


class CheckTest(unittest.TestCase):
    def run_check(self, current):
        with tempfile.TemporaryDirectory() as d:
            totals, slices = cost.read_rows(write(os.path.join(d, "rows.tsv"), current))
            tolerance, baseline = cost.read_baseline(write(os.path.join(d, "base.tsv"), BASELINE))
            return cost.compare(totals, slices, tolerance, baseline)

    def test_the_measure_as_recorded_passes(self):
        report, failed = self.run_check(rows())
        self.assertFalse(failed)
        self.assertIn("| boot | 10 | 1.91 MB | 1.91 MB | +0.0% | 2.00 ms | ok |", report)

    def test_a_rise_past_the_tolerance_fails_and_one_within_it_passes(self):
        report, failed = self.run_check(rows(boot=21_200_000))
        self.assertTrue(failed)
        self.assertTrue(any("| boot |" in line and "+6.0%" in line and "ROSE" in line for line in report))
        self.assertFalse(self.run_check(rows(boot=20_900_000))[1])

    def test_a_fall_passes_and_asks_for_an_update(self):
        report, failed = self.run_check(rows(resolve=8_000_000))
        self.assertFalse(failed)
        self.assertTrue(any("| resolve |" in line and "fell" in line for line in report))

    def test_time_is_never_held(self):
        self.assertFalse(self.run_check(rows(nanos=900_000_000))[1])

    def test_a_missing_phase_fails_and_a_moved_family_count_is_said(self):
        self.assertTrue(self.run_check(rows(drop=("boot-steady",)))[1])
        report, _ = self.run_check(rows(boot=24_000_000, boot_families=12))
        self.assertTrue(any("| boot | 12 (baseline 10) |" in line for line in report))

    def test_the_heaviest_boot_slices_are_named(self):
        report, _ = self.run_check(rows())
        self.assertIn("- pl-a: 2.86 MB over 4 families, 2.25 ms each", report)


class UpdateTest(unittest.TestCase):
    def test_update_rewrites_the_rows_and_keeps_the_tolerance(self):
        with tempfile.TemporaryDirectory() as d:
            current = write(os.path.join(d, "rows.tsv"), rows(boot=30_000_000))
            base = write(os.path.join(d, "base.tsv"), BASELINE)
            self.assertEqual(cost.main(["update", "--current", current, "--baseline", base]), 0)
            tolerance, phases = cost.read_baseline(base)
            self.assertEqual(tolerance, 5.0)
            self.assertEqual(phases["boot"], (10, 3_000_000, 2_000_000))
            self.assertEqual(cost.main(["check", "--current", current, "--baseline", base]), 0)


if __name__ == "__main__":
    unittest.main()
