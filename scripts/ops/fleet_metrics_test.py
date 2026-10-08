#!/usr/bin/env python3
"""fleet_metrics.py's reading of -Xlog:gc lines, take-up log lines and per-boot CPU series — no
network. Run: python3 scripts/ops/fleet_metrics_test.py"""
import math
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import fleet_metrics as fm  # noqa: E402


class GcTest(unittest.TestCase):
    def test_a_pause_line_is_its_kind_uptime_cause_sizes_and_pause(self):
        self.assertEqual(fm.gc_event("[2026-10-04T10:00:00.000+0000][612.345s][info][gc] GC(41) Pause Full (Allocation Failure) 900M->412M(1024M) 812.5ms"),
                         ("Full", 612.345, "Allocation Failure", 900, 412, 812.5))
        self.assertEqual(fm.gc_event("[12.0s][info][gc] GC(3) Pause Young (Allocation Failure) 300M->120M(1024M) 9.1ms"),
                         ("Young", 12.0, "Allocation Failure", 300, 120, 9.1))
        self.assertIsNone(fm.gc_event("[12.0s][info][gc,heap] Heap region size: 1M"))

    def test_boot_and_steady_state_are_summarised_apart(self):
        events = [("Full", 100.0, "Ergonomics", 900, 400, 500.0), ("Full", 700.0, "Allocation Failure", 900, 300, 1500.0),
                  ("Young", 30.0, "Allocation Failure", 300, 120, 10.0)]
        self.assertEqual(fm.gc_summary(events), [
            "boot (<10 min): 1 full GCs, live after: min 400 median 400 p90 400 max 400 MB; pause total 0s; causes {'Ergonomics': 1}",
            "boot (<10 min): 1 young GCs, heap after: median 120 p90 120 max 120 MB; pause total 0s",
            "steady (>=10 min): 1 full GCs, live after: min 300 median 300 p90 300 max 300 MB; pause total 2s; causes {'Allocation Failure': 1}",
        ])


class BootTest(unittest.TestCase):
    def test_a_take_up_line_names_the_families_it_re_resolved(self):
        self.assertEqual(fm.take_up_line("identity model: taken up in 41 s, 1204 re-resolved, 3 regions"), 1204)
        self.assertIsNone(fm.take_up_line("identity model: loaded"))

    def test_a_boot_row_reads_cpu_at_uptimes_its_peak_rate_and_its_take_up(self):
        b = 1_000_000.0
        ts = [b + 15 * i for i in range(0, 130)]                      # 15 s samples, ~32 min of uptime
        cpu = {t: (t - b) * (2.0 if t - b <= 60 else 1.0) for t in ts}  # 2 cores for the first minute, then 1
        started = {t: b for t in ts}
        rows = fm.boot_rows(cpu, started, jit={t: 7.0 for t in ts}, gc={t: 3.0 for t in ts}, full_gcs={t: 1.0 for t in ts},
                            takeups=[(b + 120, 1204), (b + 5000, 9)], since=b)
        self.assertEqual(len(rows), 1)
        boot, rr, c180, c300, c600, cc1030, peak, j300, g300, f360 = rows[0]
        self.assertEqual((boot, rr, c180, c300, c600, j300, g300, f360), (b, 1204, 180.0, 300.0, 600.0, 7.0, 3.0, 1.0))
        self.assertAlmostEqual(cc1030, (1800 - 600) / 12)
        self.assertAlmostEqual(peak, 2.0)

    def test_a_boot_with_too_few_samples_or_before_the_window_is_left_out(self):
        b = 1_000_000.0
        few = {b + 15 * i: float(i) for i in range(3)}
        self.assertEqual(fm.boot_rows(few, {t: b for t in few}, {}, {}, {}, [], since=b), [])
        many = {b + 15 * i: float(i) for i in range(10)}
        self.assertEqual(fm.boot_rows(many, {t: b for t in many}, {}, {}, {}, [], since=b + 3600), [])

    def test_an_uptime_a_boot_never_reached_is_nan(self):
        b = 1_000_000.0
        cpu = {b + 15 * i: float(i) for i in range(10)}
        row = fm.boot_rows(cpu, {t: b for t in cpu}, {}, {}, {}, [], since=b)[0]
        self.assertTrue(math.isnan(row[4]))   # cpu@600 on a boot sampled for 135 s


class TimeTest(unittest.TestCase):
    def test_an_instant_with_nanoseconds_or_an_offset_parses(self):
        self.assertEqual(fm.parse_time("2026-10-04T10:00:00.123456789Z"), fm.parse_time("2026-10-04T12:00:00.123456+02:00"))
        self.assertEqual(fm.iso(fm.parse_time("2026-10-04T10:00:00Z")), "2026-10-04T10:00:00Z")


if __name__ == "__main__":
    unittest.main()
