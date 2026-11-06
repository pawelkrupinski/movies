#!/usr/bin/env python3
"""runner_scheduler.py: bitcashier always wins ci-runner, movies takes what it leaves -- up to the host's memory, landing
checks before Main's convergence rows before measurements -- and main's suite is handed only what is free now.

    python3 scripts/ci/runner_scheduler_test.py
"""
import json
import os
import subprocess
import sys
import tempfile
import time
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import runner_scheduler as rs  # noqa: E402

CFG = rs.Config()
IDLE = rs.Bitcashier(0, 0)
NOW = 1_000_000.0


def queued(**n):
    return {c: n.get(c, 0) for c in rs.CLASSES}


def inst(slot, cls, busy=False, idle_for=0):
    return rs.Instance(slot, cls, busy, NOW - idle_for)


class CeilingTest(unittest.TestCase):
    def test_an_idle_bitcashier_leaves_movies_the_hosts_memory(self):
        # (115 - 8) // 10 = 10 jobs of ~10 GiB
        self.assertEqual(rs.ceiling(CFG, IDLE), 10)

    def test_each_busy_bitcashier_job_takes_its_share_back(self):
        self.assertEqual(rs.ceiling(CFG, rs.Bitcashier(1, 0)), 9)
        self.assertEqual(rs.ceiling(CFG, rs.Bitcashier(3, 1)), 7)
        self.assertEqual(rs.ceiling(CFG, rs.Bitcashier(11, 5)), 0)

    def test_never_past_max_slots(self):
        big = rs.Config(ci_memory_gib=1000)
        self.assertEqual(rs.ceiling(big, IDLE), big.max_slots)

    def test_every_builder_or_every_deployer_busy_is_saturated(self):
        self.assertFalse(rs.saturated(CFG, rs.Bitcashier(11, 5)))
        self.assertTrue(rs.saturated(CFG, rs.Bitcashier(12, 0)))
        self.assertTrue(rs.saturated(CFG, rs.Bitcashier(0, 6)))


class DecideTest(unittest.TestCase):
    def test_a_runner_is_started_only_for_a_queued_job(self):
        d = rs.decide(CFG, IDLE, queued(), [], NOW)
        self.assertEqual(d.starts, ())
        d = rs.decide(CFG, IDLE, queued(main=2), [], NOW)
        self.assertEqual(d.starts, ((1, "main"), (2, "main")))

    def test_landing_checks_then_main_then_measurements(self):
        small = rs.Config(max_slots=4, ci_memory_gib=48)  # ceiling (48 - 8) // 10 = 4
        d = rs.decide(small, IDLE, queued(measure=3, main=2, check=1, light=1), [], NOW)
        self.assertEqual([c for _, c in d.starts], ["check", "main", "main", "measure"])

    def test_a_waiting_runner_of_the_class_counts_against_its_queue(self):
        d = rs.decide(CFG, IDLE, queued(check=2), [inst(1, "check")], NOW)
        self.assertEqual(d.starts, ((2, "check"),))

    def test_busy_runners_hold_their_slots(self):
        running = [inst(s, "measure", busy=True) for s in range(1, 9)]
        d = rs.decide(CFG, IDLE, queued(check=5), running, NOW)
        self.assertEqual(d.starts, ((9, "check"), (10, "check")))
        self.assertEqual(d.stops, ())

    def test_bitcashier_saturated_starts_nothing_and_frees_idle_runners(self):
        ours = [inst(1, "main"), inst(2, "check", busy=True)]
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(check=3), ours, NOW)
        self.assertEqual(d.starts, ())
        self.assertEqual(d.stops, (1,))  # the busy one finishes its job
        self.assertEqual(d.free_for_main, 0)

    def test_bitcashier_starting_work_shrinks_movies_from_the_lowest_class(self):
        # ceiling 7 with 4 bitcashier jobs busy; 9 of ours, 2 of them idle
        ours = [inst(s, "check", busy=True) for s in range(1, 8)] + [inst(8, "check"), inst(9, "measure")]
        d = rs.decide(CFG, rs.Bitcashier(4, 0), queued(check=1, measure=1), ours, NOW)
        self.assertEqual(d.stops, (8, 9))
        self.assertEqual(d.starts, ())

    def test_an_idle_runner_with_nothing_queued_gives_its_slot_back(self):
        fresh, stale = inst(1, "main", idle_for=10), inst(2, "main", idle_for=CFG.idle_stop_seconds)
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [fresh, stale], NOW).stops, (2,))
        # ... but not while a job of its class waits for it
        self.assertEqual(rs.decide(CFG, IDLE, queued(main=1), [stale], NOW).stops, ())

    def test_a_stopping_slot_is_not_reused_in_the_same_tick(self):
        d = rs.decide(rs.Config(max_slots=2, ci_memory_gib=28), IDLE, queued(check=1),
                      [inst(1, "measure", idle_for=999), inst(2, "measure", busy=True)], NOW)
        self.assertEqual(d.stops, (1,))
        self.assertEqual(d.starts, ())

    def test_main_is_offered_what_is_left_after_the_queue_less_the_check_headroom(self):
        running = [inst(s, "check", busy=True) for s in range(1, 4)]
        d = rs.decide(CFG, IDLE, queued(check=1, main=2), running, NOW)
        # ceiling 10 - 3 running - 3 started = 4, less 1 kept for the next landing check
        self.assertEqual(d.free_for_main, 3)
        self.assertEqual(rs.decide(CFG, IDLE, queued(check=20), [], NOW).free_for_main, 0)


class DemandTest(unittest.TestCase):
    def test_queued_jobs_count_by_their_label(self):
        jobs = [{"status": "queued", "labels": ["movies-check"]},
                {"status": "queued", "labels": ["movies-main"]},
                {"status": "queued", "labels": ["movies"]},
                {"status": "in_progress", "labels": ["movies-main"]},
                {"status": "queued", "labels": ["ubuntu-22.04"]}]
        self.assertEqual(rs.demand_of(jobs), queued(check=1, main=1, light=1))


class RowsForMainTest(unittest.TestCase):
    def test_a_fresh_file_hands_its_free_slots(self):
        self.assertEqual(rs.rows_for_main({"at": NOW - 30, "free_for_main": 6}, NOW), 6)

    def test_a_stale_or_broken_file_hands_nothing(self):
        self.assertEqual(rs.rows_for_main({"at": NOW - rs.MAX_AGE_SECONDS - 1, "free_for_main": 6}, NOW), 0)
        self.assertEqual(rs.rows_for_main({"at": NOW + 3600, "free_for_main": 6}, NOW), 0)
        self.assertEqual(rs.rows_for_main({"free_for_main": 6}, NOW), 0)
        self.assertEqual(rs.rows_for_main({"at": NOW, "free_for_main": "x"}, NOW), 0)

    def test_the_cli_prints_an_output_line_and_zero_on_any_doubt(self):
        with tempfile.TemporaryDirectory() as d:
            fresh = os.path.join(d, "fresh.json")
            with open(fresh, "w") as f:
                json.dump({"at": time.time(), "free_for_main": 4}, f)
            broken = os.path.join(d, "broken.json")
            with open(broken, "w") as f:
                f.write("not json")
            for path, want in ((fresh, "4"), (broken, "0"), (os.path.join(d, "missing.json"), "0")):
                out = subprocess.run([sys.executable, os.path.join(HERE, "runner_scheduler.py"), "rows-for-main",
                                      "--file", path], capture_output=True, text=True, check=True).stdout
                self.assertEqual(out, f"self-hosted-rows={want}\n")


class ConfigTest(unittest.TestCase):
    def test_an_unknown_key_fails_rather_than_being_ignored(self):
        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
            json.dump({"max_slot": 12}, f)
        try:
            with self.assertRaises(ValueError):
                rs.Config.load(f.name)
        finally:
            os.remove(f.name)


if __name__ == "__main__":
    unittest.main()
