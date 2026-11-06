#!/usr/bin/env python3
"""runner_scheduler.py: bitcashier always wins ci-runner, movies takes what it leaves -- up to the host's memory, landing
checks before measurements -- and main's jobs are never served here (the owner, 2026-11-06: main always on GitHub).

    python3 scripts/ci/runner_scheduler_test.py
"""
import json
import os
import sys
import tempfile
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
        d = rs.decide(CFG, IDLE, queued(measure=2), [], NOW)
        self.assertEqual(d.starts, ((1, "measure"), (2, "measure")))

    def test_landing_checks_then_measurements_then_light_jobs(self):
        small = rs.Config(max_slots=4, ci_memory_gib=48)  # ceiling (48 - 8) // 10 = 4
        d = rs.decide(small, IDLE, queued(light=2, measure=2, check=1), [], NOW)
        self.assertEqual([c for _, c in d.starts], ["check", "measure", "measure", "light"])

    def test_a_waiting_runner_of_the_class_counts_against_its_queue(self):
        d = rs.decide(CFG, IDLE, queued(check=2), [inst(1, "check")], NOW)
        self.assertEqual(d.starts, ((2, "check"),))

    def test_busy_runners_hold_their_slots(self):
        running = [inst(s, "measure", busy=True) for s in range(1, 9)]
        d = rs.decide(CFG, IDLE, queued(check=5), running, NOW)
        self.assertEqual(d.starts, ((9, "check"), (10, "check")))
        self.assertEqual(d.stops, ())

    def test_bitcashier_saturated_starts_nothing_and_frees_idle_runners(self):
        ours = [inst(1, "measure"), inst(2, "check", busy=True)]
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(check=3), ours, NOW)
        self.assertEqual(d.starts, ())
        self.assertEqual(d.stops, (1,))  # the busy one finishes its job

    def test_bitcashier_starting_work_shrinks_movies_from_the_lowest_class(self):
        # ceiling 7 with 4 bitcashier jobs busy; 9 of ours, 2 of them idle
        ours = [inst(s, "check", busy=True) for s in range(1, 8)] + [inst(8, "check"), inst(9, "measure")]
        d = rs.decide(CFG, rs.Bitcashier(4, 0), queued(check=1, measure=1), ours, NOW)
        self.assertEqual(d.stops, (8, 9))
        self.assertEqual(d.starts, ())

    def test_an_idle_runner_with_nothing_queued_gives_its_slot_back(self):
        fresh, stale = inst(1, "measure", idle_for=10), inst(2, "measure", idle_for=CFG.idle_stop_seconds)
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [fresh, stale], NOW).stops, (2,))
        # ... but not while a job of its class waits for it
        self.assertEqual(rs.decide(CFG, IDLE, queued(measure=1), [stale], NOW).stops, ())

    def test_a_stopping_slot_is_not_reused_in_the_same_tick(self):
        d = rs.decide(rs.Config(max_slots=2, ci_memory_gib=28), IDLE, queued(check=1),
                      [inst(1, "measure", idle_for=999), inst(2, "measure", busy=True)], NOW)
        self.assertEqual(d.stops, (1,))
        self.assertEqual(d.starts, ())


class DemandTest(unittest.TestCase):
    def test_queued_jobs_count_by_their_label(self):
        jobs = [{"status": "queued", "labels": ["movies-check"]},
                {"status": "queued", "labels": ["movies-measure"]},
                {"status": "queued", "labels": ["movies"]},
                {"status": "in_progress", "labels": ["movies-measure"]},
                {"status": "queued", "labels": ["ubuntu-22.04"]}]
        self.assertEqual(rs.demand_of(jobs), queued(check=1, measure=1, light=1))

    def test_main_is_never_served_here(self):
        # main's jobs run on GitHub's hosted runners alone (the owner, 2026-11-06): no class, no label for them, so a
        # job asking for the retired `movies-main` label starts nothing
        self.assertNotIn("main", rs.CLASSES)
        self.assertNotIn("movies-main", rs.LABELS.values())
        demand = rs.demand_of([{"status": "queued", "labels": ["movies-main"]}])
        self.assertEqual(rs.decide(CFG, IDLE, demand, [], NOW).starts, ())


class ConfigTest(unittest.TestCase):
    def test_an_unknown_key_fails_rather_than_being_ignored(self):
        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
            json.dump({"max_slot": 12}, f)
        try:
            with self.assertRaises(ValueError):
                rs.Config.load(f.name)
        finally:
            os.remove(f.name)

    def test_the_retired_capacity_publication_keys_are_refused(self):
        # nothing reads capacity.json any more; a config still naming its publisher or headroom is out of date
        for key in ("publisher_app_id", "publisher_key", "check_headroom"):
            with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
                json.dump({key: 1}, f)
            try:
                with self.assertRaises(ValueError, msg=key):
                    rs.Config.load(f.name)
            finally:
                os.remove(f.name)


if __name__ == "__main__":
    unittest.main()
