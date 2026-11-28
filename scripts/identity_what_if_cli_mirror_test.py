#!/usr/bin/env python3
# script-test-reads: scripts/identity-what-if scripts/identity_what_if*.py scripts/identity-tools.sh scripts/identity-what-if-roots.tsv scripts/pure-move-check scripts/pure_move_*.py scripts/identity_stamp_carry*.py
"""scripts/identity-what-if, black-box: A MIRROR CHANGING UNDER THE RUN is a HARNESS ERROR (exit 2), never takes moved
(2026-11-19: lane C's US what-if read a seeding mirror on one side and reported 87k takes moved) -- a seed in flight or
due, a re-seed or a tailed write between the sides, a side whose live asks failed past the bound; the run FENCES the
tailer for its reads and leaves the fence lingering, DRAINS a mirror far behind first, and two runs of one base resolve
its side once. The main path's cases are in identity_what_if_cli_test.py. Ported from identity-what-if-test.sh."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import time
import unittest
from typing import List

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402
from identity_what_if_cli_fixture import JUDGEMENTS, WhatIfCase  # noqa: E402


class MirrorTest(WhatIfCase):
    def fence_kinds(self, name: str = "fences-seen") -> List[str]:
        if not os.path.exists(self.path(name)):
            return []
        return [line.split()[1] for line in self.read(name).splitlines() if len(line.split()) > 1]

    def lingering(self) -> List[str]:
        d = self.path("fence/kinowo_uk")
        return [open(os.path.join(d, f)).read().split()[1] for f in sorted(os.listdir(d)) if f.startswith("what-if-")]

    def test_a_seeding_mirror_is_refused_before_anything_compiles(self) -> None:
        self.write("generations", "seeding\n")
        done = self.what_if("c0", "c4", "2026-11-06T13:00:00Z", "--no-serve")
        self.assert_harness_error("HARNESS ERROR.*mirror not ready.*seed is in flight", done)
        self.assertEqual("", self.calls())

    def test_a_mirror_whose_reseed_is_due_is_refused(self) -> None:
        self.write("generations", "reseed-due\n")
        self.assert_harness_error("HARNESS ERROR.*mirror not ready",
                                  self.what_if("c0", "c4", "2026-11-06T13:00:00Z", "--no-serve"))

    def test_a_reseed_between_the_sides_keeps_nothing_and_strands_no_compile(self) -> None:
        self.write("generations", "100.1\n200.1\n")
        done = self.what_if("c0", "c4", "2026-11-06T14:00:00Z", "--no-serve", env={"STUB_SLOW_SBT": self.c["c4"]})
        self.assert_harness_error("HARNESS ERROR.*re-seeded during the run.*100.1.*200.1", done)
        self.assertFalse(os.path.exists(self.side_dir(self.c["c0"], "2026-11-06T14:00:00Z")))
        self.assertEqual([], self.verdict_lines("c4", "c0"))
        # an exit leaves no compile of the head behind it (Order independence #90)
        self.assertEqual(0, cli_harness.still_running(self.path("sbt-pids")))

    def test_a_seed_starting_during_the_head_is_caught_after_it(self) -> None:
        self.write("generations", "100.1\n100.1\nseeding\n")
        done = self.what_if("c0", "c4", "2026-11-06T15:00:00Z", "--no-serve")
        self.assert_harness_error("HARNESS ERROR.*re-seeded during the run", done)
        self.assertEqual(3, self.read("freshness-calls").count("--generation"))

    def test_a_write_tailed_between_the_sides_is_a_harness_error(self) -> None:
        self.write("tails", "T0\nT1\n")
        done = self.what_if("c0", "c4", "2026-11-06T15:30:00Z", "--no-serve")
        self.assert_harness_error("HARNESS ERROR.*tailed a write during the run", done)
        self.assertFalse(os.path.exists(self.side_dir(self.c["c0"], "2026-11-06T15:30:00Z")))

    def test_the_tailer_is_fenced_for_the_run_and_left_lingering(self) -> None:
        done = self.what_if("c0", "c4", "2026-11-06T15:40:00Z", "--no-serve")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["run", "run", "run"], self.fence_kinds())
        self.assertEqual(["linger"], self.lingering())

    def test_a_mirror_far_behind_is_drained_after_the_runs_holding_it(self) -> None:
        other = subprocess.Popen(["sleep", "600"])  # another agent's what-if, mid-run
        try:
            self.write(f"fence/kinowo_uk/what-if-{other.pid}", f"{int(time.time()) + 3600} run\n")
            # the stub dates the tail 60 s before ITS clock's second: 59-61 s read off the run's own
            self.write("behind", "1800\n1800\n60\n")
            run = self.start("--base", self.c["c0"], "--head", self.c["c4"], "--at", "2026-11-06T15:50:00Z",
                             "--judgements", JUDGEMENTS, "--no-serve", "uk",
                             env={"KINOWO_WHAT_IF_DRAIN_POLL_SECONDS": "0.2"})
            waiting = f"waiting for the runs holding the tail to end: what-if-{other.pid}"
            self.assertTrue(cli_harness.wait_until(lambda: waiting in self.read("err")), self.read("err"))
            self.assertEqual(["run"], self.lingering())
        finally:
            other.send_signal(signal.SIGTERM)
            other.wait()
        self.assertEqual(0, run.wait(cli_harness.HANG_BOUND), self.read("err"))
        self.assertRegex(self.read("err"), r"drained \((59|60|61)s behind\)")
        self.assertIn("CLEAN", self.read("out"))
        self.assertIn("drain", self.fence_kinds())
        self.assertEqual(["run", "run", "run"], self.fence_kinds()[-3:])

    def test_a_tail_read_freshness_warns_about_stops_no_run(self) -> None:
        self.write("tail-exit", "3\n")
        done = self.what_if("c0", "c4", "2026-11-06T15:52:00Z", "--no-serve")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("CLEAN", done.stdout)

    def test_two_countries_each_fenced_for_its_own_sides(self) -> None:
        done = self.what_if("c0", "c4", "2026-11-06T15:55:00Z", "--no-serve", countries=("de", "uk"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("linger", self.fence_kinds("fences-seen-de")[-1])

    def test_two_runs_of_one_base_at_once_resolve_its_side_once(self) -> None:
        for round_ in (1, 2, 3):
            if os.path.exists(self.path("calls")):
                os.remove(self.path("calls"))
            snapshot = f"2026-11-06T21:0{round_}:00Z"
            runs = [self.start("--base", self.c["c0"], "--head", self.c["c4"], "--at", snapshot, "--out",
                               self.path(f"conc-out-{name}"), "--no-serve", "--judgements", JUDGEMENTS, "uk",
                               env={"KINOWO_WHAT_IF_SIDE_LOCK_POLL_SECONDS": "0.2"}, err=f"conc-{name}.err",
                               out=f"conc-{name}.out") for name in ("a", "b")]
            codes = [run.wait(cli_harness.HANG_BOUND) for run in runs]
            errors = self.read("conc-a.err") + self.read("conc-b.err")
            self.assertEqual([0, 0], codes, errors)
            self.assertEqual(1, self.read("calls").count(f"java-start {self.c['c0']}"), errors)
            self.assertEqual(4, (self.read("conc-a.out") + self.read("conc-b.out")).count("CLEAN"), errors)
        left = [f for _, dirs, files in os.walk(self.path("cache/what-if/uk")) for f in dirs + files
                if f.endswith(".lock") or ".part" in f]
        self.assertEqual([], left)

    def test_a_side_whose_live_asks_failed_past_the_bound_is_not_kept(self) -> None:
        answers = "16000 live request(s) (api.themoviedb.org 16000; 16000 failed) in 300s, ~53.3 req/s"
        done = self.what_if("c0", "c4", "2026-11-06T17:00:00Z", "--no-serve", env={"STUB_ANSWERS": answers})
        self.assert_harness_error("HARNESS ERROR.*answers failed.*16000 of 16000", done)
        self.assertFalse(os.path.exists(self.side_dir(self.c["c0"], "2026-11-06T17:00:00Z")))

    def test_a_few_failed_asks_within_the_bound_still_diff(self) -> None:
        answers = "400 live request(s) (api.themoviedb.org 400; 3 failed) in 30s, ~13.3 req/s"
        done = self.what_if("c0", "c4", "2026-11-06T18:00:00Z", "--no-serve", env={"STUB_ANSWERS": answers})
        self.assertEqual(0, done.code, done.output)
        self.assertIn("CLEAN", done.stdout)


if __name__ == "__main__":
    unittest.main()
