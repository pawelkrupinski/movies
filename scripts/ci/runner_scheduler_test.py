#!/usr/bin/env python3
"""runner_scheduler.py: bitcashier always wins ci-runner, movies takes what it leaves -- up to the host's memory, landing
checks before measurements -- and main's jobs are never served here (the owner, 2026-11-06: main always on GitHub).

    python3 scripts/ci/runner_scheduler_test.py
"""
import io
import json
import os
import shutil
import sys
import tempfile
import unittest
import unittest.mock

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

    # Verify scrape recording #14: nine measurement runners in ci-movies-measure.slice's 48 GiB, ~11 GiB of JVM each --
    # the slice's OOM killer took five of them (exit 137). A class's slice holds as many runners as fit at a job's worst
    # case (the container's --memory), never more.
    def test_a_class_never_outgrows_its_slice(self):
        caps = {"measure": 48 * 2**30 // (CFG.job_max_gib * 2**30)}  # 48 GiB / 16 GiB = 3
        d = rs.decide(CFG, IDLE, queued(measure=9), [], NOW, caps)
        self.assertEqual([c for _, c in d.starts], ["measure"] * 3)

    def test_running_runners_count_against_their_class_slice(self):
        ours = [inst(1, "measure", busy=True), inst(2, "measure", busy=True)]
        d = rs.decide(CFG, IDLE, queued(measure=5, check=1), ours, NOW, {"measure": 3})
        self.assertEqual(d.starts, ((3, "check"), (4, "measure")))

    def test_a_class_with_no_slice_limit_is_capped_by_the_ceiling_alone(self):
        d = rs.decide(CFG, IDLE, queued(check=12), [], NOW, {"measure": 3})
        self.assertEqual(len(d.starts), 10)

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


class FakeDocker:
    """`systemctl` and `docker top` as ci-runner answers them. `docker top` hands its ps options to ps and then looks for
    a PID column to keep the container's own rows: without one it exits 1 ("Couldn't find PID field in ps output")."""

    def __init__(self, comms, top_fails=False, state="active"):
        self.comms, self.top_fails, self.state, self.calls = comms, top_fails, state, []

    def __call__(self, cmd, timeout=30):
        self.calls.append(cmd)
        if cmd[0] == "systemctl":
            return rs.subprocess.CompletedProcess(cmd, 0, f"{self.state}\n" if cmd[1] == "is-active" else "", "")
        fields = cmd[cmd.index("-eo") + 1].split(",")
        if self.top_fails or "pid" not in fields:
            return rs.subprocess.CompletedProcess(cmd, 1, "", "Error response from daemon: Couldn't find PID field in ps output\n")
        rows = [" ".join("PID" if f == "pid" else f.upper() for f in fields)]
        rows += [" ".join(str(100 + i) if f == "pid" else c for f in fields) for i, c in enumerate(self.comms)]
        return rs.subprocess.CompletedProcess(cmd, 0, "\n".join(rows) + "\n", "")


class HostTest(unittest.TestCase):
    """#1005: the probe asked `docker top <c> -eo comm`, which always fails, so every runner read idle and eight convergence
    rows were stopped mid-job at idle_stop_seconds."""

    def host(self, docker):
        state = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, state)
        with open(os.path.join(state, "2.env"), "w") as f:
            f.write("RUNNER_CLASS=check\n")
        patcher = unittest.mock.patch.object(rs, "_run", docker)
        patcher.start()
        self.addCleanup(patcher.stop)
        return rs.Host(rs.Config(state_dir=state))

    def test_a_runner_running_a_job_reads_busy(self):
        [i] = self.host(FakeDocker(["tini", "Runner.Listener", "Runner.Worker"])).instances(NOW)
        self.assertTrue(i.busy)

    def test_a_runner_waiting_for_a_job_reads_idle(self):
        [i] = self.host(FakeDocker(["tini", "Runner.Listener"])).instances(NOW)
        self.assertFalse(i.busy)

    def test_a_probe_that_fails_reads_busy_and_says_so(self):
        # never stop a runner on doubt: a failed probe is a runner that may be running a job
        with unittest.mock.patch("sys.stderr", new_callable=io.StringIO) as err:
            [i] = self.host(FakeDocker([], top_fails=True)).instances(NOW)
        self.assertTrue(i.busy)
        self.assertIn("Couldn't find PID field", err.getvalue())

    def test_a_slot_still_starting_waits_for_its_job_and_no_second_runner_is_started_for_it(self):
        # activating: ExecStartPre mints its registration token, no container yet, so `docker top` fails. Read as busy, it
        # counted as no runner waiting for the queued job, and the next tick started another one for the same job.
        docker = FakeDocker([], top_fails=True, state="activating")
        with unittest.mock.patch("sys.stderr", new_callable=io.StringIO):
            host = self.host(docker)
            [i] = host.instances(NOW)
            self.assertFalse(i.busy)
            self.assertEqual((), rs.decide(rs.Config(), rs.Bitcashier(0, 0), {"check": 1}, [i], NOW).starts)
            host.stop(2)  # nor is it stopped on doubt
        self.assertFalse([c for c in docker.calls if c[:2] == ["systemctl", "stop"]])

    def test_a_busy_runner_is_never_stopped(self):
        docker = FakeDocker(["Runner.Worker"])
        self.host(docker).stop(2)
        self.assertFalse([c for c in docker.calls if c[:2] == ["systemctl", "stop"]])

    def test_a_runner_whose_probe_fails_is_never_stopped(self):
        docker = FakeDocker([], top_fails=True)
        with unittest.mock.patch("sys.stderr", new_callable=io.StringIO):
            self.host(docker).stop(2)
        self.assertFalse([c for c in docker.calls if c[:2] == ["systemctl", "stop"]])

    def test_an_idle_runner_is_stopped(self):
        docker = FakeDocker(["Runner.Listener"])
        self.host(docker).stop(2)
        self.assertTrue([c for c in docker.calls if c[:2] == ["systemctl", "stop"]])

    def test_a_class_slice_cap_is_its_memory_max_in_worst_case_jobs(self):
        root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, root)
        for cls, limit in (("measure", str(48 * 2**30)), ("check", "max")):
            os.makedirs(os.path.join(root, f"ci-movies-{cls}.slice"))
            with open(os.path.join(root, f"ci-movies-{cls}.slice", "memory.max"), "w") as f:
                f.write(limit + "\n")
        cfg = rs.Config(class_cgroup=os.path.join(root, "ci-movies-{cls}.slice"))
        self.assertEqual(rs.class_caps(cfg), {"measure": 3})  # "max" and a missing slice cap nothing


if __name__ == "__main__":
    unittest.main()
