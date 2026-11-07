#!/usr/bin/env python3
"""runner_scheduler.py: bitcashier always wins ci-runner, movies takes what it leaves -- a runner starts when the memory
its job needs is free right now (the host, ci.slice and its class's slice) and the CPU is under-used, landing checks
before measurements -- and main's jobs are never served here (the owner, 2026-11-06: main always on GitHub).

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
GIB = 2**30


def queued(**n):
    return {c: n.get(c, 0) for c in rs.CLASSES}


def inst(slot, cls, busy=False, idle_for=0, started_ago=3600):
    return rs.Instance(slot, cls, busy, NOW - idle_for, NOW - started_ago)


def readings(available=500, ci_free=500, class_free=None, cpu_busy=0.0, cores=1000, builders_ws=0.0):
    return rs.Readings(available, ci_free, dict(class_free or {}), cpu_busy, cores, builders_ws)


# Plenty of everything: only the queue, the slots and bitcashier's runners decide. The recorded tests pass ci-runner's 32.
ROOMY = readings()
CORES = 32


class MemoryRoomTest(unittest.TestCase):
    """The memory a new job needs must be free NOW: the tighter of the host's MemAvailable and ci.slice's headroom, less
    a reserve for every bitcashier runner that is idle and may start, less a safety margin, less the expected size of
    every runner of ours that has not shown its memory yet."""

    def test_whole_expected_jobs_in_the_live_headroom(self):
        # 72 GiB free, no bitcashier runner idle to reserve for: (72 - 6) // 11 = 6
        r = readings(available=72, ci_free=90)
        self.assertEqual(rs.memory_room(CFG, rs.Bitcashier(12, 6), r, pending=0), 6)

    def test_the_tighter_of_the_host_and_ci_slice(self):
        self.assertEqual(rs.memory_room(CFG, rs.Bitcashier(12, 6), readings(available=90, ci_free=39), 0), 3)

    def test_each_idle_bitcashier_runner_keeps_its_reserve(self):
        # 6 idle builders at the 4 GiB floor, 6 idle deployers at 1 GiB: 72 - 24 - 6 - 6 = 36 -> 3
        self.assertEqual(rs.memory_room(CFG, rs.Bitcashier(6, 0), readings(available=72, ci_free=90), 0), 3)

    def test_an_idle_builders_reserve_is_what_a_busy_one_holds_now(self):
        # 6 busy builders hold 30 GiB of working set (5 each): the 6 idle ones reserve 30, the deployers 6
        r = readings(available=72, ci_free=90, builders_ws=30)
        self.assertEqual(rs.memory_room(CFG, rs.Bitcashier(6, 0), r, 0), 2)

    def test_a_runner_that_has_not_allocated_yet_counts_at_its_expected_size(self):
        r = readings(available=72, ci_free=90)
        self.assertEqual(rs.memory_room(CFG, rs.Bitcashier(12, 6), r, pending=2), 4)

    def test_overcommitted_is_negative(self):
        self.assertLess(rs.memory_room(CFG, rs.Bitcashier(12, 6), readings(available=3, ci_free=10), 0), 0)


class CpuRoomTest(unittest.TestCase):
    """Another job only helps while the host's CPU is under-used: on a saturated host it slows every job, bitcashier's
    included, and gains no throughput."""

    def test_room_up_to_the_target_in_whole_jobs(self):
        # 85 % of 32 = 27.2 cores; 16 busy: (27.2 - 16) // 4 = 2
        self.assertEqual(rs.cpu_room(CFG, readings(cpu_busy=16, cores=CORES), pending=0), 2)

    def test_a_just_started_job_counts_at_its_cores(self):
        self.assertEqual(rs.cpu_room(CFG, readings(cpu_busy=16, cores=CORES), pending=1), 1)

    def test_a_saturated_host_has_none(self):
        self.assertLessEqual(rs.cpu_room(CFG, readings(cpu_busy=31.4, cores=CORES), 0), 0)

    def test_no_reading_yet_starts_nothing(self):
        self.assertEqual(rs.cpu_room(CFG, readings(cpu_busy=None, cores=CORES), 0), 0)


class RecordedReadingsTest(unittest.TestCase):
    """ci-runner's own readings, 2026-11-07."""

    # The owner's 'fix the schedule so runners don't stay idle': bitcashier 9/12 builders busy, 3 measurements running
    # (31 GiB in their slice), 32 measurements queued, MemAvailable 58 GiB. The old fixed budgets allowed 3 (slice
    # 48 GiB // 16 GiB worst case) -- with the host's CPU at 3138 % of 3200 % nothing more helps.
    def busy_morning(self, cpu_busy, measure_slice_gib):
        ours = [inst(s, "measure", busy=True) for s in (1, 2, 3)]
        r = readings(available=58, ci_free=50, class_free={"measure": measure_slice_gib - 31}, cpu_busy=cpu_busy,
                     cores=CORES, builders_ws=9 * 5)
        return rs.decide(CFG, rs.Bitcashier(9, 0), queued(measure=32), ours, NOW, r)

    def test_today_with_the_cpu_saturated_starts_nothing_more(self):
        self.assertEqual(self.busy_morning(cpu_busy=31.4, measure_slice_gib=80).starts, ())

    def test_today_with_cpu_to_spare_starts_what_the_memory_holds(self):
        # 50 - (3 idle builders x 5, what a busy one holds + 6 deployers x 1) - 6 margin = 23 GiB: 2 more jobs of 11
        d = self.busy_morning(cpu_busy=12, measure_slice_gib=80)
        self.assertEqual(d.starts, ((4, "measure"), (5, "measure")))

    def test_an_idle_host_takes_jobs_up_to_its_memory(self):
        # bitcashier idle: (105 GiB available - 12 x 4 - 6 x 1 reserved - 6 margin) // 11 = 4 (the measure slice
        # holds (80 - 6) // 11 = 6, the CPU (27.2 - 2) // 4 = 6)
        r = readings(available=105, ci_free=115, class_free={"measure": 80}, cpu_busy=2, cores=CORES)
        self.assertEqual(len(rs.decide(CFG, IDLE, queued(measure=32), [], NOW, r).starts), 4)

    # 07:40-08:22Z: the fixed budgets started six measurements at once into the empty 48 GiB slice, then three more at
    # 07:51 beside six running; the slice hit its 48 GiB and its OOM killer took 18 processes (exit 137).
    def test_the_oom_morning_never_fills_the_old_48_gib_slice(self):
        r = readings(available=100, ci_free=100, class_free={"measure": 48}, cpu_busy=4, cores=CORES)
        burst = rs.decide(CFG, rs.Bitcashier(3, 1), queued(measure=30), [inst(2, "light", busy=True)], NOW, r)
        self.assertEqual(len(burst.starts), 3)  # (48 - 6) // 11, not 6
        # the next tick: those three have not allocated yet, so the slice still reads empty -- they count at 11 each
        fresh = [inst(s, "measure", busy=True, started_ago=30) for s in (1, 3, 4)]
        self.assertEqual(rs.decide(CFG, rs.Bitcashier(3, 1), queued(measure=27), fresh, NOW, r).starts, ())
        # 07:51: six running, their slice at 42 of 48 GiB -- no seventh, eighth, ninth
        six = [inst(s, "measure", busy=True) for s in (1, 3, 4, 5, 6, 7)]
        r = readings(available=60, ci_free=60, class_free={"measure": 48 - 42}, cpu_busy=10, cores=CORES)
        self.assertEqual(rs.decide(CFG, rs.Bitcashier(2, 0), queued(measure=30), six, NOW, r).starts, ())


class SaturatedTest(unittest.TestCase):
    def test_every_builder_or_every_deployer_busy_is_saturated(self):
        self.assertFalse(rs.saturated(CFG, rs.Bitcashier(11, 5)))
        self.assertTrue(rs.saturated(CFG, rs.Bitcashier(12, 0)))
        self.assertTrue(rs.saturated(CFG, rs.Bitcashier(0, 6)))


class DecideTest(unittest.TestCase):
    def test_a_runner_is_started_only_for_a_queued_job(self):
        d = rs.decide(CFG, IDLE, queued(), [], NOW, ROOMY)
        self.assertEqual(d.starts, ())
        d = rs.decide(CFG, IDLE, queued(measure=2), [], NOW, ROOMY)
        self.assertEqual(d.starts, ((1, "measure"), (2, "measure")))

    def test_never_past_max_slots(self):
        d = rs.decide(CFG, IDLE, queued(check=40), [], NOW, ROOMY)
        self.assertEqual(len(d.starts), CFG.max_slots)

    def test_landing_checks_then_measurements_then_light_jobs(self):
        small = rs.Config(max_slots=4)
        d = rs.decide(small, IDLE, queued(light=2, measure=2, check=1), [], NOW, ROOMY)
        self.assertEqual([c for _, c in d.starts], ["check", "measure", "measure", "light"])

    def test_a_waiting_runner_of_the_class_counts_against_its_queue(self):
        d = rs.decide(CFG, IDLE, queued(check=2), [inst(1, "check")], NOW, ROOMY)
        self.assertEqual(d.starts, ((2, "check"),))

    def test_busy_runners_hold_their_slots(self):
        running = [inst(s, "measure", busy=True) for s in range(1, 9)]
        d = rs.decide(CFG, IDLE, queued(check=5), running, NOW, ROOMY)
        self.assertEqual(d.starts, ((9, "check"), (10, "check")))
        self.assertEqual(d.stops, ())

    def test_a_class_never_outgrows_its_slice(self):
        # Verify scrape recording #14: nine measurements in a 48 GiB slice, the OOM killer took five. A class starts
        # only what its slice's live headroom holds at the expected size.
        d = rs.decide(CFG, IDLE, queued(measure=9, check=1), [], NOW, readings(class_free={"measure": 40}))
        self.assertEqual([c for _, c in d.starts], ["check", "measure", "measure", "measure"])

    def test_a_class_with_no_slice_limit_is_capped_by_the_host_alone(self):
        d = rs.decide(CFG, IDLE, queued(check=12), [], NOW, readings(class_free={"measure": 0}))
        self.assertEqual(len(d.starts), 10)

    def test_bitcashier_saturated_starts_nothing_and_frees_idle_runners(self):
        ours = [inst(1, "measure"), inst(2, "check", busy=True)]
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(check=3), ours, NOW, ROOMY)
        self.assertEqual(d.starts, ())
        self.assertEqual(d.stops, (1,))  # the busy one finishes its job

    def test_bitcashier_needing_memory_stops_idle_runners_from_the_lowest_class(self):
        # bitcashier started work: 12 GiB left, less 5 reserved and the 6 margin, holds neither waiting runner's 11
        ours = [inst(s, "check", busy=True) for s in range(1, 8)] + [inst(8, "check"), inst(9, "measure")]
        r = readings(available=12, ci_free=12)
        d = rs.decide(CFG, rs.Bitcashier(11, 5), queued(check=1, measure=1), ours, NOW, r)
        self.assertEqual(d.stops, (8, 9))
        self.assertEqual(d.starts, ())

    def test_an_idle_runner_with_nothing_queued_gives_its_slot_back(self):
        fresh, stale = inst(1, "measure", idle_for=10), inst(2, "measure", idle_for=CFG.idle_stop_seconds)
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [fresh, stale], NOW, ROOMY).stops, (2,))
        # ... but not while a job of its class waits for it
        self.assertEqual(rs.decide(CFG, IDLE, queued(measure=1), [stale], NOW, ROOMY).stops, ())

    def test_a_stopping_slot_is_not_reused_in_the_same_tick(self):
        d = rs.decide(rs.Config(max_slots=2), IDLE, queued(check=1),
                      [inst(1, "measure", idle_for=999), inst(2, "measure", busy=True)], NOW, ROOMY)
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
        self.assertEqual(rs.decide(CFG, IDLE, demand, [], NOW, ROOMY).starts, ())


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
            self.assertEqual((), rs.decide(rs.Config(), rs.Bitcashier(0, 0), {"check": 1}, [i], NOW, ROOMY).starts)
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


class ReadingsTest(unittest.TestCase):
    """The live inputs, off a cgroup tree and /proc laid out as ci-runner's."""

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root)

    def cgroup(self, rel, max_bytes, current, inactive_file):
        d = os.path.join(self.root, "cg", rel)
        os.makedirs(d, exist_ok=True)
        for name, text in (("memory.max", str(max_bytes)), ("memory.current", str(current)),
                           ("memory.stat", f"anon 1\nfile 2\ninactive_file {inactive_file}\n")):
            with open(os.path.join(d, name), "w") as f:
                f.write(text + "\n")
        return d

    def proc(self, available_kb, user, idle):
        d = os.path.join(self.root, "proc")
        os.makedirs(d, exist_ok=True)
        with open(os.path.join(d, "meminfo"), "w") as f:
            f.write(f"MemTotal: 131072000 kB\nMemAvailable: {available_kb} kB\n")
        with open(os.path.join(d, "stat"), "w") as f:
            f.write(f"cpu  {user} 0 0 {idle} 0 0 0 0 0 0\ncpu0 1 0 0 1 0 0 0 0 0 0\n")
        return d

    def cfg(self):
        cg = os.path.join(self.root, "cg")
        return rs.Config(ci_cgroup=os.path.join(cg, "ci.slice"),
                         class_cgroup=os.path.join(cg, "ci.slice/ci-movies.slice/ci-movies-{cls}.slice"),
                         builder_cgroups=os.path.join(cg, "ci.slice/ci-check.slice/builder-*.service"),
                         proc=os.path.join(self.root, "proc"))

    def test_headroom_is_the_limit_less_the_working_set_page_cache_is_free(self):
        # ci.slice 115 GiB, 70 GiB charged of which 31 GiB inactive page cache: 76 GiB free, not 45
        self.cgroup("ci.slice", 115 * GIB, 70 * GIB, 31 * GIB)
        self.cgroup("ci.slice/ci-movies.slice/ci-movies-measure.slice", 80 * GIB, 31 * GIB, 6 * GIB)
        with open(os.path.join(self.cgroup("ci.slice/ci-movies.slice/ci-movies-check.slice", 0, 0, 0), "memory.max"), "w") as f:
            f.write("max\n")  # no limit: the class is held by the host alone
        self.cgroup("ci.slice/ci-check.slice/builder-1.service", 0, 5 * GIB, 1 * GIB)
        self.cgroup("ci.slice/ci-check.slice/builder-2.service", 0, 3 * GIB, 1 * GIB)
        self.proc(58 * 2**20, user=0, idle=0)
        cpu = rs.CpuMeter(window_seconds=300)
        r = rs.read_readings(self.cfg(), cpu, NOW)
        self.assertEqual(r.mem_available_gib, 58)
        self.assertEqual(r.ci_free_gib, 76)
        self.assertEqual(r.class_free_gib, {"measure": 55})
        self.assertEqual(r.builders_working_set_gib, 6)
        self.assertIsNone(r.cpu_busy_cores)  # one sample is no rate yet

    def test_cpu_is_the_busy_share_over_the_window_in_cores(self):
        cpu = rs.CpuMeter(window_seconds=300)
        cpu.add(NOW - 400, busy=0, total=0)
        cpu.add(NOW - 300, busy=1000, total=4000)
        cpu.add(NOW - 100, busy=2000, total=6000)
        cpu.add(NOW, busy=4000, total=8000)
        # within the window: (4000 - 1000) / (8000 - 4000) of 32 cores; the older sample is dropped
        self.assertAlmostEqual(cpu.busy_cores(32), 24.0)
        self.assertEqual(len(cpu.samples), 3)

    def test_proc_stat_busy_is_everything_but_idle_and_iowait(self):
        self.assertEqual(rs.cpu_times("cpu  10 1 2 100 5 3 4 7 0 0\n"), (27, 132))


if __name__ == "__main__":
    unittest.main()
