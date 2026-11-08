#!/usr/bin/env python3
"""runner_scheduler.py: bitcashier always wins ci-runner, movies takes what it leaves -- a runner starts when the memory
its job needs is free right now (the host, ci.slice and its class's slice) and the CPU is under-used, landing checks
before measurements -- and main's jobs are never served here (the owner, 2026-11-06: main always on GitHub).

    python3 scripts/ci/runner_scheduler_test.py
"""
import dataclasses
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
# The recorded readings below predate the check lane and test the memory and CPU accounting alone: they run without it
# (CheckLaneTest replays the lane on the same accounting).
NO_LANE = rs.Config(check_lane_jobs=0)
IDLE = rs.Bitcashier(0, 0)
NOW = 1_000_000.0
GIB = 2**30


def queued(**n):
    return {c: n.get(c, 0) for c in rs.CLASSES}


def inst(slot, cls, busy=False, idle_for=0, started_ago=3600):
    return rs.Instance(slot, cls, busy, NOW - idle_for, NOW - started_ago)


def readings(available=500, ci_free=500, class_free=None, cpu_busy=0.0, cores=1000, busy_builders=(), class_cpu=None):
    return rs.Readings(available, ci_free, dict(class_free or {}), cpu_busy, cores, tuple(busy_builders),
                       class_cpu=dict(class_cpu or {}))


def cpus(text):
    return rs.parse_cpu_list(text)


# ci-runner's slices (bitcashier infra, movies-runners.nix): checks on CCD0 less two cores, measurements and light jobs
# on CCD1 plus those two cores (6-7 and their hyperthreads 22-23)
CHECK_CPUS, MEASURE_CPUS = cpus("0-5,16-21"), cpus("6-15,22-31")


def on_cpusets(check_busy, measure_busy):
    return {"check": rs.ClassCpu(CHECK_CPUS, check_busy), "measure": rs.ClassCpu(MEASURE_CPUS, measure_busy),
            "light": rs.ClassCpu(MEASURE_CPUS, measure_busy)}


def memory_room(cfg, bc, r, pending_gib):
    """Whole measurements the host's free memory holds."""
    return rs._whole_jobs(rs.memory_free_gib(cfg, bc, r, pending_gib), cfg.job_expected_gib["measure"])


def cpu_room(cfg, r, pending_cpus):
    """Whole 4-core measurements the CPU holds."""
    return rs._whole_jobs(rs.cpu_spare(cfg, r, pending_cpus, cfg.cpu_start_below), cfg.job_cpus["measure"])


# Plenty of everything: only the queue, the slots and bitcashier's runners decide. The recorded tests pass ci-runner's 32.
ROOMY = readings()
CORES = 32


class MemoryRoomTest(unittest.TestCase):
    """The memory a new job needs must be free NOW: the tighter of the host's MemAvailable and ci.slice's headroom, less
    a reserve for the bitcashier jobs that may start before the next reading (builder_burst builders at what a busy one
    holds, every idle deployer), less a safety margin, less the expected size of every runner of ours that has not
    shown its memory yet."""

    def test_whole_expected_jobs_in_the_live_headroom(self):
        # 72 GiB free, no bitcashier runner idle to reserve for: (72 - 6) // 11 = 6
        r = readings(available=72, ci_free=90)
        self.assertEqual(memory_room(CFG, rs.Bitcashier(12, 6), r, pending_gib=0), 6)

    def test_the_tighter_of_the_host_and_ci_slice(self):
        self.assertEqual(memory_room(CFG, rs.Bitcashier(12, 6), readings(available=90, ci_free=39), 0), 3)

    def test_a_burst_of_idle_builders_keeps_its_reserve(self):
        # 6 idle builders, of which builder_burst (4) reserve the 4 GiB floor; 6 idle deployers 1 GiB:
        # 72 - 16 - 6 - 6 = 44 -> 4
        self.assertEqual(memory_room(CFG, rs.Bitcashier(6, 0), readings(available=72, ci_free=90), 0), 4)
        # fewer idle than the burst: only those
        self.assertEqual(rs.reserved_builders(CFG, rs.Bitcashier(10, 0)), 2)
        self.assertEqual(rs.reserved_builders(CFG, rs.Bitcashier(12, 0)), 0)

    def test_a_builders_reserve_is_what_the_median_busy_one_holds_now(self):
        # 6 busy builders at 3..9 GiB: the median 5.5, for each of the 4 reserved; the deployers 6
        r = readings(available=72, ci_free=90, busy_builders=(3, 4, 5, 6, 7, 9))
        self.assertEqual(rs.builder_reserve_gib(CFG, r), 5.5)
        self.assertEqual(memory_room(CFG, rs.Bitcashier(6, 0), r, 0), 3)  # 72 - 22 - 6 - 6 = 38

    def test_a_builders_reserve_stays_within_its_floor_and_cap(self):
        self.assertEqual(rs.builder_reserve_gib(CFG, readings()), CFG.per_idle_builder_gib)  # none busy
        self.assertEqual(rs.builder_reserve_gib(CFG, readings(busy_builders=(1,))), CFG.per_idle_builder_gib)
        self.assertEqual(rs.builder_reserve_gib(CFG, readings(busy_builders=(30, 40))), CFG.per_busy_builder_cap_gib)

    def test_a_runner_that_has_not_allocated_yet_counts_at_its_expected_size(self):
        r = readings(available=72, ci_free=90)
        self.assertEqual(memory_room(CFG, rs.Bitcashier(12, 6), r, pending_gib=2 * 11), 4)

    def test_overcommitted_is_negative(self):
        self.assertLess(memory_room(CFG, rs.Bitcashier(12, 6), readings(available=3, ci_free=10), 0), 0)


class CpuRoomTest(unittest.TestCase):
    """Another job only helps while the host's CPU is under-used: on a saturated host it slows every job, bitcashier's
    included, and gains no throughput."""

    def test_room_up_to_the_target_in_whole_jobs(self):
        # 85 % of 32 = 27.2 cores; 16 busy: (27.2 - 16) // 4 = 2
        self.assertEqual(cpu_room(CFG, readings(cpu_busy=16, cores=CORES), pending_cpus=0), 2)

    def test_a_just_started_job_counts_at_its_cores(self):
        self.assertEqual(cpu_room(CFG, readings(cpu_busy=16, cores=CORES), pending_cpus=4), 1)

    def test_a_saturated_host_has_none(self):
        self.assertLessEqual(cpu_room(CFG, readings(cpu_busy=31.4, cores=CORES), 0), 0)

    def test_no_reading_yet_starts_nothing(self):
        self.assertEqual(cpu_room(CFG, readings(cpu_busy=None, cores=CORES), 0), 0)


class RecordedReadingsTest(unittest.TestCase):
    """ci-runner's own readings, 2026-11-07."""

    # The owner's 'fix the schedule so runners don't stay idle': bitcashier 9/12 builders busy, 3 measurements running
    # (31 GiB in their slice), 32 measurements queued, MemAvailable 58 GiB. The old fixed budgets allowed 3 (slice
    # 48 GiB // 16 GiB worst case) -- with the host's CPU at 3138 % of 3200 % nothing more helps.
    def busy_morning(self, cpu_busy, measure_slice_gib):
        ours = [inst(s, "measure", busy=True) for s in (1, 2, 3)]
        r = readings(available=58, ci_free=50, class_free={"measure": measure_slice_gib - 31}, cpu_busy=cpu_busy,
                     cores=CORES, busy_builders=[5] * 9)
        return rs.decide(NO_LANE, rs.Bitcashier(9, 0), queued(measure=32), ours, NOW, r)

    def test_today_with_the_cpu_saturated_starts_nothing_more(self):
        self.assertEqual(self.busy_morning(cpu_busy=31.4, measure_slice_gib=80).starts, ())

    def test_today_with_cpu_to_spare_starts_what_the_memory_holds(self):
        # 50 - (3 idle builders x 5, what the median busy one holds + 6 deployers x 1) - 6 margin = 23 GiB: 2 more of 11
        d = self.busy_morning(cpu_busy=12, measure_slice_gib=80)
        self.assertEqual(d.starts, ((4, "measure"), (5, "measure")))

    def test_an_idle_host_takes_jobs_up_to_its_memory(self):
        # bitcashier idle: (105 GiB available - 4 x 4 - 6 x 1 reserved - 6 margin) // 11 = 7; the measure slice holds
        # (80 - 6) // 11 = 6, the CPU (27.2 - 2) // 4 = 6
        r = readings(available=105, ci_free=115, class_free={"measure": 80}, cpu_busy=2, cores=CORES)
        self.assertEqual(len(rs.decide(NO_LANE, IDLE, queued(measure=32), [], NOW, r).starts), 6)

    # 13:30Z, the owner's "now 4 tasks are running but only half the cpu is used": bitcashier 1/12 builders busy,
    # 0/6 deployers; 4 movies runners busy, 5 measurements and 5 light jobs queued; host 92 GiB free, ci.slice 87, the
    # measure slice 55; CPU 10 of 32 busy. The busy builder's own unit holds ~5 GiB, but the old rule split ci-check's
    # whole working set (the idle runners' processes and caches, ~55 GiB) over the one busy builder and reserved that
    # for each of 11 idle ones: room 0, ten jobs queued on a half-idle host.
    def at_1330(self, busy_builder_gib):
        ours = [inst(1, "measure", busy=True), inst(2, "measure", busy=True), inst(3, "light", busy=True),
                inst(4, "check", busy=True)]
        r = readings(available=92, ci_free=87, class_free={"measure": 55}, cpu_busy=10, cores=CORES,
                     busy_builders=[busy_builder_gib])
        return rs.decide(NO_LANE, rs.Bitcashier(1, 0), queued(measure=5, light=5), ours, NOW, r)

    def test_1330_a_quiet_bitcashier_leaves_movies_the_idle_cpu(self):
        # memory: 87 - (4 x 5 + 6 x 1) - 6 = 55 -> 5; the CPU: (27.2 - 10) // 4 = 4; the measure slice (55 - 6) // 11 = 4
        d = self.at_1330(busy_builder_gib=5)
        self.assertEqual(d.room, 4)
        # and the light lane beside them: 11 GiB and (30.4 - 10 - 16) = 4.4 cores left hold its 3 slots at 1 / 1
        self.assertEqual([c for _, c in d.starts], ["measure"] * 4 + ["light"] * 3)

    def test_1330_one_huge_busy_builder_reserves_no_more_than_the_cap(self):
        # even a busy builder at 55 GiB reserves the 8 GiB cap: 87 - (4 x 8 + 6) - 6 = 43 -> 3
        self.assertGreaterEqual(self.at_1330(busy_builder_gib=55).room, 2)

    # 15:46:51Z, the owner's "half CPU again": bitcashier idle (0/12, 0/6), host 95 GiB free, ci.slice 88, the measure
    # slice 56 of its 80; CPU 13 of 32 busy; five measurements running -- slots 5 and 7 since 15:37, slots 1-3 started
    # 15:43-15:44Z (settling) -- 15 measurements queued. The old rule charged each settling one a full 11 GiB and 4
    # cores ON TOP of what it already showed, and every new one 4 cores against ~2.6 used:
    # CPU room (27.2 - 13 - 12) // 4 = 0, room 0. The margin is the slice's once, not per job.
    def at_1546(self, measure_slice_gib):
        settled = [rs.Instance(s, "measure", True, NOW, NOW - 590, mem_gib=7, cpu_cores=2.6, cpu_seconds=1500)
                   for s in (5, 7)]
        settling = [rs.Instance(s, "measure", True, NOW, NOW - 200, mem_gib=3.3, cpu_cores=2.6, cpu_seconds=520)
                    for s in (1, 2, 3)]
        r = rs.Readings(95, 88, {"measure": measure_slice_gib - 24}, 13, CORES, (), 300)
        return rs.decide(NO_LANE, rs.Bitcashier(0, 0), queued(measure=15), settled + settling, NOW, r)

    def test_1546_a_job_is_charged_the_cores_jobs_use_and_a_settling_one_only_what_it_has_not_shown(self):
        d = self.at_1546(measure_slice_gib=80)
        self.assertEqual(d.limits["measure_cores"], 2.6)  # the settled ones' median, not 4
        # settling: 3 x (11 - 3.3) GiB and 3 x (2.6 - 520 / 300) cores still owed
        # host: 88 - 22 reserved - 6 - 23.1 = 36.9 -> 3; CPU: (27.2 - 13 - 2.6) // 2.6 = 4
        self.assertEqual((d.limits["memory"], d.limits["cpu"]), (3, 4))
        # and the 80 GiB slice: 56 - 6 - 23.1 = 26.9 -> 2 -- five measurements peak at 12-16 GiB each (measured
        # 16:50Z), so it holds the eighth back: the slice, not CPU, binds now
        self.assertEqual((d.limits["slice"], d.room), (2, 2))

    def test_1546_with_a_96_gib_measure_slice_takes_three_more(self):
        d = self.at_1546(measure_slice_gib=96)  # 72 - 6 - 23.1 = 42.9 -> 3
        self.assertEqual(d.room, 3)
        self.assertEqual(d.starts, ((4, "measure"), (6, "measure"), (8, "measure")))

    def test_a_class_with_no_settled_runner_is_charged_its_ceiling(self):
        fresh = [rs.Instance(1, "measure", True, NOW, NOW - 10, cpu_cores=1.0)]
        self.assertEqual(rs.measured_cpus(CFG, fresh, "measure", NOW), 4)
        idle_ones = [rs.Instance(1, "measure", True, NOW, NOW - 900, cpu_cores=0.2)]
        self.assertEqual(rs.measured_cpus(CFG, idle_ones, "measure", NOW), CFG.job_cpus_floor)

    # 12:32-12:58Z: identity run 42370669779's timings job (label `movies`, 10 s, well under 1 GiB) queued 26 min behind
    # 20 measurements, every job charged a measurement's 11 GiB and 4 cores and light ranked last. Light jobs now
    # have their own lane at their own size.
    def test_a_light_job_starts_beside_a_measure_queue_the_cpu_holds_back(self):
        ours = [inst(s, "measure", busy=True) for s in range(1, 6)]
        r = readings(available=44, ci_free=44, class_free={"measure": 25}, cpu_busy=26, cores=CORES)
        d = rs.decide(CFG, rs.Bitcashier(6, 1), queued(measure=20, light=1), ours, NOW, r)
        self.assertEqual(d.starts, ((11, "light"),))  # (27.2 - 26) < 4 cores: no measurement; 1 GiB, 1 core: yes
        self.assertEqual(d.room, 0)

    def test_a_light_job_starts_while_bitcashier_is_saturated(self):
        r = readings(available=44, ci_free=44, class_free={"measure": 48}, cpu_busy=20, cores=CORES,
                     busy_builders=[5] * 12)
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(measure=20, light=1), [], NOW, r)
        self.assertEqual((d.room, d.starts), (0, ((11, "light"),)))

    def test_bitcashier_saturated_leaves_no_room(self):
        r = readings(available=92, ci_free=87, class_free={"measure": 55}, cpu_busy=10, cores=CORES,
                     busy_builders=[5] * 12)
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(measure=5), [], NOW, r)
        self.assertEqual((d.room, d.starts), (0, ()))

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


class CpusetAdmissionTest(unittest.TestCase):
    """Each class is admitted on the CPUs its slice may run on (cpuset.cpus.effective), not the host's. On ci-runner the
    measure slice holds CCD1 (plus two CCD0 cores) and the check slice the rest of CCD0, so a host-wide reading both
    over-admits measurements onto a full CCD1 beside an idle CCD0 (2026-11-08 04:36-06:00Z: nine and ten measurement
    runners on CCD1's 16 threads, ~1.8 cores each, Germany's drift rows 36-51 min against 14-15 with three) and holds
    them back for load on CPUs they cannot use: bitcashier's builders and the check lane's cores on CCD0 (15:20Z:
    host 22 of 32 busy, CCD1 10.2 of 16, 20 measurements queued and none started)."""

    def settled_measurements(self, n, cores_each):
        return [rs.Instance(s, "measure", True, NOW, NOW - 900, mem_gib=7, cpu_cores=cores_each, cpu_seconds=1500)
                for s in range(1, n + 1)]

    def test_measurements_start_on_their_own_idle_cpus_while_the_host_is_busy_elsewhere(self):
        # host 22 of 32 busy (bitcashier on both CCDs), the measure CPUs 8 of 20: 0.85 x 20 - 8 = 9 -> two 4-core jobs;
        # the host-wide reading said 27.2 - 22 - the lane's 8 < 0
        r = readings(cpu_busy=22, cores=CORES, class_cpu=on_cpusets(check_busy=12, measure_busy=8))
        d = rs.decide(CFG, IDLE, queued(measure=20), [], NOW, r)
        self.assertEqual([c for _, c in d.starts], ["measure", "measure"])
        self.assertEqual(d.limits["cpu"], 2)

    def test_a_full_measure_cpuset_starts_no_measurement_beside_an_idle_check_ccd(self):
        # six measurements at 2.6 cores fill their 20 threads (16 busy of 17 allowed); the host reads 16 of 32
        six = self.settled_measurements(6, cores_each=2.6)
        r = readings(cpu_busy=16, cores=CORES, class_cpu=on_cpusets(check_busy=0, measure_busy=16))
        d = rs.decide(CFG, IDLE, queued(measure=20), six, NOW, r)
        self.assertEqual((d.starts, d.limits["cpu"]), ((), 0))

    def test_the_check_lane_never_waits_on_measurements_that_fill_their_cpuset(self):
        # measurements fill theirs (19 of 20 busy) and bitcashier holds 5 of the check CPUs: host 24 of 32, so the
        # host-wide 27.2 - 24 left a check none of its 4 cores; its own CPUs hold it (10.2 - 5)
        eight = self.settled_measurements(8, cores_each=2.4)
        r = readings(cpu_busy=24, cores=CORES, class_cpu=on_cpusets(check_busy=5, measure_busy=19))
        d = rs.decide(CFG, IDLE, queued(check=1, measure=12), eight, NOW, r)
        self.assertEqual(d.starts, ((9, "check"),))

    def test_a_measurement_started_beside_a_check_takes_none_of_its_cores(self):
        # each class's cores are spent only on its own CPUs: 10.2 - 2 holds two checks, 17 - 4 three measurements
        r = readings(cpu_busy=6, cores=CORES, class_cpu=on_cpusets(check_busy=2, measure_busy=4))
        d = rs.decide(NO_LANE, IDLE, queued(check=2, measure=5), [], NOW, r)
        self.assertEqual([c for _, c in d.starts], ["check", "check", "measure", "measure", "measure"])

    def test_classes_on_shared_or_unknown_cpus_still_share_their_cores(self):
        self.assertTrue(rs.share_cpus(readings(class_cpu=on_cpusets(0, 0)), "measure", "light"))
        self.assertFalse(rs.share_cpus(readings(class_cpu=on_cpusets(0, 0)), "measure", "check"))
        self.assertTrue(rs.share_cpus(readings(), "measure", "check"))  # no cpuset read: the host's, shared
        # and with no cpusets the check lane's cores are kept from measurements as before
        self.assertEqual(len(rs.decide(CFG, IDLE, queued(measure=5), [], NOW,
                                       readings(cpu_busy=15, cores=CORES)).starts), 1)


class AfterTheCpusetDeployTest(unittest.TestCase):
    """2026-11-08 18:55-18:57Z, ci-runner's journal, an hour after the cpuset deploy: 60 measurements queued (the oldest
    252 min) and 4-5 landing checks (57 min), one to three runners of ours up, the measure CPUs 9-14 of 20 busy and the
    check CPUs 5-8 of 12 -- and both classes STARVED "held by cpu". With no settled runner of a class (the one settled
    measurement had just ended) a job was charged job_cpus, the 4-core ceiling, though measurements were using 1.5-3.2
    (the median 2.7 a tick before); and a landing check inside its lane was held by the CPU like any other job."""

    def at(self, check_busy, measure_busy, ours, lane_free):
        r = rs.Readings(111, 108, {"measure": 94}, check_busy + measure_busy, CORES, (4,) * 9, 300,
                        on_cpusets(check_busy=check_busy, measure_busy=measure_busy), {"measure": 96})
        d = rs.decide(CFG, rs.Bitcashier(9, 1), queued(check=4, measure=60), ours, NOW, r,
                      remembered_cpus={"measure": 2.7})
        self.assertEqual(d.limits["check_lane"], f"{lane_free} x 10 GiB 4 cores")
        return d

    def test_1856_a_lane_check_and_a_measurement_both_start(self):
        # slot 1: a measurement 90 s into its job at 1.1 cores; slot 2: a check that took its job 30 s ago
        ours = [rs.Instance(1, "measure", True, NOW, NOW - 90, mem_gib=2, cpu_cores=1.1, cpu_seconds=99),
                rs.Instance(2, "check", True, NOW, NOW - 30, mem_gib=0)]
        d = self.at(check_busy=5, measure_busy=10, ours=ours, lane_free=1)
        # was: 0 and 0 -- the measurement charged 4 - 0.33 owed and 4 per new one (17 - 10 - 3.67 < 4); the check 10.2 - 5
        # - 4 owed < 4. Now: 17 - 10 - (2.7 - 0.33) = 4.63 holds one at 2.7, and the lane's free check starts
        self.assertEqual(sorted(c for _, c in d.starts), ["check", "measure"])

    def test_1857_a_lane_check_and_a_measurement_both_start(self):
        # slot 3 took a measurement 30 s ago as well: 17 - 10 - (2.7 - 0.8) - 2.7 = 2.4 -- no third measurement at 2.7
        # beside two still owing, but the lane's check is never held by the CPU
        ours = [rs.Instance(1, "measure", True, NOW, NOW - 150, mem_gib=4, cpu_cores=1.6, cpu_seconds=240),
                rs.Instance(2, "check", True, NOW, NOW - 60, mem_gib=1, cpu_cores=0.6, cpu_seconds=36),
                rs.Instance(3, "measure", True, NOW, NOW - 30, mem_gib=0)]
        d = self.at(check_busy=5, measure_busy=10, ours=ours, lane_free=1)
        self.assertEqual([c for _, c in d.starts], ["check"])

    def test_a_check_past_the_lane_is_still_held_by_its_cpus(self):
        two = [rs.Instance(s, "check", True, NOW, NOW - 900, mem_gib=5, cpu_cores=4, cpu_seconds=3600) for s in (1, 2)]
        d = self.at(check_busy=10, measure_busy=0, ours=two, lane_free=0)
        self.assertNotIn("check", [c for _, c in d.starts])

    def test_the_cores_charged_outlive_a_scheduler_restart(self):
        root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, root)
        host = rs.Host(rs.Config(state_dir=os.path.join(root, "state")))
        self.assertEqual(host.remembered_cpus(), {})  # none yet: the ceiling
        host.remember_cpus({"check": 4, "measure": 2.7, "light": 1})
        self.assertEqual(rs.Host(rs.Config(state_dir=os.path.join(root, "state"))).remembered_cpus()["measure"], 2.7)
        with open(os.path.join(root, "state", "per-job-cores.json"), "w") as f:
            f.write("not json")
        self.assertEqual(host.remembered_cpus(), {})

    def test_a_class_with_no_settled_runner_is_charged_what_its_jobs_used_last(self):
        fresh = [rs.Instance(1, "measure", True, NOW, NOW - 10)]
        self.assertEqual(rs.measured_cpus(CFG, fresh, "measure", NOW, {"measure": 2.7}), 2.7)
        self.assertEqual(rs.measured_cpus(CFG, fresh, "measure", NOW, {}), 4)  # never measured: the ceiling


class SliceCapsTest(unittest.TestCase):
    """The owner's "cap = charge" (2026-11-08): a measurement is charged in its slice what its container may take
    (container_gib, its --memory), so admitted measurements never sum past the slice's memory.max, however little
    the running ones hold now."""

    def test_settled_measurements_holding_little_admit_no_more_than_the_slice_holds_caps(self):
        # 96 GiB slice, five measurements settled at 7 GiB: the working set leaves 61 - 6 = 55 GiB (five more of 11),
        # but six caps of 16 is the slice: one more
        five = [rs.Instance(s, "measure", True, NOW, NOW - 900, mem_gib=7, cpu_cores=2, cpu_seconds=1500)
                for s in range(1, 6)]
        r = readings(class_free={"measure": 96 - 35}, cpu_busy=0, cores=CORES)
        r = dataclasses.replace(r, class_max_gib={"measure": 96})
        d = rs.decide(NO_LANE, IDLE, queued(measure=20), five, NOW, r)
        self.assertEqual((len(d.starts), d.limits["caps"]), (1, 1))
        # six alive: none, and the log names the caps
        six = five + [rs.Instance(6, "measure", False, NOW, NOW - 30)]
        d = rs.decide(NO_LANE, IDLE, queued(measure=20), six, NOW, r)
        self.assertEqual((d.starts, d.room, d.limits["caps"]), ((), 0, 0))

    def test_a_slice_with_no_limit_has_no_caps(self):
        d = rs.decide(NO_LANE, IDLE, queued(check=3), [], NOW, readings(cpu_busy=0, cores=CORES))
        self.assertEqual(len(d.starts), 3)
        self.assertIsNone(d.limits["caps"])


def settled(r, starts, cpu_each=1.7):
    """The host a tick after `starts`: each new job settled at its class's expected size and `cpu_each` cores, out of
    the host's and ci.slice's free memory and into the busy cores."""
    gib = sum(CFG.job_expected_gib[c] for _, c in starts)
    return rs.Readings(r.mem_available_gib - gib, r.ci_free_gib - gib, dict(r.class_free_gib),
                       r.cpu_busy_cores + cpu_each * len(starts), r.cpu_cores, r.busy_builder_gib, r.cpu_span_seconds)


class CheckLaneTest(unittest.TestCase):
    """2026-11-08 05:45-05:50Z (ci-runner's journal): bitcashier 1/12 builders, 0/6 deployers. Measurements that ended
    were replaced at once while no landing check was queued (05:47:37: 'memory': 0, nine measurement runners); the check
    queued at 05:50 found 'room 0' -- memory 0 -- and waited for a measurement to end. agentctl's remote check gives up
    at 80 min: land entry ws3b bounced checking -> queued for ~2.5 h and WS4's first entry expired. Priority alone
    reserves nothing; the check lane holds `check_lane_jobs` checks' memory, cores and slots that a measurement never
    takes."""

    def before_the_refill(self):
        # 05:4x: four measurements and one landing check running (settled), seven measurements queued, no check
        ours = [rs.Instance(s, "measure", True, NOW, NOW - 900, mem_gib=6, cpu_cores=1.7, cpu_seconds=1500)
                for s in (1, 3, 8, 10)] + [rs.Instance(4, "check", True, NOW, NOW - 900, mem_gib=5, cpu_cores=1.7,
                                                       cpu_seconds=1500)]
        r = rs.Readings(91, 87, {"measure": 75}, 18, CORES, (5,), 300)
        return ours, r

    def test_a_check_queued_after_the_measurements_refilled_starts_at_once(self):
        ours, r = self.before_the_refill()
        bc = rs.Bitcashier(1, 0)
        refill = rs.decide(CFG, bc, queued(measure=7), ours, NOW, r)
        self.assertTrue(refill.starts and all(c == "measure" for _, c in refill.starts))
        # the next ticks: the refill settled, the running check ended, a new landing check queued
        running = [i for i in ours if i.cls == "measure"] + [
            rs.Instance(s, c, True, NOW, NOW - 900, mem_gib=CFG.job_expected_gib[c], cpu_cores=1.7, cpu_seconds=1500)
            for s, c in refill.starts]
        after = rs.decide(CFG, bc, queued(check=1, measure=3), running, NOW, settled(r, refill.starts))
        self.assertEqual(after.room, 0)  # no room for a measurement ...
        self.assertEqual([c for _, c in after.starts], ["check"])  # ... and the check starts anyway

    def test_measurements_never_start_into_the_check_reservation(self):
        # free memory past every reserve is the lane's two checks and one measurement less a GiB: no measurement
        lane = CFG.check_lane_jobs * CFG.job_expected_gib["check"]
        free = 4 * 4 + 6 + CFG.memory_margin_gib + lane + CFG.job_expected_gib["measure"] - 1
        r = readings(available=free, ci_free=free, cpu_busy=0, cores=CORES)
        d = rs.decide(CFG, IDLE, queued(measure=20), [], NOW, r)
        self.assertEqual((d.starts, d.room), ((), 0))
        self.assertIn("check_lane", d.limits)
        # one GiB more holds one
        r = readings(available=free + 1, ci_free=free + 1, cpu_busy=0, cores=CORES)
        self.assertEqual(rs.decide(CFG, IDLE, queued(measure=20), [], NOW, r).starts, ((1, "measure"),))

    def test_measurements_leave_the_lanes_slots_and_cores(self):
        # slots: eight measurements running, two slots left -- the lane's
        eight = [inst(s, "measure", busy=True) for s in range(1, 9)]
        self.assertEqual(rs.decide(CFG, IDLE, queued(measure=5), eight, NOW, ROOMY).starts, ())
        self.assertEqual(rs.decide(CFG, IDLE, queued(check=3), eight, NOW, ROOMY).starts,
                         ((9, "check"), (10, "check")))
        # cores: 27.2 - 15 = 12.2 spare; the lane's two checks at 4 leave 4.2, one 4-core measurement
        d = rs.decide(CFG, IDLE, queued(measure=5), [], NOW, readings(cpu_busy=15, cores=CORES))
        self.assertEqual(len(d.starts), 1)

    def test_a_running_check_fills_its_share_of_the_lane(self):
        # one check running: the lane holds back one more check, not two
        lane_one = 4 * 4 + 6 + CFG.memory_margin_gib + CFG.job_expected_gib["check"] + CFG.job_expected_gib["measure"]
        check = rs.Instance(1, "check", True, NOW, NOW - 900, mem_gib=5, cpu_cores=2, cpu_seconds=600)
        r = readings(available=lane_one, ci_free=lane_one, cpu_busy=2, cores=CORES)
        self.assertEqual(rs.decide(CFG, IDLE, queued(measure=5), [check], NOW, r).starts, ((2, "measure"),))

    def test_saturated_bitcashier_still_starts_nothing_heavy(self):
        r = readings(available=92, ci_free=87, class_free={"measure": 55}, cpu_busy=10, cores=CORES,
                     busy_builders=[5] * 12)
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(check=2, measure=5), [], NOW, r)
        self.assertEqual((d.room, d.starts), (0, ()))


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

    def test_landing_checks_then_measurements_and_light_jobs_in_their_own_lane(self):
        small = rs.Config(max_slots=3)
        d = rs.decide(small, IDLE, queued(light=2, measure=3, check=1), [], NOW, ROOMY)
        # slot 3 is the check lane's second check's: no measurement takes it
        self.assertEqual(d.starts, ((1, "check"), (2, "measure"), (4, "light"), (5, "light")))

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

    def test_the_light_lane_holds_its_own_slots_only(self):
        d = rs.decide(CFG, IDLE, queued(light=10), [], NOW, ROOMY)
        self.assertEqual(d.starts, ((11, "light"), (12, "light"), (13, "light")))
        self.assertEqual(d.room, CFG.max_slots - CFG.check_lane_jobs)  # the heavy slots are untouched

    def test_the_light_lane_never_starts_on_a_full_cpu(self):
        # 95 % of 32 = 30.4 cores: 30 busy leaves under one
        self.assertEqual(rs.decide(CFG, IDLE, queued(light=1), [], NOW, readings(cpu_busy=30, cores=CORES)).starts, ())
        self.assertEqual(rs.decide(CFG, IDLE, queued(light=1), [], NOW, readings(cpu_busy=None, cores=CORES)).starts, ())

    def test_the_light_lane_needs_its_size_free(self):
        # bitcashier saturated, 6.5 GiB less the 6 margin: half a light job
        r = readings(available=6.5, ci_free=6.5, cpu_busy=0, cores=CORES)
        self.assertEqual(rs.decide(CFG, rs.Bitcashier(12, 6), queued(light=1), [], NOW, r).starts, ())

    def test_bitcashier_saturated_keeps_an_idle_light_runner_for_its_queued_job(self):
        ours = [inst(1, "measure"), inst(11, "light")]
        d = rs.decide(CFG, rs.Bitcashier(12, 0), queued(measure=1, light=1), ours, NOW, ROOMY)
        self.assertEqual((d.stops, d.starts), ((1,), ()))

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


class StarvedTest(unittest.TestCase):
    """A class whose oldest queued job has waited past `starve_report_seconds` while nothing of it starts is said, with
    the limit that holds it -- Scala Steward #145 (2026-11-08) sat queued for an hour on movies-measure while every tick
    read `room 0` and only the full `held by` dict said why, so its stuck red named no cause."""

    def jobs(self, *specs):
        return [{"status": "queued", "labels": [label], "created_at": created} for label, created in specs]

    def test_the_oldest_queued_job_of_each_class_says_how_long_it_has_waited(self):
        now = rs.iso_seconds("2026-11-08T13:28:21Z")
        waits = rs.oldest_waits(self.jobs(("movies-measure", "2026-11-08T12:23:21Z"), ("movies-measure", "2026-11-08T13:00:00Z"),
                                          ("movies-check", "2026-11-08T13:27:21Z"), ("ubuntu-26.04", "2026-11-08T10:00:00Z")), now)
        self.assertEqual(waits, {"measure": 3900, "check": 60})

    def test_a_measurement_held_by_the_cpu_for_an_hour_is_said_with_its_cause(self):
        busy = readings(cpu_busy=31, cores=CORES)
        d = rs.decide(CFG, rs.Bitcashier(10, 0), queued(measure=4), [], NOW, busy)
        self.assertEqual(d.starts, ())
        self.assertEqual(rs.starved(CFG, d, queued(measure=4), {"measure": 3900}),
                         ["STARVED measure: 4 queued, the oldest 65 min, held by cpu"])

    def test_a_class_that_starts_or_has_not_waited_long_is_not_starved(self):
        d = rs.decide(CFG, IDLE, queued(measure=1), [], NOW, ROOMY)
        self.assertEqual(rs.starved(CFG, d, queued(measure=1), {"measure": 3900}), [])
        busy = rs.decide(CFG, rs.Bitcashier(10, 0), queued(measure=4), [], NOW, readings(cpu_busy=31, cores=CORES))
        self.assertEqual(rs.starved(CFG, busy, queued(measure=4), {"measure": 600}), [])

    def test_bitcashier_saturated_is_the_cause_it_names(self):
        d = rs.decide(CFG, rs.Bitcashier(CFG.builders, CFG.deployers), queued(measure=2), [], NOW, ROOMY)
        self.assertEqual(rs.starved(CFG, d, queued(measure=2), {"measure": 1800}),
                         ["STARVED measure: 2 queued, the oldest 30 min, held by bitcashier saturated"])


class ConfigTest(unittest.TestCase):
    def test_every_class_needs_a_size(self):
        with self.assertRaises(ValueError):
            rs.Config(job_expected_gib={"check": 11, "measure": 11})
        with self.assertRaises(ValueError):
            rs.Config(job_cpus={"check": 4, "measure": 4, "light": 1, "main": 4})

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
        if cmd[1] == "inspect":  # the container's id, which names its cgroup scope
            if self.top_fails:
                return rs.subprocess.CompletedProcess(cmd, 1, "", "Error: No such object\n")
            return rs.subprocess.CompletedProcess(cmd, 0, "c0ffee\n", "")
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
        return rs.Host(rs.Config(state_dir=state, class_cgroup=os.path.join(state, "cg", "ci-movies-{cls}.slice")))

    def container(self, host, current_gib, inactive_gib, cpu_seconds):
        d = os.path.join(host.cfg.class_cgroup.format(cls="check"), "docker-c0ffee.scope")
        os.makedirs(d, exist_ok=True)
        for name, text in (("memory.current", str(current_gib * GIB)),
                           ("memory.stat", f"anon 1\ninactive_file {inactive_gib * GIB}\n"),
                           ("cpu.stat", f"usage_usec {int(cpu_seconds * 1e6)}\nuser_usec 1\n")):
            with open(os.path.join(d, name), "w") as f:
                f.write(text)

    def test_a_runner_shows_its_containers_own_memory_and_cpu(self):
        # 15:46Z: each measurement was charged 4 cores and 11 GiB while it already showed in the readings
        host = self.host(FakeDocker(["Runner.Worker"]))
        self.container(host, current_gib=7, inactive_gib=2, cpu_seconds=100)
        [first] = host.instances(NOW)
        self.assertEqual((first.mem_gib, first.cpu_cores, first.cpu_seconds), (5, None, 100))  # one sample: no rate
        self.container(host, current_gib=8, inactive_gib=2, cpu_seconds=178)
        [i] = host.instances(NOW + 30)
        self.assertEqual((i.mem_gib, i.cpu_seconds), (6, 178))
        self.assertAlmostEqual(i.cpu_cores, 2.6)  # 78 s of CPU in 30 s

    def test_a_runner_with_no_container_shows_nothing(self):
        with unittest.mock.patch("sys.stderr", new_callable=io.StringIO):
            [i] = self.host(FakeDocker([], top_fails=True)).instances(NOW)
        self.assertEqual((i.mem_gib, i.cpu_cores, i.cpu_seconds), (None, None, None))

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

    def process(self, cgroup_dir, pid, comm):
        with open(os.path.join(cgroup_dir, "cgroup.procs"), "a") as f:
            f.write(f"{pid}\n")
        os.makedirs(os.path.join(self.root, "proc", str(pid)), exist_ok=True)
        with open(os.path.join(self.root, "proc", str(pid), "comm"), "w") as f:
            f.write(comm + "\n")

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
        # three builders: two run a job (a Runner.Worker among their processes), one idle with a big cache
        for n, current, comm in ((1, 5, "Runner.Worker"), (2, 3, "Runner.Worker"), (3, 40, "Runner.Listener")):
            d = self.cgroup(f"ci.slice/ci-check.slice/builder-{n}.service", 0, current * GIB, 1 * GIB)
            self.process(d, 100 + n, comm)
        self.proc(58 * 2**20, user=0, idle=0)
        cpu = rs.CpuMeters(window_seconds=300)
        r = rs.read_readings(self.cfg(), cpu, NOW)
        self.assertEqual(r.mem_available_gib, 58)
        self.assertEqual(r.ci_free_gib, 76)
        self.assertEqual(r.class_free_gib, {"measure": 55})
        self.assertEqual(r.class_max_gib, {"measure": 80})  # the check slice's "max" is no limit
        self.assertEqual(sorted(r.busy_builder_gib), [2, 4])  # the busy units' own working sets; the idle one's not
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

    def test_a_cpu_list_reads_as_its_set(self):
        self.assertEqual(rs.parse_cpu_list("6-15,22-31\n"), frozenset(list(range(6, 16)) + list(range(22, 32))))
        self.assertEqual(rs.parse_cpu_list("3\n"), frozenset({3}))
        self.assertEqual(rs.parse_cpu_list(""), frozenset())

    def test_each_class_reads_the_busy_cores_of_its_own_cpuset(self):
        # four CPUs: the check slice on 0-1, measure and light on 2-3; over the window CPU 0 is idle, 1 half busy,
        # 2 and 3 fully busy -- the measure CPUs 2 of 2, the check CPUs 0.5 of 2, the host 2.5 of 4
        slices = "ci.slice/ci-movies.slice/ci-movies-{}.slice"
        for cls, text in (("check", "0-1"), ("measure", "2-3"), ("light", "2-3")):
            d = self.cgroup(slices.format(cls), 0, 0, 0)
            with open(os.path.join(d, "cpuset.cpus.effective"), "w") as f:
                f.write(text + "\n")
        self.cgroup("ci.slice", 115 * GIB, 0, 0)
        stat = os.path.join(self.root, "proc", "stat")

        def write_stat(per_cpu):
            self.proc(58 * 2**20, user=0, idle=0)
            total = [sum(x) for x in zip(*per_cpu)]
            with open(stat, "w") as f:
                f.write(f"cpu  {total[0]} 0 0 {total[1]} 0 0 0 0 0 0\n")
                for n, (busy, idle) in enumerate(per_cpu):
                    f.write(f"cpu{n} {busy} 0 0 {idle} 0 0 0 0 0 0\n")

        cpu = rs.CpuMeters(window_seconds=300)
        write_stat([(0, 0)] * 4)
        first = rs.read_readings(self.cfg(), cpu, NOW - 30)
        self.assertIsNone(first.class_cpu["measure"].busy_cores)  # one sample: no rate yet
        write_stat([(0, 100), (50, 50), (100, 0), (100, 0)])
        r = rs.read_readings(self.cfg(), cpu, NOW)
        self.assertEqual(r.class_cpu["measure"].cpus, frozenset({2, 3}))
        self.assertAlmostEqual(r.class_cpu["measure"].busy_cores, 2.0)
        self.assertAlmostEqual(r.class_cpu["check"].busy_cores, 0.5)
        self.assertAlmostEqual(r.cpu_busy_cores, 2.5)
        self.assertEqual(len(cpu.sets), 2)  # measure and light share one meter


if __name__ == "__main__":
    unittest.main()
