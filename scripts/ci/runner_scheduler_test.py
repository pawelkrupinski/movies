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


def inst(slot, cls, busy=False, idle_for=0, started_ago=3600, cpu_cores="cap"):
    """A runner of ours; settled, its container read at its class's whole CPU cap (job_cpus) unless told otherwise, so
    it owes the CPU reading nothing beyond what that reading already holds."""
    cores = CFG.job_cpus[cls] if cpu_cores == "cap" else cpu_cores
    return rs.Instance(slot, cls, busy, NOW - idle_for, NOW - started_ago, cpu_cores=cores)


def readings(available=500, ci_free=500, class_free=None, cpu_busy=0.0, cores=1000, busy_builders=(), class_cpu=None):
    return rs.Readings(available, ci_free, dict(class_free or {}), cpu_busy, cores, tuple(busy_builders),
                       class_cpu=dict(class_cpu or {}))


def cpus(text):
    return rs.parse_cpu_list(text)


# ci-runner's slices until 2026-11-17 (bitcashier infra, movies-runners.nix), which the recordings below ran on: checks
# on CCD0 less two cores, measurements and light jobs on CCD1 plus those two cores (6-7 and their hyperthreads 22-23)
CHECK_CPUS, MEASURE_CPUS = cpus("0-5,16-21"), cpus("6-15,22-31")
# and the config they ran under: a check charged 4 cores, at most 2 borrowed, and no peak below the 4-core charge
BEFORE_A1 = dataclasses.replace(CFG, job_cpus={"check": 4, "measure": 4, "light": 1}, borrow_max_checks=2,
                                check_peak_cpus=4.0)
# since 2026-11-17 (the owner's "A2"): checks on CCD0 and CCD1's first core (0-8,16-24), measurements and light jobs on
# CCD1's other seven (9-15,25-31)
CHECK_CPUS_A2, MEASURE_CPUS_A2 = cpus("0-8,16-24"), cpus("9-15,25-31")


def on_cpusets(check_busy, measure_busy, check_cpus=CHECK_CPUS, measure_cpus=MEASURE_CPUS):
    return {"check": rs.ClassCpu(check_cpus, check_busy), "measure": rs.ClassCpu(measure_cpus, measure_busy),
            "light": rs.ClassCpu(measure_cpus, measure_busy)}


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

    def test_1546_every_settling_runner_is_charged_its_cap_less_what_it_has_shown(self):
        d = self.at_1546(measure_slice_gib=80)
        # settling: 3 x (11 - 3.3) GiB still owed; CPU: each settling runner its 4-core cap less what the reading holds
        # of it, 3 x (4 - 520 / 300), and the settled ones the largest rest of a cap, 4 - 2.6: 8.2 owed
        # host: 88 - 22 reserved - 6 - 23.1 = 36.9 -> 3; CPU: (27.2 - 13 - 8.2) // 4 = 1
        self.assertEqual(d.limits["owed_cores"], 8.2)
        self.assertEqual((d.limits["memory"], d.limits["cpu"]), (3, 1))
        # and the 80 GiB slice: 56 - 6 - 23.1 = 26.9 -> 2; the CPU binds first
        self.assertEqual((d.limits["slice"], d.room), (2, 1))

    def test_1546_with_a_96_gib_measure_slice_takes_what_the_cpu_holds(self):
        d = self.at_1546(measure_slice_gib=96)  # the slice 72 - 6 - 23.1 = 42.9 -> 3; the CPU 1
        self.assertEqual(d.room, 1)
        self.assertEqual(d.starts, ((4, "measure"),))

    def test_a_runner_owes_its_cap_less_what_the_cpu_reading_holds_of_it(self):
        r = rs.Readings(100, 100, {}, 0, CORES, (), 300)
        # settled: its window's cores; settling: its CPU time over the span (90 s / 300 s); never read: the whole cap
        self.assertAlmostEqual(rs.cpu_charge(CFG, r, rs.Instance(1, "measure", True, NOW, NOW - 900, cpu_cores=1.3),
                                             NOW), 2.7)
        self.assertAlmostEqual(rs.cpu_charge(CFG, r, rs.Instance(1, "measure", True, NOW, NOW - 100, cpu_cores=1.0,
                                                                 cpu_seconds=90), NOW), 3.7)
        self.assertEqual(rs.cpu_charge(CFG, r, rs.Instance(1, "check", True, NOW, NOW - 900), NOW), 3)
        # past its cap (GC threads): all of it is in the reading, it owes nothing
        self.assertEqual(rs.cpu_charge(CFG, r, rs.Instance(1, "measure", True, NOW, NOW - 900, cpu_cores=4.6), NOW), 0)
        self.assertEqual(rs.cpu_charge(CFG, r, rs.Instance(1, "light", True, NOW, NOW - 900, cpu_cores=0.2), NOW), 0.8)

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
        self.assertEqual(d.starts, ())
        self.assertLessEqual(d.limits["cpu"], 0)

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
        d = rs.decide(BEFORE_A1, rs.Bitcashier(9, 1), queued(check=4, measure=60), ours, NOW, r)
        self.assertEqual(d.limits["check_lane"], f"{lane_free} x 10 GiB 4 cores")
        return d

    def test_1856_a_lane_check_starts_and_a_measurement_waits_for_its_whole_cap(self):
        # slot 1: a measurement 90 s into its job at 1.1 cores; slot 2: a check that took its job 30 s ago
        ours = [rs.Instance(1, "measure", True, NOW, NOW - 90, mem_gib=2, cpu_cores=1.1, cpu_seconds=99),
                rs.Instance(2, "check", True, NOW, NOW - 30, mem_gib=0)]
        d = self.at(check_busy=5, measure_busy=10, ours=ours, lane_free=1)
        # 17 - 10 - (4 - 0.33) = 3.33: a measurement's 4 cores do not fit beside the ~9.7 cores of other load (the
        # 2.7 its jobs showed on a contended cpuset is not what it burns); the lane's check is never held by the CPU
        self.assertEqual([c for _, c in d.starts], ["check"])

    def test_1857_a_lane_check_starts_beside_two_measurements_still_owing(self):
        ours = [rs.Instance(1, "measure", True, NOW, NOW - 150, mem_gib=4, cpu_cores=1.6, cpu_seconds=240),
                rs.Instance(2, "check", True, NOW, NOW - 60, mem_gib=1, cpu_cores=0.6, cpu_seconds=36),
                rs.Instance(3, "measure", True, NOW, NOW - 30, mem_gib=0)]
        d = self.at(check_busy=5, measure_busy=10, ours=ours, lane_free=1)
        self.assertEqual([c for _, c in d.starts], ["check"])

    def test_a_check_past_the_lane_is_still_held_by_its_cpus(self):
        two = [rs.Instance(s, "check", True, NOW, NOW - 900, mem_gib=5, cpu_cores=4, cpu_seconds=3600) for s in (1, 2)]
        d = self.at(check_busy=10, measure_busy=0, ours=two, lane_free=0)
        # none on the checks' own CPUs (it may borrow the idle measure ones ahead of optional measurements: A3)
        self.assertEqual([s for s, c in d.starts if c == "check" and s not in d.borrowed], [])


class HalfIdleCpusetsTest(unittest.TestCase):
    """2026-11-15/16, 24 h of ci-runner's journal: measurements queued on 1569 ticks, 1183 of them held by the CPU,
    while the measure cpuset read 4-10 of its 20 threads busy (p10-p90; never past 15) and memory held 2-6 more. Every
    settled runner was charged its whole 4-core cap though a settled measurement used 2.2 cores at the median and a
    check 2.8 (the owner: "allow even more in parallel as long as mem and cpu are not maxed out"). A settled runner is
    in the reading; only one of them ramping up is charged past it."""

    def at(self, check_busy, measure_busy, available, ci_free, measure_free, ours, demand, bc=0, cfg=BEFORE_A1):
        r = rs.Readings(available, ci_free, {"measure": measure_free}, check_busy + measure_busy, CORES, (4.0,) * bc,
                        300, on_cpusets(check_busy=check_busy, measure_busy=measure_busy), {"measure": 96})
        return rs.decide(cfg, rs.Bitcashier(bc, 0), demand, ours, NOW, r)

    @staticmethod
    def settled(slot, cls, mem, cores):
        return rs.Instance(slot, cls, True, NOW, NOW - 900, mem_gib=mem, cpu_cores=cores, cpu_seconds=cores * 900)

    def test_023235_a_fifth_measurement_starts_on_a_half_idle_cpuset(self):
        # 02:32:35Z: bitcashier idle, 56 measurements queued, two checks (4.4, 3.4 cores) and four measurements (1.5,
        # 2.7, 2.7, 1.6) all settled; check CPUs 8/12, measure CPUs 9/20. Was: 17 - 9 - (2.5 + 1.3 + 1.3 + 2.4) < 4,
        # 'cpu': 0 -- now 17 - 9 - 2.5 = 5.5 holds one more, and the slice's caps (96 // 16 - 4) a second
        ours = [self.settled(1, "check", 7, 4.4), self.settled(2, "check", 6, 3.4),
                *(self.settled(s, "measure", m, c) for s, m, c in ((3, 2, 1.5), (4, 4, 2.7), (5, 4, 2.7), (6, 3, 1.6)))]
        d = self.at(8, 9, 95, 82, 83, ours, queued(measure=56))
        self.assertEqual((d.limits["cpu"], d.limits["caps"]), (1, 2))
        self.assertEqual(d.starts, ((7, "measure"),))

    def test_the_new_one_holds_the_next_back_until_it_shows(self):
        # the tick after: the fifth one is settling and owes its whole cap, 17 - 9.2 - 4 - 2.5 < 4
        ours = [self.settled(s, "measure", 4, 2.2) for s in (3, 4, 5, 6)] + \
               [rs.Instance(7, "measure", True, NOW, NOW - 30, mem_gib=1, cpu_cores=0.5, cpu_seconds=15)]
        d = self.at(4, 9.2, 95, 82, 70, ours, queued(measure=55))
        self.assertEqual(d.starts, ())

    def test_124239_checks_past_their_cap_on_a_full_cpuset_still_hold_a_third(self):
        # 12:42:39Z: two settled checks at 5.2 cores each, the check CPUs 11 of 12 busy: saturated, nothing starts
        ours = [self.settled(1, "check", 6, 5.2), self.settled(3, "check", 6, 5.2),
                *(self.settled(s, "measure", 8, 2.2) for s in (2, 4, 5))]
        d = self.at(11, 8, 79, 64, 66, ours, queued(check=5), bc=1)
        self.assertEqual(d.starts, ())
        self.assertEqual(rs.starved(CFG, d, queued(check=5), {"check": 1200}),
                         ["STARVED check: 5 queued, the oldest 20 min, held by cpu"])

    def test_a_starved_check_names_its_own_cpus_not_the_idle_measure_ones(self):
        # 483 ticks of 2026-11-15/16 said "STARVED check: ... held by nothing named": the cause named was a
        # measurement's terms, and the measure cpuset had room while the check one was full
        # (since 2026-11-16 that check borrows the idle measure CPUs: BorrowedCheckTest; here without borrowing)
        ours = [self.settled(1, "check", 6, 5.0), self.settled(2, "check", 6, 5.0)]
        d = self.at(11, 2, 100, 95, 90, ours, queued(check=3), cfg=dataclasses.replace(BEFORE_A1, borrow_max_checks=0))
        self.assertEqual(rs.starved(CFG, d, queued(check=3), {"check": 1500}),
                         ["STARVED check: 3 queued, the oldest 25 min, held by cpu"])


LANDING_ROW = {"movies-landing": 1}  # one queued measurement, a landing check's row


class BorrowedCheckTest(unittest.TestCase):
    """The owner, 2026-11-16: landing checks borrow the measure cpuset's idle CPUs (4 / 7 / 10 of its 20 threads busy at
    p10 / p50 / p90 over 2026-11-15/16) while their own 12 are full. The 2026-11-03 split stays the rule it was made
    for: a check's test phase went 6 -> 16 min beside measurements on shared cores. So a check borrows only cores no
    job of ours can grow into (every runner on the measure cpuset at its whole cap, plus the check's, within
    cpu_start_below of it, and the live reading holding it too), only while no measurement waits without a runner,
    never more than borrow_max_checks, and always leaving measure_reserve_jobs measurements' cores, memory, slice caps
    and slots free."""

    def at(self, ours, demand, check_busy=11, measure_busy=3, measure_free=80, cfg=BEFORE_A1, lanes=None):
        r = rs.Readings(100, 95, {"measure": measure_free}, check_busy + measure_busy, CORES, (), 300,
                        on_cpusets(check_busy=check_busy, measure_busy=measure_busy), {"measure": 96})
        return rs.decide(cfg, IDLE, demand, ours, NOW, r, lanes)

    @staticmethod
    def settled(slot, cls, cores, pool=""):
        return rs.Instance(slot, cls, True, NOW, NOW - 900, mem_gib=6, cpu_cores=cores, cpu_seconds=cores * 900,
                           slice_class=pool)

    def two_full_checks(self):
        # 12:42:39Z (2026-11-16): two settled checks at 5.2 cores each fill their 12 threads; a third waited 20 min
        return [self.settled(1, "check", 5.2), self.settled(2, "check", 5.2)]

    def test_a_check_its_cpus_cannot_hold_starts_on_the_idle_measure_cpus(self):
        d = self.at(self.two_full_checks(), queued(check=3))
        self.assertEqual((d.starts, d.borrowed), (((3, "check"), (4, "check")), (3, 4)))

    def test_a_check_borrows_only_once_every_queued_landing_measurement_has_its_runner(self):
        # one landing check's measurement queued and started: one check borrows what is left past it and the kept one
        d = self.at(self.two_full_checks(), queued(check=3, measure=1), lanes=LANDING_ROW)
        self.assertEqual((d.starts, d.borrowed), (((3, "measure"), (4, "check")), (4,)))
        # one held by its slice (10.5 GiB past the margin: a check's 10 fit, a measurement's 11 not): no check takes
        # its place, even with nothing kept for measurements
        no_reserve = dataclasses.replace(BEFORE_A1, measure_reserve_jobs=0)
        d = self.at(self.two_full_checks(), queued(check=3, measure=1), measure_free=16.5, cfg=no_reserve,
                    lanes=LANDING_ROW)
        self.assertEqual(d.starts, ())

    def test_a_check_borrows_ahead_of_an_optional_measurement(self):
        # the owner, 2026-11-17 ("A3"): the same tick with the measurement an optional one (movies-measure) -- the
        # checks borrow first, and the measurement then finds no room past the one kept for a landing's row
        d = self.at(self.two_full_checks(), queued(check=3, measure=1))
        self.assertEqual((d.starts, d.borrowed), (((3, "check"), (4, "check")), (3, 4)))

    def test_optional_measurements_leave_the_kept_room_while_a_check_is_borrowed(self):
        # one borrowed check settled at 2 cores, 5 of 20 busy: 17 - 5 - 2 (its rest) = 10 holds two 4-core
        # measurements, but one of them is kept for a landing's row: one optional measurement starts
        ours = self.two_full_checks() + [self.settled(3, "check", 2.0, pool="measure")]
        d = self.at(ours, queued(measure=5), measure_busy=5)
        self.assertEqual(d.starts, ((4, "measure"),))
        # a landing's rows take the kept room too
        d = self.at(ours, queued(measure=5), measure_busy=5, lanes={"movies-landing": 5})
        self.assertEqual(d.starts, ((4, "measure"), (5, "measure")))

    def test_every_job_on_the_measure_cpus_is_counted_at_its_cap_for_a_borrow(self):
        # two measurements reading 1 core each on a quiet cpuset: 2 x 4 + 4 for the check + 4 kept for a measurement
        # = 16 of 17 holds one; three: 20 > 17, none, however idle the reading
        two = [self.settled(s, "measure", 1.0) for s in (3, 4)]
        self.assertEqual(self.at(self.two_full_checks() + two, queued(check=3)).borrowed, (5,))
        three = two + [self.settled(5, "measure", 1.0)]
        self.assertEqual(self.at(self.two_full_checks() + three, queued(check=3)).borrowed, ())

    def test_a_busy_measure_reading_holds_a_borrow_back(self):
        # nothing of ours there, but 10 of 20 busy (bitcashier, a light job): 17 - 10 - 4 kept = 3 < 4
        self.assertEqual(self.at(self.two_full_checks(), queued(check=3), measure_busy=10).borrowed, ())

    def test_never_more_than_borrow_max_checks(self):
        borrowed = [self.settled(s, "check", 2.0, pool="measure") for s in (3, 4)]
        self.assertEqual(self.at(self.two_full_checks() + borrowed, queued(check=3)).starts, ())

    def test_borrowed_checks_never_hold_the_check_lane(self):
        # 2026-11-17 09:23Z: one check at home (0.4 cores), two borrowed, the check cpuset 3 of 12 busy, 6 checks queued
        # 21 min "held by cpu" with check_lane '0 x': the lane counted the borrowed checks, which run on the measure
        # CPUs, so the checks' own 12 threads sat idle. The lane is the checks' own cpuset's: one more starts there.
        home = rs.Instance(4, "check", True, NOW, NOW - 120, mem_gib=1, cpu_cores=0.4, cpu_seconds=48)
        borrowed = [self.settled(s, "check", 2.0, pool="measure") for s in (2, 3)]
        d = self.at([home] + borrowed, queued(check=6), check_busy=3, measure_busy=7)
        self.assertEqual((d.starts, d.borrowed), (((1, "check"),), ()))

    def test_a_borrowed_check_is_charged_its_whole_cap_to_measurements(self):
        # one borrowed check reading 1 core on 4 busy: measurements see 17 - 4 - 3 (its rest of 4) = 10, two -- not
        # 17 - 4 = 13, three
        d = self.at([self.settled(3, "check", 1.0, pool="measure")], queued(measure=5), check_busy=2, measure_busy=4)
        self.assertEqual(d.limits["cpu"], 2)

    def test_a_measurement_queued_after_a_borrow_starts_on_the_next_tick(self):
        # the bound: a borrow leaves one measurement's cores, memory, slice cap and slot, so the first measurement queued
        # beside borrowed checks never waits for one to end
        first = self.at(self.two_full_checks(), queued(check=3))
        ours = self.two_full_checks() + [rs.Instance(s, "check", True, NOW, NOW - 30, mem_gib=2, cpu_cores=1,
                                                     cpu_seconds=30, slice_class="measure") for s in first.borrowed]
        d = self.at(ours, queued(check=1, measure=4), measure_busy=4, measure_free=60,
                    lanes={"movies-red-fix": 1, "movies-measure": 3})
        self.assertEqual((d.starts, d.labels), (((5, "measure"),), ((5, "movies-red-fix"),)))

    def test_an_idle_borrowed_runner_gives_way_to_a_queued_landing_measurement(self):
        idle = rs.Instance(3, "check", False, NOW, NOW - 60, slice_class="measure")
        d = self.at(self.two_full_checks() + [idle], queued(check=1, measure=1), lanes=LANDING_ROW)
        self.assertIn(3, d.stops)

    def test_an_idle_borrowed_runner_stays_for_an_optional_measurement(self):
        idle = rs.Instance(3, "check", False, NOW, NOW - 60, slice_class="measure")
        d = self.at(self.two_full_checks() + [idle], queued(check=1, measure=1))
        self.assertNotIn(3, d.stops)

    def test_an_idle_borrowed_runner_stays_while_every_queued_measurement_has_its_runner(self):
        # the measurement's own runner waits for it (GitHub hands it over within the tick): stopping the borrowed check's
        # runner frees nothing a measurement needs, leaves its check queued and starts it again next tick, and races
        # GitHub handing it that check
        idle = rs.Instance(3, "check", False, NOW, NOW - 60, slice_class="measure")
        waiting = rs.Instance(4, "measure", False, NOW, NOW - 20)
        d = self.at(self.two_full_checks() + [idle, waiting], queued(check=1, measure=1))
        self.assertNotIn(3, d.stops)

    def test_no_borrow_where_the_classes_share_their_cpus(self):
        r = readings(cpu_busy=0, cores=CORES)
        d = rs.decide(CFG, IDLE, queued(check=5), self.two_full_checks() + [self.settled(3, "check", 5)], NOW, r)
        self.assertEqual(d.borrowed, ())


class ChargedCheckTest(unittest.TestCase):
    """The owner, 2026-11-17 ("A1", "A2"): checks waited ~20 min for a slot and ran ~10. Their 12 threads held three,
    each charged its JVM's 4 cores, though a settled check used 2.6 at the median (ci-runner's journal, 2026-11-15..17,
    6297 busy readings); so a check is charged 3, on 18 threads (0-8,16-24) -- the measurements keep CCD1's other 14.
    The p90 holds the line: a check burns 5.7 cores at its p90 (two at once 9.3, three 13.9), so its own cpuset never
    holds more than its threads fit at check_peak_cpus, and the live reading still stops every start at
    cpu_start_below."""

    def at(self, ours, n, check_busy, measure_busy=7, cfg=CFG, check_cpus=CHECK_CPUS_A2, measure_cpus=MEASURE_CPUS_A2):
        r = rs.Readings(111, 101, {"measure": 95}, check_busy + measure_busy, CORES, (4.0,), 300,
                        on_cpusets(check_busy, measure_busy, check_cpus, measure_cpus), {"measure": 96})
        return rs.decide(cfg, rs.Bitcashier(1, 0), queued(check=n), ours, NOW, r)

    @staticmethod
    def check(slot, cores):
        return rs.Instance(slot, "check", True, NOW, NOW - 900, mem_gib=4, cpu_cores=cores, cpu_seconds=cores * 900)

    def home_starts(self, d):
        return [s for s, c in d.starts if c == "check" and s not in d.borrowed]

    def test_150558_seven_checks_queued_beside_two_settled_ones(self):
        # 2026-11-17 15:05:58Z: checks 3.6 and 1.8 cores, the check CPUs 8 of 12 busy, 7 checks queued 22 min "held by
        # cpu": 10.2 - 8 - (4 - 1.8) < 4, none. On 18 threads at 3 each: 15.3 - 8 - 1.2 = 6.1 holds two more
        ours = [self.check(1, 3.6), self.check(2, 1.8)]
        before = self.at(ours, 7, check_busy=8, cfg=dataclasses.replace(BEFORE_A1, borrow_max_checks=0),
                         check_cpus=CHECK_CPUS, measure_cpus=MEASURE_CPUS)
        self.assertEqual(self.home_starts(before), [])
        self.assertEqual(self.home_starts(self.at(ours, 7, check_busy=8)), [3, 4])

    def test_a_landings_burst_starts_no_more_than_the_threads_hold_at_the_p90(self):
        # a landing queues its check jobs at once, the check CPUs idle: 15.3 // 3 would start five (~23 cores at the
        # p90 on 18 threads); 18 // 4.5 starts four
        d = self.at([], 6, check_busy=0.5)
        self.assertEqual(self.home_starts(d), [1, 2, 3, 4])
        self.assertEqual(d.limits["check"]["peak"], 4)

    def test_checks_reading_their_p90_hold_the_next_back(self):
        # three settled checks reading 13.9 cores together: 15.3 - 13.9 < 3, the reading holds a fourth back
        ours = [self.check(s, c) for s, c in ((1, 4.6), (2, 4.6), (3, 4.7))]
        self.assertEqual(self.home_starts(self.at(ours, 3, check_busy=14.5)), [])

    def test_on_the_old_12_threads_the_peak_holds_two(self):
        # why A2 lands with (never after) A1: charged 3 on 12 threads, the p90 would hold only the lane's two
        d = self.at([self.check(1, 2.6), self.check(2, 2.6)], 3, check_busy=5.5, cfg=dataclasses.replace(CFG, borrow_max_checks=0),
                    check_cpus=CHECK_CPUS, measure_cpus=MEASURE_CPUS)
        self.assertEqual(self.home_starts(d), [])

    def test_more_than_two_borrow_where_the_measure_cpus_hold_them(self):
        # borrow_max_checks 4 (A3: more than 2): on the old 20 measure threads, idle, four borrow -- 4 x 3 + 4 kept
        # = 16 of 17 -- and the cap holds a fifth. On A2's 14 the cores and the kept measurement allow two: 3 x 3 + 4
        # = 13 > 11.9.
        full = [self.check(1, 5.2), self.check(2, 5.2)]
        d = self.at(full, 6, check_busy=11, measure_busy=1, cfg=dataclasses.replace(CFG, check_peak_cpus=4.0),
                    check_cpus=CHECK_CPUS, measure_cpus=MEASURE_CPUS)
        self.assertEqual(d.borrowed, (3, 4, 5, 6))
        self.assertEqual(self.at(full, 6, check_busy=17, measure_busy=1).borrowed, (3, 4))


class DriftBurstTest(unittest.TestCase):
    """2026-11-09 20:32-20:34Z, ci-runner's journal: two dispatches queued 12 identity drift rows for the measure
    cpuset (20 threads, ~11-12 of them busy with other load). Each row's JVM may burn 4 cores (ActiveProcessorCount=4)
    but every one was charged the 1.3 its contended predecessors had shown: six started in two minutes, the cpuset read
    20/20 by 20:40, and a UK drift row took 900 s (527 s on a quiet ci-runner, 375 s hosted). Charged its cap, each
    runner owes the rest of its 4 cores until the reading shows them."""

    def at(self, bc, available, ci_free, slice_free, check_busy, measure_busy, ours, measure_queued):
        r = rs.Readings(available, ci_free, {"measure": slice_free}, check_busy + measure_busy, CORES, (4.0,) * bc,
                        300, on_cpusets(check_busy=check_busy, measure_busy=measure_busy), {"measure": 96})
        return rs.decide(CFG, rs.Bitcashier(bc, 0), queued(measure=measure_queued), ours, NOW, r)

    def test_203234_the_first_row_starts(self):
        # 17 - 12 = 5 holds one 4-core row (was: 3 at 1.3; memory held it to 1)
        d = self.at(6, 73, 78, 96, check_busy=8, measure_busy=12, ours=[], measure_queued=12)
        self.assertEqual((d.limits["cpu"], d.starts), (1, ((1, "measure"),)))

    def test_203304_no_second_row_beside_one_still_owing_its_cores(self):
        # slot 1 took its row 30 s ago and shows none of it yet: 17 - 11 - 4 = 2 (was: 17 - 11 - 1.3 -> 3 more started)
        ours = [rs.Instance(1, "measure", True, NOW, NOW - 30, mem_gib=0)]
        d = self.at(4, 100, 98, 95, check_busy=7, measure_busy=11, ours=ours, measure_queued=11)
        self.assertEqual(d.starts, ())

    def test_203434_five_young_rows_at_half_a_core_hold_a_sixth_back(self):
        # five rows 1-2 min old showing ~0.5 cores each on 8 of 20 busy: they owe 5 x ~3.5 (was: a sixth started)
        ours = [rs.Instance(s, "measure", True, NOW, NOW - 60 - 30 * s, mem_gib=1, cpu_cores=0.5, cpu_seconds=15)
                for s in range(1, 6)]
        d = self.at(10, 107, 100, 90, check_busy=5, measure_busy=8, ours=ours, measure_queued=7)
        self.assertEqual(d.starts, ())

    def test_settled_rows_reading_below_their_cap_owe_one_of_them_ramping_up(self):
        # three rows past settle_seconds at 1.3 cores each on 10 of 20: one may still ramp to its cap, 17 - 10 - 2.7 =
        # 4.3 holds one more (was: every settled row its whole cap, 17 - 10 - 3 x 2.7 < 4: none) -- and a second
        # waits for the first's 4 cores to show (was: (17 - 10) // 1.3 = 5 at the used median)
        ours = [rs.Instance(s, "measure", True, NOW, NOW - 900, mem_gib=8, cpu_cores=1.3, cpu_seconds=1200)
                for s in (1, 2, 3)]
        d = self.at(4, 100, 98, 60, check_busy=7, measure_busy=10, ours=ours, measure_queued=9)
        self.assertEqual(d.starts, ((4, "measure"),))


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
        # 05:4x: three measurements and one landing check running (settled, near their 4-core caps), seven
        # measurements queued, no check
        ours = [rs.Instance(s, "measure", True, NOW, NOW - 900, mem_gib=6, cpu_cores=3.6, cpu_seconds=1500)
                for s in (1, 3, 8)] + [rs.Instance(4, "check", True, NOW, NOW - 900, mem_gib=5, cpu_cores=3.6,
                                                   cpu_seconds=1500)]
        r = rs.Readings(91, 87, {"measure": 75}, 14.4, CORES, (5,), 300)
        return ours, r

    def test_a_check_queued_after_the_measurements_refilled_starts_at_once(self):
        ours, r = self.before_the_refill()
        bc = rs.Bitcashier(1, 0)
        refill = rs.decide(CFG, bc, queued(measure=7), ours, NOW, r)
        self.assertTrue(refill.starts and all(c == "measure" for _, c in refill.starts))
        # the next ticks: the refill settled, the running check ended, a new landing check queued
        running = [i for i in ours if i.cls == "measure"] + [
            rs.Instance(s, c, True, NOW, NOW - 900, mem_gib=CFG.job_expected_gib[c], cpu_cores=4, cpu_seconds=1500)
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


def lane_runner(slot, label, busy=False, idle_for=0, cls="measure"):
    """A runner of `cls` registered with one of its lanes' labels (LANES), a plain label stored as none."""
    return dataclasses.replace(inst(slot, cls, busy=busy, idle_for=idle_for),
                               label="" if label == rs.LABELS[cls] else label)


class MeasureLanesTest(unittest.TestCase):
    """2026-11-16 06:28-08:00Z: Main red (#2515-#2521), and its fix's pre-push drift check (Identity takes 43816825023,
    10 drift rows on movies-measure) ran 1-3 rows at a time for 80+ min. The measure lane held 4-5 jobs and 14-27
    queued measurement jobs shared it: an identity-measure run's 10 takes rows, another drift check, order-independence,
    the build caches. GitHub hands a movies-measure runner to whichever queued movies-measure job it picks, so a
    gate-red fix's landing check waited behind optional work. The lanes are labels the scheduler starts runners for,
    the highest first: red fixes, then landing checks, then measurements -- same size, same slice, same admission."""

    def test_the_lanes_are_served_red_fixes_first_then_landing_checks_then_measurements(self):
        lanes = {"movies-red-fix": 1, "movies-landing": 1, "movies-measure": 3}
        d = rs.decide(rs.Config(max_slots=2, check_lane_jobs=0), IDLE, queued(measure=5), [], NOW, ROOMY, lanes)
        self.assertEqual(d.starts, ((1, "measure"), (2, "measure")))
        self.assertEqual(dict(d.labels), {1: "movies-red-fix", 2: "movies-landing"})

    def test_a_measurement_never_takes_a_runner_started_for_a_landing_check(self):
        # the idle movies-measure runner waits for a measurement; the landing check gets one of its own
        lanes = {"movies-landing": 1, "movies-measure": 1}
        d = rs.decide(CFG, IDLE, queued(measure=2), [lane_runner(1, "movies-measure")], NOW, ROOMY, lanes)
        self.assertEqual(dict(d.labels), {2: "movies-landing"})

    def test_a_runner_waiting_in_a_lane_counts_against_that_lane_alone(self):
        lanes = {"movies-landing": 1}
        d = rs.decide(CFG, IDLE, queued(measure=1), [lane_runner(1, "movies-landing")], NOW, ROOMY, lanes)
        self.assertEqual(d.starts, ())

    def test_an_idle_runner_whose_lane_has_nothing_queued_gives_its_slot_back(self):
        # a landing check queued is no reason to keep a movies-measure runner nobody asks for
        stale = lane_runner(1, "movies-measure", idle_for=CFG.idle_stop_seconds)
        d = rs.decide(CFG, IDLE, queued(measure=1), [stale], NOW, ROOMY, {"movies-landing": 1})
        self.assertEqual(d.stops, (1,))
        self.assertEqual(dict(d.labels), {2: "movies-landing"})

    def test_without_lane_counts_every_measurement_is_the_measure_lanes(self):
        d = rs.decide(CFG, IDLE, queued(measure=1), [], NOW, ROOMY)
        self.assertEqual(dict(d.labels), {1: "movies-measure"})

    def test_queued_jobs_count_by_lane_and_every_lane_is_a_measurement(self):
        jobs = [{"status": "queued", "labels": ["movies-red-fix"]}, {"status": "queued", "labels": ["movies-landing"]},
                {"status": "queued", "labels": ["movies-landing"]}, {"status": "queued", "labels": ["movies-measure"]},
                {"status": "in_progress", "labels": ["movies-landing"]}, {"status": "queued", "labels": ["movies-check"]}]
        self.assertEqual(rs.demand_of(jobs), queued(check=1, measure=4))
        self.assertEqual(rs.lane_demand_of(jobs),
                         {"movies-red-fix": 1, "movies-landing": 2, "movies-measure": 1, "movies-check": 1})


class CheckLanesTest(unittest.TestCase):
    """2026-11-17 10:42Z: a red fix (Fixes-Red 2564) was still checking 41 min after it was queued. agentctl puts a red
    fix first in its own queue, but its precommit-check jobs then queued for `movies-check` runners with every other
    landing's (p50 ~8 min a job, 2026-11-14..17), GitHub handing a free runner to any of them. A red fix's checks ask for
    movies-check-red-fix, a lane of the check class the scheduler serves first -- like the measure lanes."""

    def test_a_red_fix_check_takes_the_next_check_start(self):
        lanes = {"movies-check-red-fix": 1, "movies-check": 3}
        d = rs.decide(rs.Config(max_slots=1), IDLE, queued(check=4), [], NOW, ROOMY, lanes)
        self.assertEqual((d.starts, dict(d.labels)), (((1, "check"),), {1: "movies-check-red-fix"}))

    def test_a_check_runner_waiting_for_another_landing_never_counts_for_a_red_fix(self):
        lanes = {"movies-check-red-fix": 1, "movies-check": 1}
        d = rs.decide(CFG, IDLE, queued(check=2), [lane_runner(1, "movies-check", cls="check")], NOW, ROOMY, lanes)
        self.assertEqual(dict(d.labels), {2: "movies-check-red-fix"})

    def test_queued_checks_count_by_lane(self):
        jobs = [{"status": "queued", "labels": ["movies-check-red-fix"]}, {"status": "queued", "labels": ["movies-check"]}]
        self.assertEqual((rs.demand_of(jobs), rs.lane_demand_of(jobs)),
                         (queued(check=2), {"movies-check-red-fix": 1, "movies-check": 1}))


def check_job(run, created, lane="movies-check", status="queued"):
    """A precommit-check job as the jobs API lists it: its lane and its run's pin label (precommit-check.yml)."""
    return {"status": status, "labels": [lane, f"{rs.PIN_PREFIX}{run}"], "created_at": created, "run_id": run}


def pinned_runner(slot, run, lane="movies-check", busy=False, idle_for=0):
    return dataclasses.replace(lane_runner(slot, lane, busy=busy, idle_for=idle_for, cls="check"),
                               pin=f"{rs.PIN_PREFIX}{run}")


class OldestFirstTest(unittest.TestCase):
    """2026-11-17: precommit-check run 44061032302's `heavy rows (e2e)`, created 10:09Z, queued until 10:45Z while later
    landings' check jobs started. A runner registered with the lane's label alone was handed WHICHEVER queued job of the
    lane GitHub chose, so under a backlog one change's jobs were overtaken again and again (W1 alerts). Every check job
    now also asks for its run's pin label (movies-run-<run id>), and a check runner registers the lane's label plus the
    pin of the OLDEST queued job no runner already waits for: it can take that run's jobs alone."""

    def test_a_check_runner_is_pinned_to_the_oldest_queued_jobs_run(self):
        # newer landings' jobs listed first, as the API lists the newest runs first
        jobs = [check_job(300, "2026-11-17T10:30:00Z"), check_job(300, "2026-11-17T10:30:05Z"),
                check_job(200, "2026-11-17T10:20:00Z"), check_job(100, "2026-11-17T10:09:00Z")]
        queue = rs.queue_of(jobs)
        d = rs.decide(rs.Config(max_slots=1), IDLE, rs.demand_of(jobs), [], NOW, ROOMY, queue=queue)
        self.assertEqual((d.starts, dict(d.labels), dict(d.pins)),
                         (((1, "check"),), {1: "movies-check"}, {1: f"{rs.PIN_PREFIX}100"}))

    def test_starts_go_oldest_first_one_runner_per_queued_job(self):
        jobs = [check_job(300, "2026-11-17T10:30:00Z"), check_job(100, "2026-11-17T10:09:00Z"),
                check_job(200, "2026-11-17T10:20:00Z"), check_job(100, "2026-11-17T10:09:30Z")]
        d = rs.decide(rs.Config(max_slots=3), IDLE, rs.demand_of(jobs), [], NOW, ROOMY, queue=rs.queue_of(jobs))
        self.assertEqual([p for _, p in d.pins], [f"{rs.PIN_PREFIX}{r}" for r in (100, 100, 200)])

    def test_a_runner_waiting_for_a_newer_run_never_counts_for_the_oldest(self):
        # before: an idle movies-check runner counted against the lane's whole queue, whoever's job it would get
        jobs = [check_job(100, "2026-11-17T10:09:00Z"), check_job(300, "2026-11-17T10:30:00Z")]
        d = rs.decide(CFG, IDLE, rs.demand_of(jobs), [pinned_runner(1, 300)], NOW, ROOMY, queue=rs.queue_of(jobs))
        self.assertEqual(dict(d.pins), {2: f"{rs.PIN_PREFIX}100"})
        covered = rs.decide(CFG, IDLE, rs.demand_of(jobs), [pinned_runner(1, 300), pinned_runner(2, 100)], NOW, ROOMY,
                            queue=rs.queue_of(jobs))
        self.assertEqual(covered.starts, ())

    def test_the_red_fix_lane_still_goes_first_then_oldest_within_each_lane(self):
        jobs = [check_job(100, "2026-11-17T10:09:00Z"), check_job(400, "2026-11-17T10:40:00Z", "movies-check-red-fix"),
                check_job(200, "2026-11-17T10:20:00Z")]
        d = rs.decide(rs.Config(max_slots=2), IDLE, rs.demand_of(jobs), [], NOW, ROOMY, queue=rs.queue_of(jobs))
        self.assertEqual(dict(d.labels), {1: "movies-check-red-fix", 2: "movies-check"})
        self.assertEqual(dict(d.pins), {1: f"{rs.PIN_PREFIX}400", 2: f"{rs.PIN_PREFIX}100"})

    def test_a_job_with_no_pin_is_served_as_before(self):
        # a check branch pushed before its workflow asked for a pin, or any other job of the lane
        jobs = [{"status": "queued", "labels": ["movies-check"], "created_at": "2026-11-17T10:00:00Z"},
                check_job(100, "2026-11-17T10:09:00Z")]
        d = rs.decide(rs.Config(max_slots=2), IDLE, rs.demand_of(jobs), [], NOW, ROOMY, queue=rs.queue_of(jobs))
        self.assertEqual(dict(d.pins), {1: "", 2: f"{rs.PIN_PREFIX}100"})

    def test_a_borrowed_check_is_pinned_to_the_oldest_too(self):
        jobs = [check_job(300, "2026-11-17T10:30:00Z"), check_job(100, "2026-11-17T10:09:00Z")]
        borrowing = BorrowedCheckTest()
        r = rs.Readings(100, 95, {"measure": 80}, 14, CORES, (), 300, on_cpusets(check_busy=11, measure_busy=3),
                        {"measure": 96})
        d = rs.decide(BEFORE_A1, IDLE, rs.demand_of(jobs), borrowing.two_full_checks(), NOW, r, queue=rs.queue_of(jobs))
        self.assertEqual((d.borrowed, d.pins), ((3, 4), ((3, f"{rs.PIN_PREFIX}100"), (4, f"{rs.PIN_PREFIX}300"))))

    def test_an_idle_runner_whose_run_has_nothing_queued_gives_its_slot_back(self):
        # its run cancelled (a re-pushed check branch): the lane's other runs' jobs are no reason to keep it
        jobs = [check_job(300, "2026-11-17T10:30:00Z")]
        stale = pinned_runner(1, 100, idle_for=CFG.idle_stop_seconds)
        d = rs.decide(CFG, IDLE, rs.demand_of(jobs), [stale], NOW, ROOMY, queue=rs.queue_of(jobs))
        self.assertEqual((d.stops, dict(d.pins)), ((1,), {2: f"{rs.PIN_PREFIX}300"}))

    def test_a_pinned_runner_registers_its_pin_and_is_read_back_with_it(self):
        host = HostTest.make_host(self, FakeDocker(["Runner.Listener"]))
        host.start(4, "check", "", "movies-check", f"{rs.PIN_PREFIX}100")
        with open(os.path.join(host.cfg.state_dir, "4.env")) as f:
            self.assertIn(f"RUNNER_LABELS=movies-check,{rs.PIN_PREFIX}100\n", f.read())
        i = next(i for i in host.instances(NOW) if i.slot == 4)
        self.assertEqual((i.lane, i.pin), ("movies-check", f"{rs.PIN_PREFIX}100"))


EXPLORER_RUN = 44177877990  # the landing check run whose jobs waited (2026-11-17 21:15-21:40Z)


def recorded(slot, cls, mem, cores, started_ago, pin_run=None, busy=True, label=""):
    """A runner of ours as the scheduler's journal line names it: its working set, its cores over the window, its age."""
    i = rs.Instance(slot, cls, busy, NOW, NOW - started_ago, mem_gib=mem, cpu_cores=cores,
                    cpu_seconds=cores * min(started_ago, 300), label=label)
    return dataclasses.replace(i, pin=f"{rs.PIN_PREFIX}{pin_run}") if pin_run else i


class ExplorerLaneTest(unittest.TestCase):
    """2026-11-17 21:15-21:40Z (ci-runner's movies-runner-scheduler journal): the nightly drift explorer's rows queued 6-9
    deep on movies-measure and held the 14 measure threads, while precommit-check run 44177877990's jobs queued with the
    checks' own 18 threads full ("held by cpu", borrowed []). Each time an explorer row ended (21:27:30, 21:31:00) the
    freed measure slot went to the next explorer row, never to a borrowed check: the borrow needs every job on the
    measure CPUs at its whole cap to fit beside the check, and the refilled rows never let it. The explorers now ask for
    movies-explore, the lowest measure lane, and none of it starts while a check waits for a runner: the explorer's
    running rows yield the measure CPUs as they end."""

    def tick(self, ours, measure_busy, explore_label, explorers=6, check_busy=11):
        r = rs.Readings(97, 86, {"measure": 82}, check_busy + measure_busy, CORES, (), 300,
                        on_cpusets(check_busy, measure_busy, CHECK_CPUS_A2, MEASURE_CPUS_A2), {"measure": 96})
        queue = ([rs.Queued("movies-check", f"{rs.PIN_PREFIX}{EXPLORER_RUN}", 1.0)]
                 + [rs.Queued(explore_label, "", 0.5)] * explorers)
        return rs.decide(CFG, IDLE, queued(check=1, measure=explorers), ours, NOW, r, queue=queue)

    def at_212730(self, label):
        # ours [(1, check/44177877990, busy, 1, 0.9) (2, measure, busy, 7, 1.7) (3, measure, busy, 6, 2.4)
        #       (5, check/44177877990, busy, 8, 8.4)]; cpu {'check': '11/18', 'measure': '5/14'}; queued check 1, measure 6
        return [recorded(1, "check", 1, 0.9, 60, EXPLORER_RUN), recorded(2, "measure", 7, 1.7, 1080, label=label),
                recorded(3, "measure", 6, 2.4, 750, label=label), recorded(5, "check", 8, 8.4, 660, EXPLORER_RUN)]

    def test_212730_before_an_explorer_row_took_the_freed_measure_slot(self):
        # the journal: start [(4, 'measure')] lanes [(4, 'movies-measure')] borrowed []
        d = self.tick(self.at_212730(""), 5, "movies-measure")
        self.assertEqual((d.starts, d.labels, d.borrowed), (((4, "measure"),), ((4, "movies-measure"),), ()))

    def test_212730_now_no_explorer_row_starts_while_the_check_waits(self):
        d = self.tick(self.at_212730("movies-explore"), 5, "movies-explore")
        self.assertEqual((d.starts, d.borrowed), ((), ()))

    def test_213100_with_the_rows_yielding_the_check_borrows_the_measure_cpus(self):
        # 21:31:00 had both checks (1: 3.3 cores, 5: 7.6) and the row started at 21:27:30 (2.6 of the 6 busy measure
        # cores). Without that row, the two older rows ended and the checks' threads as full as at 21:26:30 (13 of 18),
        # the check borrows the idle measure CPUs -- with the row there (the old order), the measure CPUs held no borrow
        ours = [recorded(1, "check", 5, 3.3, 270, EXPLORER_RUN), recorded(5, "check", 8, 7.6, 870, EXPLORER_RUN)]
        refilled = ours + [recorded(4, "measure", 7, 2.6, 210)]
        self.assertEqual(self.tick(refilled, 6, "movies-measure", check_busy=13).borrowed, ())
        d = self.tick(ours, 3.4, "movies-explore", check_busy=13)
        self.assertIn((2, "check"), d.starts)
        self.assertEqual((d.borrowed, dict(d.pins)[2]), ((2,), f"{rs.PIN_PREFIX}{EXPLORER_RUN}"))
        self.assertNotIn("movies-explore", dict(d.labels).values(), "the borrow keeps the room an explorer would take")

    def test_explorer_rows_start_when_no_check_waits(self):
        d = rs.decide(CFG, IDLE, queued(measure=2), [], NOW, ROOMY, queue=[rs.Queued("movies-explore")] * 2)
        self.assertEqual(dict(d.labels), {1: "movies-explore", 2: "movies-explore"})

    def test_explorer_rows_go_last_of_the_measure_lanes(self):
        queue = [rs.Queued("movies-explore", "", 0.0), rs.Queued("movies-measure", "", 1.0)]
        d = rs.decide(rs.Config(max_slots=1, check_lane_jobs=0), IDLE, queued(measure=2), [], NOW, ROOMY, queue=queue)
        self.assertEqual(dict(d.labels), {1: "movies-measure"})

    def test_an_idle_explorer_runner_gives_way_to_a_waiting_check(self):
        # the check lane full (two checks at 9 cores on the 18 threads) and the idle explorer runner's unshown cap all
        # that holds a borrow back: it goes at once, not after idle_stop_seconds -- and stays while no check waits
        checks = [recorded(1, "check", 8, 9, 900, EXPLORER_RUN), recorded(5, "check", 8, 9, 900, EXPLORER_RUN)]
        ours = checks + [lane_runner(3, "movies-explore")]
        d = self.tick(ours, 2, "movies-explore", explorers=1, check_busy=18)
        self.assertEqual((d.borrowed, d.stops), ((), (3,)))
        r = rs.Readings(97, 86, {"measure": 82}, 20, CORES, (), 300, on_cpusets(18, 2, CHECK_CPUS_A2, MEASURE_CPUS_A2),
                        {"measure": 96})
        quiet = rs.decide(CFG, IDLE, queued(measure=1), ours, NOW, r, queue=[rs.Queued("movies-explore")])
        self.assertEqual(quiet.stops, ())

    def test_explorer_jobs_count_as_measurements_in_their_lane(self):
        jobs = [{"status": "queued", "labels": ["movies-explore"]}]
        self.assertEqual((rs.demand_of(jobs), rs.lane_demand_of(jobs)), (queued(measure=1), {"movies-explore": 1}))


def plan_job(run, created, status="in_progress", branch="check/20261118-0001-movies-x",
             workflow=rs.WARM_WORKFLOW, name=rs.WARM_PLAN_JOB):
    """A precommit-check run's plan job as the jobs API lists it (on the light lane)."""
    return {"status": status, "labels": ["movies"], "created_at": created, "run_id": run, "name": name,
            "workflow_name": workflow, "head_branch": branch}


class WarmCheckTest(unittest.TestCase):
    """THE WARM CHECK RUNNER (the owner, 2026-11-18, "8"): every check job queued ~30 s with nothing else ahead of it
    (precommit-check, 2026-11-11..18, jobs queued <= 75 s: check (rest) p50 31 s, check (identity) p50 35.5 s) -- up to a
    tick (30 s) before the scheduler saw it, then ~7 s to start the container and register the ephemeral runner. A
    runner registered ahead with the lane's label alone could take no check job: each asks for its run's pin too
    (OLDEST FIRST). But a run's later check jobs (the identity shards, the old refs) need its plan job, so while the plan
    runs their lane and pin are known: ONE check runner is registered for them then, the oldest such run first, admitted
    as a check is, after the queued ones. It goes as soon as its run has nothing left for it (warm_grace_seconds), so
    its cores and memory return to the measure lane."""

    def test_a_running_precommit_plan_is_a_check_to_come_in_its_runs_lane_and_pin(self):
        jobs = [plan_job(200, "2026-11-18T10:00:05Z"), plan_job(100, "2026-11-18T10:00:00Z", status="queued",
                                                                 branch="check/red-20261118-movies-y"),
                plan_job(300, "2026-11-18T10:00:09Z", status="completed"),
                plan_job(400, "2026-11-18T10:00:09Z", workflow="Identity takes"),
                plan_job(500, "2026-11-18T10:00:09Z", name="check (rest)")]
        self.assertEqual([(q.lane, q.pin) for q in rs.anticipated_of(jobs)],
                         [("movies-check-red-fix", f"{rs.PIN_PREFIX}100"), ("movies-check", f"{rs.PIN_PREFIX}200")])
        self.assertTrue(all(rs.listed(j) for j in jobs[:2]), "the jobs list keeps a running plan job")
        self.assertFalse(rs.listed(plan_job(1, "2026-11-18T10:00:00Z", workflow="Identity takes")))

    def test_one_warm_runner_for_the_oldest_run_to_come(self):
        coming = [rs.Queued("movies-check", f"{rs.PIN_PREFIX}200", 2.0), rs.Queued("movies-check", f"{rs.PIN_PREFIX}100", 1.0)]
        d = rs.decide(CFG, IDLE, queued(), [], NOW, ROOMY, queue=[], anticipated=coming)
        self.assertEqual((d.starts, dict(d.pins), d.warm), (((1, "check"),), {1: f"{rs.PIN_PREFIX}100"}, (1,)))

    def test_a_warm_runner_waiting_holds_off_a_second(self):
        coming = [rs.Queued("movies-check", f"{rs.PIN_PREFIX}100", 1.0), rs.Queued("movies-check", f"{rs.PIN_PREFIX}200", 2.0)]
        d = rs.decide(CFG, IDLE, queued(), [pinned_runner(1, 100)], NOW, ROOMY, queue=[], anticipated=coming)
        self.assertEqual(d.starts, ())

    def test_queued_checks_go_first(self):
        jobs = [check_job(300, "2026-11-18T10:00:00Z")]
        coming = [rs.Queued("movies-check", f"{rs.PIN_PREFIX}100", 1.0)]
        d = rs.decide(rs.Config(max_slots=1), IDLE, rs.demand_of(jobs), [], NOW, ROOMY, queue=rs.queue_of(jobs),
                      anticipated=coming)
        self.assertEqual((dict(d.pins), d.warm), ({1: f"{rs.PIN_PREFIX}300"}, ()))

    def test_no_warm_runner_where_a_check_would_not_start(self):
        coming = [rs.Queued("movies-check", f"{rs.PIN_PREFIX}100", 1.0)]
        self.assertEqual(rs.decide(CFG, rs.Bitcashier(12, 0), queued(), [], NOW, ROOMY, queue=[],
                                   anticipated=coming).starts, ())
        full = rs.Readings(10, 10, {}, 0, CORES, (), 300, {}, {})  # no memory for a check
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [], NOW, full, queue=[], anticipated=coming).starts, ())

    def test_a_warm_runner_stays_while_its_plan_runs(self):
        coming = [rs.Queued("movies-check", f"{rs.PIN_PREFIX}100", 1.0)]
        waiting = pinned_runner(1, 100, idle_for=CFG.idle_stop_seconds * 2)
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [waiting], NOW, ROOMY, queue=[], anticipated=coming).stops, ())

    def test_its_run_with_nothing_left_for_it_the_warm_runner_goes_after_the_grace(self):
        # the plan said no identity check: nothing of its run will queue, so its 10 GiB and cores go back
        young = pinned_runner(1, 100, idle_for=CFG.warm_grace_seconds - 1)
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [young], NOW, ROOMY, queue=[]).stops, ())
        done = pinned_runner(1, 100, idle_for=CFG.warm_grace_seconds)
        self.assertEqual(rs.decide(CFG, IDLE, queued(), [done], NOW, ROOMY, queue=[]).stops, (1,))

    def test_the_jobs_it_was_warmed_for_are_its_own(self):
        # the plan ended and its identity shards queued: the warm runner covers one, a second runner starts for the other
        jobs = [check_job(100, "2026-11-18T10:01:00Z"), check_job(100, "2026-11-18T10:01:00Z")]
        d = rs.decide(CFG, IDLE, rs.demand_of(jobs), [pinned_runner(1, 100)], NOW, ROOMY, queue=rs.queue_of(jobs))
        self.assertEqual((d.starts, dict(d.pins)), (((2, "check"),), {2: f"{rs.PIN_PREFIX}100"}))

    def test_the_loop_ticks_faster_while_checks_are_queued_or_coming(self):
        self.assertEqual(rs.pace_seconds(CFG, [], [], 5000), CFG.tick_seconds)
        self.assertEqual(rs.pace_seconds(CFG, [rs.Queued("movies-check")], [], 5000), CFG.busy_tick_seconds)
        self.assertEqual(rs.pace_seconds(CFG, [rs.Queued("movies-measure")], [], 5000), CFG.tick_seconds)
        self.assertEqual(rs.pace_seconds(CFG, [], [rs.Queued("movies-check")], 5000), CFG.busy_tick_seconds)
        self.assertEqual(rs.pace_seconds(CFG, [rs.Queued("movies-check")], [], 1000), CFG.tick_seconds * 4,
                         "a reader budget running low still slows the loop")


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
        return HostTest.make_host(self, docker)

    @staticmethod
    def make_host(case, docker):
        state = tempfile.mkdtemp()
        case.addCleanup(shutil.rmtree, state)
        with open(os.path.join(state, "2.env"), "w") as f:
            f.write("RUNNER_CLASS=check\n")
        patcher = unittest.mock.patch.object(rs, "_run", docker)
        patcher.start()
        case.addCleanup(patcher.stop)
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

    def test_a_borrowed_check_runs_in_the_measure_slice_and_is_read_back_there(self):
        docker = FakeDocker(["Runner.Worker"])
        host = self.host(docker)
        host.start(5, "check", "measure")
        with open(os.path.join(host.cfg.state_dir, "5.env")) as f:
            env = f.read()
        self.assertIn("RUNNER_LABELS=movies-check\n", env)
        self.assertIn("RUNNER_SLICE=ci-movies-measure.slice\n", env)
        d = os.path.join(host.cfg.class_cgroup.format(cls="measure"), "docker-c0ffee.scope")
        os.makedirs(d)
        with open(os.path.join(d, "memory.current"), "w") as f:
            f.write(str(3 * GIB))
        with open(os.path.join(d, "memory.stat"), "w") as f:
            f.write("inactive_file 0\n")
        borrowed = next(i for i in host.instances(NOW) if i.slot == 5)
        self.assertEqual((borrowed.cls, borrowed.runs_in, borrowed.mem_gib), ("check", "measure", 3))

    def test_a_lane_runner_registers_its_lanes_label_in_the_measure_slice_and_is_read_back_in_it(self):
        host = self.host(FakeDocker(["Runner.Listener"]))
        host.start(6, "measure", "", "movies-red-fix")
        with open(os.path.join(host.cfg.state_dir, "6.env")) as f:
            env = f.read()
        self.assertIn("RUNNER_CLASS=measure\n", env)
        self.assertIn("RUNNER_LABELS=movies-red-fix\n", env)
        self.assertIn("RUNNER_SLICE=ci-movies-measure.slice\n", env)
        lane = next(i for i in host.instances(NOW) if i.slot == 6)
        self.assertEqual((lane.cls, lane.lane, lane.borrowed), ("measure", "movies-red-fix", False))

    def test_a_light_runner_says_this_scheduler_serves_the_lanes(self):
        # a workflow's plan job, on a light runner, reads its RUNNER_LABELS: the lanes' labels are asked for only where
        # a scheduler that starts runners for them is deployed (scripts/ci/measure-lane.sh)
        host = self.host(FakeDocker(["Runner.Listener"]))
        host.start(11, "light")
        with open(os.path.join(host.cfg.state_dir, "11.env")) as f:
            self.assertIn(f"RUNNER_LABELS=movies,{rs.LANES_MARKER},{rs.EXPLORE_MARKER}\n", f.read())
        light = next(i for i in host.instances(NOW) if i.slot == 11)
        self.assertEqual(light.lane, "movies")

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
