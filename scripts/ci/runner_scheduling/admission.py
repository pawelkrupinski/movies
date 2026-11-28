"""The decision, pure: which runners a tick starts and stops (decide), over one tick's admission ledger -- what memory,
each class's cores, slices and caps still hold, and what this tick has started (runner_scheduler.py's docstring: the
rule, THE CHECK LANE, BORROWED CHECKS, THE MEASURE LANES, THE EXPLORER LANE, THE LIGHT LANE, THE WARM LIGHT RUNNER)."""
from __future__ import annotations

from typing import Dict, List, Optional, Tuple

from .charges import (class_free_gib, cpu_owed, cpu_spare, memory_free_gib, memory_owed, saturated, settling,
                      share_cpus, whole_jobs)
from .jobs import queued_jobs, uncovered
from .model import (ADMISSION_LIMITS, CLASS_OF, CLASSES, EXPLORE_LANE, LANDING_MEASURE_LANES, LANES,
                    OPTIONAL_MEASURE_LANES, Bitcashier, Config, Decision, Instance, Queued, Readings)
from .stops import idle_to_stop

Among = Optional[Tuple[str, ...]]  # the lanes of a class a start is limited to (None: all of them)


def decide(cfg: Config, bc: Bitcashier, demand: Dict[str, int], instances: List[Instance], now: float, r: Readings,
           lanes: Optional[Dict[str, int]] = None, queue: Optional[List[Queued]] = None) -> Decision:
    """The runners to start ((slot, class), highest class first, then the light lane) and the idle ones to stop, for
    `demand` (queued jobs per class), the laned classes' queued jobs (`queue`, queue_of; or only their counts per lane,
    `lanes`, none pinned; neither given: all in the classes' plain labels' lanes), the `instances` that exist and the
    host's live readings `r`."""
    jobs = queued_jobs(demand, lanes, queue)
    full = saturated(cfg, bc)
    stops = idle_to_stop(cfg, bc, demand, instances, now, r, jobs)
    used = {i.slot for i in instances}  # a stopping slot is not reused in the same tick
    ledger = Admission(cfg, bc, r, demand, [i for i in instances if i.slot not in stops], jobs, now)
    heavy_slots = free_slots(used, 1, cfg.max_slots)
    limits = ledger.limits(heavy_slots)
    if not full:
        stops += ledger.admit_heavy(heavy_slots)
    # THE LIGHT LANE: its own slots, admitted at a light job's size whatever the heavy queue or bitcashier's runners.
    light_slots = free_slots(used, cfg.max_slots + 1, cfg.max_slots + cfg.light_slots)
    ledger.admit("light", light_slots)
    warm = ledger.keep_warm(light_slots)
    return Decision(0 if full else room_of(limits), full, tuple(ledger.starts), tuple(sorted(stops)), limits,
                    tuple(ledger.borrowed), tuple(sorted(ledger.labels.items())), tuple(sorted(ledger.pins.items())),
                    tuple(warm))


def free_slots(used: set[int], first: int, last: int) -> List[int]:
    return [s for s in range(first, last + 1) if s not in used]


def room_of(limits: Dict[str, object]) -> int:
    """How many measurements every admission limit held (ADMISSION_LIMITS, the whole-job ones of `limits`)."""
    return max(0, min(v for k, v in limits.items() if k in ADMISSION_LIMITS and isinstance(v, int)))


class Admission:
    """One tick's admission ledger, over the runners alive past the tick's stops: the memory free for more movies jobs,
    each class's spare cores on its own CPUs, each limited slice's memory and container caps -- less what our settling
    runners still owe and what this tick has started -- and the starts, borrowed checks, lanes and pins it made."""

    def __init__(self, cfg: Config, bc: Bitcashier, r: Readings, demand: Dict[str, int], alive: List[Instance],
                 jobs: List[Queued], now: float):
        self.cfg, self.r, self.demand, self.alive, self.jobs = cfg, r, demand, alive, jobs
        self.pending = [i for i in alive if settling(cfg, i, now)]
        self.owed_cpus = cpu_owed(cfg, r, alive, now)
        self.memory = memory_free_gib(cfg, bc, r, memory_owed(cfg, self.pending))
        # each class's spare cores on its own CPUs, less what our runners there are charged past the reading (cpu_owed)
        self.cpu = {cls: self.spare_cores(cls, now) for cls in CLASSES}
        # the check lane's cores are kept from measurements only where checks could run on them
        self.lane_shares_cpus = share_cpus(r, "measure", "check")
        self.held = {cls: self.slice_free_gib(cls) for cls in CLASSES}
        # each limited slice's room in whole container caps, less the runners of its class alive (busy or waiting)
        self.caps = {cls: self.slice_caps(cls) for cls in CLASSES if cls in r.class_max_gib}
        self.starts: List[Tuple[int, str]] = []
        self.borrowed: List[int] = []
        self.labels: Dict[int, str] = {}  # slot -> the lane a measurement start registers
        self.pins: Dict[int, str] = {}  # slot -> the run's pin it registers beside it

    def slice_free_gib(self, cls: str) -> Optional[float]:
        return class_free_gib(self.cfg, self.r, cls, memory_owed(self.cfg, [i for i in self.pending if i.runs_in == cls]))

    def slice_caps(self, cls: str) -> int:
        return whole_jobs(self.r.class_max_gib[cls], self.cfg.container_gib) - sum(1 for i in self.alive if i.runs_in == cls)

    def spare_cores(self, cls: str, now: float) -> float:
        below = self.cfg.light_cpu_start_below if cls == "light" else self.cfg.cpu_start_below
        sharing = [i for i in self.alive if share_cpus(self.r, cls, i.runs_in)]
        return cpu_spare(self.cfg, self.r, cpu_owed(self.cfg, self.r, sharing, now), below, cls)

    # ── what holds a job ────────────────────────────────────────────────────────────────────────────────────────────
    def checks_on_own_cpus(self) -> int:
        """The checks on their OWN cpuset, running or starting: a borrowed one runs on the measure CPUs."""
        return (sum(1 for i in self.alive if i.cls == "check" and not i.borrowed)
                + sum(1 for s, c in self.starts if c == "check" and s not in self.borrowed))

    def lane(self) -> Tuple[int, float, float]:
        """(slots, GiB, cores) the check lane still holds: check_lane_jobs less the checks running or starting, each at
        what a new check is charged (none settled: job_cpus, so measured cores would leave a fresh one short). Only
        the checks on their OWN cpuset count: a borrowed one runs on the measure CPUs (2026-11-17 09:23Z: two
        borrowed checks held the lane at 0 while the checks' 12 threads were 3 busy and 6 checks queued 21 min)."""
        free = max(0, self.cfg.check_lane_jobs - self.checks_on_own_cpus())
        return free, free * self.cfg.job_expected_gib["check"], free * self.cfg.job_cpus["check"]

    def peak_room(self) -> Optional[int]:
        """How many more checks the checks' own cpuset holds at their peak (check_peak_cpus), less the checks on it,
        running or starting (a borrowed one runs on the measure CPUs); None: the cpuset was not read."""
        own = self.r.class_cpu.get("check")
        if own is None:
            return None
        return whole_jobs(len(own.cpus), self.cfg.check_peak_cpus) - self.checks_on_own_cpus()

    def kept(self, cls: str, keep_measurements: int) -> Tuple[int, float, float]:
        """(slots, GiB, cores) a `cls` job leaves: the check lane from a measurement (a check, or a light job in its own
        slots, is not held by it; its cores only where checks share the measure CPUs), and `keep_measurements`
        measurements' room."""
        keep_slots, keep_gib, keep_cpus = self.lane() if cls == "measure" else (0, 0, 0)
        keep_cpus = keep_cpus if self.lane_shares_cpus else 0
        return (keep_slots + keep_measurements, keep_gib + keep_measurements * self.cfg.job_expected_gib["measure"],
                keep_cpus + keep_measurements * self.cfg.job_cpus["measure"])

    def cpu_holds(self, cls: str, keep_cpus: float) -> bool:
        """The CPUs hold the job -- or it is a check inside the lane, which the CPU never holds: a lane check waits for
        nothing (2026-11-08 18:55-18:57Z: lane checks queued 57 min "held by cpu" on 5-8 of 12 busy cores; a busy
        core slows it, under its slice's weight, but never queues it)."""
        return self.cpu[cls] - keep_cpus >= self.cfg.job_cpus[cls] or (cls == "check" and self.lane()[0] > 0)

    def peak_holds(self, cls: str) -> bool:
        return cls != "check" or (room := self.peak_room()) is None or room > 0

    def slice_holds(self, cls: str, keep_measurements: int) -> bool:
        """The class's slice holds the job past `keep_measurements` measurements: its memory and its caps."""
        held, keep_held = self.held[cls], keep_measurements * self.cfg.job_expected_gib["measure"]
        return ((held is None or held - keep_held >= self.cfg.job_expected_gib[cls])
                and self.caps.get(cls, 1) - keep_measurements >= 1)

    def room_for(self, cls: str, slots: List[int], keep_measurements: int = 0) -> bool:
        """Whether everything holds one more `cls` job past what is kept (kept)."""
        keep_slots, keep_gib, keep_cpus = self.kept(cls, keep_measurements)
        return (len(slots) > keep_slots and self.memory - keep_gib >= self.cfg.job_expected_gib[cls]
                and self.cpu_holds(cls, keep_cpus) and self.peak_holds(cls) and self.slice_holds(cls, keep_measurements))

    # ── what is queued with no runner ───────────────────────────────────────────────────────────────────────────────
    def unserved_jobs(self, label: str) -> List[Queued]:
        """A lane's queued jobs, oldest first, with no runner of that lane and pin waiting for them or starting this
        tick (uncovered)."""
        waiting = ([(i.lane, i.pin) for i in self.alive if i.cls == CLASS_OF[label] and i.lane == label and not i.busy]
                   + [(lane_label, self.pins[slot]) for slot, lane_label in self.labels.items() if lane_label == label])
        return uncovered([q for q in self.jobs if q.lane == label], waiting)

    def lane_unserved(self, label: str) -> int:
        """A lane's queued jobs with no runner of that lane waiting for them or starting this tick."""
        return len(self.unserved_jobs(label))

    def unserved(self, cls: str, among: Among = None) -> int:
        """Queued jobs of `cls` with no runner waiting for them or starting this tick (a check's or measurement's in its
        lane; `among`: in those of its lanes alone)."""
        if cls in LANES:
            return sum(self.lane_unserved(label) for label in (among or LANES[cls]))
        waiting = sum(1 for i in self.alive if i.cls == cls and not i.busy)
        return max(0, self.demand.get(cls, 0) - waiting - sum(1 for _, c in self.starts if c == cls))

    # ── starting ────────────────────────────────────────────────────────────────────────────────────────────────────
    def take(self, slot: int, cls: str, home: str, among: Among = None) -> None:
        """Start `cls` in `home`'s slice: charge its memory, its slice and caps, and its cores on every class sharing
        that slice's CPUs. A check or a measurement registers its class's highest lane still unserved (`among`: of
        those lanes)."""
        if cls in LANES:
            self.register_lane(slot, cls, among)
        self.starts.append((slot, cls))
        self.charge(home, self.cfg.job_expected_gib[cls], self.cfg.job_cpus[cls])

    def register_lane(self, slot: int, cls: str, among: Among) -> None:
        """The highest lane unserved, pinned to its oldest unserved job's run (OLDEST FIRST)."""
        label = next(label for label in (among or LANES[cls]) if self.lane_unserved(label))
        self.pins[slot] = self.unserved_jobs(label)[0].pin
        self.labels[slot] = label

    def charge(self, home: str, size: float, cores: float) -> None:
        if home in self.caps:
            self.caps[home] -= 1
        self.memory -= size
        held = self.held[home]
        self.held[home] = None if held is None else held - size
        for other in self.cpu:
            if share_cpus(self.r, other, home):
                self.cpu[other] -= cores

    def admit(self, cls: str, slots: List[int], among: Among = None, keep_measurements: int = 0) -> None:
        """Start runners for `cls`'s unserved jobs (`among`: of those lanes alone) while everything holds one
        (room_for)."""
        need = self.unserved(cls, among)
        while need and self.room_for(cls, slots, keep_measurements):
            self.take(slots.pop(0), cls, cls, among)
            need -= 1

    def admit_heavy(self, slots: List[int]) -> List[int]:
        """The checks, the landing-path measurements, the borrowed checks, the optional measurements and the explorers'
        rows, in that order; the idle explorer runners to stop while a check still waits for a runner."""
        self.admit("check", slots)
        self.admit("measure", slots, LANDING_MEASURE_LANES)
        self.borrow_check(slots)
        # optional measurements after the borrows, leaving the kept measurements' room while a borrowed check runs
        keep = self.cfg.measure_reserve_jobs if self.borrowed or any(i.borrowed for i in self.alive) else 0
        self.admit("measure", slots, OPTIONAL_MEASURE_LANES, keep_measurements=keep)
        # THE EXPLORER LANE last, and none of it while a check still waits for a runner: an idle explorer runner goes
        # too (what it is charged is what may hold the check's borrow back), and the running rows yield as they end
        if self.unserved("check"):
            return [i.slot for i in self.alive if not i.busy and i.lane == EXPLORE_LANE]
        self.admit("measure", slots, (EXPLORE_LANE,), keep_measurements=keep)
        return []

    def borrow_check(self, slots: List[int]) -> None:
        """BORROWED CHECKS (the owner, 2026-11-16): a queued check its own cpuset could not hold starts in the measure
        slice, on CPUs measurements leave idle -- without the 2026-11-03 slowdown (a check's test phase 6 -> 16 min
        beside measurements on shared cores) and without holding a measurement back:
          * MEASURED CPU, AS EVERY OTHER START: the live reading of the measure CPUs, less what our runners there are
            charged past it (cpu_owed: every settling runner and every borrowed check the rest of its whole cap, one
            settled job ramping up), holds this check at its job_cpus and the kept measurements' cores. No ledger of
            every job there at its whole cap (2026-11-16..19): on A2's 14 threads one light runner and one measurement,
            charged 5 cores while they used under 2, plus the check's 4 and a kept measurement's 4 passed 0.85 x 14,
            so a check never borrowed beside any measurement -- on 2026-11-19, of the 2151 ticks a landing check waited
            "held by cpu", 1157 read the measure CPUs at most 6 of 14 busy and the host at least 10 of 32 idle.
          * LANDING-PATH MEASUREMENTS FIRST: none starts while a queued red-fix or landing measurement
            (LANDING_MEASURE_LANES) has no runner, an idle borrowed runner is stopped when one queues with no runner
            (idle_to_stop), and at most borrow_max_checks run at once. An optional measurement (movies-measure) is
            admitted AFTER the borrows (the owner, 2026-11-17, "A3": a landing check ahead of optional measurements).
          * THE BOUND: while a landing-path measurement runs or starts (its run's next rows may queue any tick), each
            borrow leaves measure_reserve_jobs measurements' cores, and always their memory, slice caps and slots --
            optional measurements too leave them while a borrowed check runs -- so a landing-path measurement queued
            beside borrowed checks starts on the next tick whatever they do; only measurements beyond those may wait,
            each for at most the end of one borrowed check (a landing-check job, under 20 min), and only where the
            CPUs would have held them -- never more of them than borrow_max_checks. With no landing-path measurement
            in flight the cores are not kept for one that may come: a queued landing check is of the same class
            (lane-policy.tsv) and holds them for one job at most (2026-11-19 20:02:53Z: five checks queued 33 min,
            "held by cpu", the measure CPUs 2 of 14 busy and no measurement queued or running)."""
        if share_cpus(self.r, "check", "measure") or "measure" not in self.r.class_cpu:
            return  # one cpuset (or unknown ones): nothing to borrow
        keep_cores = self.cfg.measure_reserve_jobs * self.cfg.job_cpus["measure"] if self.landing_rows() else 0
        need = self.unserved("check")
        while need and len(slots) > self.cfg.measure_reserve_jobs and self.may_borrow(keep_cores):
            slot = slots.pop(0)
            self.take(slot, "check", "measure")
            self.borrowed.append(slot)
            need -= 1

    def landing_rows(self) -> bool:
        """A landing-path measurement runs or starts this tick."""
        return (any(i.cls == "measure" and i.lane in LANDING_MEASURE_LANES for i in self.alive)
                or any(label in LANDING_MEASURE_LANES for label in self.labels.values()))

    def may_borrow(self, keep_cores: float) -> bool:
        """One more check may borrow: every landing-path measurement has its runner, under borrow_max_checks, and the
        measure pool holds the check past the kept measurements (borrow_check, THE BOUND)."""
        borrowing = sum(1 for i in self.alive if i.borrowed) + len(self.borrowed)
        return (not self.unserved("measure", LANDING_MEASURE_LANES) and borrowing < self.cfg.borrow_max_checks
                and self.measure_pool_holds_check(keep_cores))

    def measure_pool_holds_check(self, keep_cores: float) -> bool:
        reserve = self.cfg.measure_reserve_jobs
        keep_gib, check_gib = reserve * self.cfg.job_expected_gib["measure"], self.cfg.job_expected_gib["check"]
        held = self.held["measure"]
        return (self.cpu["measure"] - keep_cores >= self.cfg.job_cpus["check"] and self.memory - keep_gib >= check_gib
                and (held is None or held - keep_gib >= check_gib) and self.caps.get("measure", 1) - reserve >= 1)

    def keep_warm(self, slots: List[int]) -> List[int]:
        """THE WARM LIGHT RUNNER (the owner, 2026-11-18, "10": precommit-check's plan job queued p50 38-68 s, p90 664-837 s
        over 2026-11-11..18, and every check job after it waits for it): warm_light_runners light runners kept idle
        past the queued light jobs, so the next one -- a landing check's plan above all, which asks for no pin -- is
        taken the moment it queues. Admitted as a light job is (1 GiB, 1 core, its own slots)."""
        spare = (sum(1 for i in self.alive if i.cls == "light" and not i.busy)
                 + sum(1 for _, c in self.starts if c == "light") - self.demand.get("light", 0))
        warm: List[int] = []
        while spare < self.cfg.warm_light_runners and self.room_for("light", slots):
            slot = slots.pop(0)
            self.take(slot, "light", "light")
            warm.append(slot)
            spare += 1
        return warm

    # ── what held a measurement, for the log line ───────────────────────────────────────────────────────────────────
    def limits(self, heavy_slots: List[int]) -> Dict[str, object]:
        """What holds a measurement before the starts: ADMISSION_LIMITS in whole jobs, plus the charge of our settling
        runners (the log line names the binding term), and a landing check's own terms (check_limits)."""
        lane_slots, lane_gib, lane_cpus = self.lane()
        size, measure_cpus = self.cfg.job_expected_gib["measure"], self.cfg.job_cpus["measure"]
        held = self.held["measure"]
        return {"memory": whole_jobs(self.memory - lane_gib, size),
                "slice": None if held is None else whole_jobs(held, size),
                "caps": self.caps.get("measure"),
                "cpu": whole_jobs(self.cpu["measure"] - (lane_cpus if self.lane_shares_cpus else 0), measure_cpus),
                "slots": max(0, len(heavy_slots) - lane_slots),
                "settling": f"{len(self.pending)} owe {memory_owed(self.cfg, self.pending):.0f} GiB",
                "owed_cores": round(self.owed_cpus, 1),
                "check_lane": f"{lane_slots} x {self.cfg.job_expected_gib['check']} GiB {self.cfg.job_cpus['check']} cores",
                "check": self.check_limits(heavy_slots, lane_slots)}

    def check_limits(self, heavy_slots: List[int], lane_slots: int) -> Dict[str, Optional[int]]:
        """A landing check's own terms (no lane kept from it; inside the lane the CPU never holds it), so a starved check
        names its own cause, not a measurement's (2026-11-15/16: 483 "held by nothing named")."""
        size, held = self.cfg.job_expected_gib["check"], self.held["check"]
        return {"memory": whole_jobs(self.memory, size),
                "slice": None if held is None else whole_jobs(held, size),
                "caps": self.caps.get("check"),
                "cpu": None if lane_slots else whole_jobs(self.cpu["check"], self.cfg.job_cpus["check"]),
                "slots": len(heavy_slots),
                "peak": self.peak_room()}
