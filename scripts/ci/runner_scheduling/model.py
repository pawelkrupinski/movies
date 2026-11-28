"""The scheduler's vocabulary: the job classes and their lanes' labels, the config every number of the rule lives in
(runner_scheduler.py's docstring says the rule), and the data each tick reads and decides -- no logic past a property."""
from __future__ import annotations

import dataclasses
import json
from typing import Any

from .machine import Machine

# The job classes in priority order, and the one label a runner of each class registers with.
CLASSES = ("check", "measure", "light")
LABELS = {"check": "movies-check", "measure": "movies-measure", "light": "movies"}
# THE MEASURE LANES (2026-11-16): labels of the measure class, highest first. A runner registers ONE of them, so it takes
# only that lane's job, and a free measure start goes to the highest lane with a job no runner waits for. Every lane is a
# measurement in all else: its size, its slice, its admission. A red-fix's and a landing check's heavy rows ask for theirs
# (scripts/ci/measure-lane.sh), so they never queue behind optional measurements for a runner GitHub hands to any of them.
#
# THE EXPLORER LANE (the owner, 2026-11-18, "7"): the nightly drift and family explorers' rows ask for the LAST lane,
# movies-explore, below every optional measurement -- and none of it starts while a landing check waits for a runner
# (decide: their rows yield the measure CPUs as they end, so a check can borrow them). On 2026-11-17 21:15-21:40Z the
# drift explorer's rows refilled the measure slot each time one ended, and precommit-check run 44177877990's jobs waited
# with borrowed [] (ExplorerLaneTest).
#
# THE STAGE LANE (the owner, 2026-11-19): deploy staging (nix-stage-closures.yml) asks for movies-stage, after the
# landing path and before every optional measurement -- Stage NixOS closures #102's nix-stage job waited 2.5 h on
# movies-measure behind 23 of them ("STARVED measure: 23 queued, the oldest 300 min, held by cpu").
STAGE_LANE = "movies-stage"
# THE CONSUMER LANE (the owner, 2026-11-19): a row that waits for an artifact another row of its run uploads -- Record's
# order-independence rows, which replay the corpus their convergence row records -- asks for movies-consumer, just below
# movies-measure, so a free measure start goes to the producer first. Record scrape fixtures #198's UK order row took a
# movies-measure runner at 00:10Z while its convergence row waited until 01:41Z, and idled until its bound failed it.
CONSUMER_LANE = "movies-consumer"
MEASURE_LANES = ("movies-red-fix", "movies-landing", STAGE_LANE, LABELS["measure"], CONSUMER_LANE, "movies-explore")
EXPLORE_LANE = MEASURE_LANES[-1]
# The extra label a light runner of a scheduler serving the lanes registers: a workflow's plan job, on a light runner,
# asks for a lane only where its own RUNNER_LABELS name this (a lane no scheduler serves would wait forever).
LANES_MARKER = "movies-lanes"
# ...and the one that says it serves the explorer lane too (measure-lane.sh `explore`): a light runner of an older
# scheduler lacks it, and an explorer's rows then ask for movies-measure, which that scheduler serves
EXPLORE_MARKER = "movies-lanes-explore"
# ...and the one that says it serves the stage lane (measure-lane.sh `stage`): an older scheduler's light runner lacks it,
# and deploy staging then asks for movies-measure, which that scheduler serves
STAGE_MARKER = "movies-lanes-stage"
# ...and the consumer lane's (measure-lane.sh `consumer`), likewise
CONSUMER_MARKER = "movies-lanes-consumer"
# THE CHECK LANES (2026-11-17: red fix 20261117-110122-...-2ed6cd, Fixes-Red 2564, was still checking 41 min after it
# was queued: agentctl puts a red fix first in its own queue, but its precommit-check jobs then waited ~9.5 min a job
# for a `movies-check` runner like every other landing's). The same as the measure lanes, for the check class: a check
# runner registers ONE of them and a free check start goes to the highest with a job no runner of it waits for. A red
# fix's checks ask for movies-check-red-fix (precommit-check.yml: agentctl pushes them to check/red-<id>).
CHECK_LANES = ("movies-check-red-fix", LABELS["check"])
LANES = {"check": CHECK_LANES, "measure": MEASURE_LANES}
# OLDEST FIRST (2026-11-17): a job's label naming its run, which a lane runner registers beside the lane's to take only
# that run's jobs -- the run of the oldest queued job no runner waits for (uncovered). Never a class's or a lane's.
PIN_PREFIX = "movies-run-"
# The measure lanes on a landing's path (a gate-red fix's rows, a landing check's): served ahead of a borrowed check.
# The optional ones are not (the owner, 2026-11-17, "A3"): a landing check borrows ahead of them, and of the explorers'.
LANDING_MEASURE_LANES = MEASURE_LANES[:2]
OPTIONAL_MEASURE_LANES = (STAGE_LANE, LABELS["measure"], CONSUMER_LANE)
CLASS_OF = {label: cls for cls, label in LABELS.items()} | {lane: cls for cls, order in LANES.items() for lane in order}
# The limits that hold a measurement, in whole jobs (Decision.limits; the other entries there only explain them).
ADMISSION_LIMITS = ("memory", "slice", "caps", "cpu", "slots", "peak")


@dataclasses.dataclass(frozen=True)
class Config:
    repo: str = "pawelkrupinski/movies"
    max_slots: int = 10
    # ADMISSION -- every number of the rule in the module docstring, here and nowhere else
    # Each class's expected working set and cores: a measurement holds 10-11 GiB (2026-11-07: 31 GiB for 3, peaks 13);
    # a landing check 4 GiB at its median and at most 9 (ci-runner's journal, 2026-11-05..08: 2361 busy readings, p99 8),
    # so 10; a measurement at the JVM's 4 cores. A landing check is charged 3 of its JVM's 4 (the owner, 2026-11-17,
    # "A1"): a settled check used 2.8 cores at the median (2026-11-15/16), and charged 4 three of them filled their 12
    # threads while the cpuset read half idle. The live reading still stops every start at cpu_start_below, and a
    # settling check owes the rest of its 3 until it shows them, so a burst never starts past that line either
    # (ChargedCheckTest). A light job (a timings report, a prune, a dispatch) runs seconds in under 1 GiB.
    job_expected_gib: dict[str, float] = dataclasses.field(default_factory=lambda: {"check": 10, "measure": 11, "light": 1})
    job_cpus: dict[str, float] = dataclasses.field(default_factory=lambda: {"check": 3, "measure": 4, "light": 1})
    # THE CHECKS' PEAK: what a busy check burns at its p90 -- its sbt forks and docker past the JVM's 4 (ci-runner's
    # journal 2026-11-15..17: two checks 9.3 cores at the p90, three 13.9). Its own cpuset never holds more checks
    # than its threads fit at this, the lane's included: charged 3, a landing's burst of check jobs would otherwise
    # start five on 18 threads, ~23 cores at that p90 -- the 2026-11-03 slowdown (a test phase 6 -> 16 min).
    check_peak_cpus: float = 4.5
    # Every movies runner container's --memory (bitcashier infra passes this same number to the unit). A class whose
    # slice has a memory.max never holds more runners than that many caps fit: what admission charges a job in its
    # slice is what its container may take, so the slice's own OOM killer never fires (2026-11-07 07:40-08:22Z: nine
    # measurements, each capped at 16, in a 48 GiB slice -- 18 processes killed, exit 137).
    container_gib: float = 16
    light_slots: int = 3  # the light lane: slots past max_slots, for light jobs only
    # THE CHECK LANE: this many landing checks' slots, memory (job_expected_gib) and cores (job_cpus) that
    # measurement admission never takes, less the checks already running. Two: a landing check run has a plan job, then
    # the rest, the old refs and identity's shards beside it, and 2-4 check jobs ran at once on most busy ticks.
    check_lane_jobs: int = 2
    # BORROWED CHECKS (the owner, 2026-11-16): a landing check its own cpuset cannot hold may run in the measure slice,
    # at most this many at once, and only while the measure pool keeps measure_reserve_jobs measurements' memory, slice
    # caps and slots past it, and their cores while a landing-path measurement is in flight (borrow_check: what holds
    # one, and the bound on a measurement's delay).
    # Four (the owner, 2026-11-17, "A3": more than 2): the cores and the kept measurement bind first.
    borrow_max_checks: int = 4
    measure_reserve_jobs: int = 1
    light_cpu_start_below: float = 0.95  # the light lane's CPU ceiling: a light job starts on a busy host, not a full one
    memory_margin_gib: float = 6  # left free beyond every reserve, for the running jobs' growth
    # A builder's reserve: the median working set of the busy builder units, within [per_idle_builder_gib,
    # per_busy_builder_cap_gib] (2026-11-07: 34 GiB for 7 busy, ~4.9 each), for builder_burst of the idle builders
    per_idle_builder_gib: float = 4
    per_busy_builder_cap_gib: float = 8
    builder_burst: int = 4  # bitcashier builder jobs that may start between two ticks' readings
    per_idle_deployer_gib: float = 1  # a deploy job holds little (ci-deploy.slice: 0.3 GiB)
    settle_seconds: int = 300  # a runner this young counts at its expected size: its memory has not shown yet
    cpu_start_below: float = 0.85  # of the host's cores, busy over the window: above it, nothing starts
    cpu_window_seconds: int = 300
    ci_cgroup: str = "/sys/fs/cgroup/ci.slice"
    # each class's slice (RUNNER_SLICE), whose own headroom caps that class ({cls} is the class)
    class_cgroup: str = "/sys/fs/cgroup/ci.slice/ci-movies.slice/ci-movies-{cls}.slice"
    proc: str = "/proc"
    builders: int = 12
    deployers: int = 6
    # cgroup directories of bitcashier's runner units (globs), whose processes say whether a job runs
    builder_cgroups: str = "/sys/fs/cgroup/ci.slice/ci-check.slice/github-runner-bitcashier-gh-builder-*.service"
    deployer_cgroups: str = "/sys/fs/cgroup/ci.slice/ci-deploy.slice/github-runner-bitcashier-gh-deployer-*.service"
    idle_stop_seconds: int = 180
    # a class whose oldest queued job waited this long with none of it starting is said as STARVED, with the limit that
    # holds it (Scala Steward #145, 2026-11-08: an hour queued on movies-measure, its stuck red naming no cause)
    starve_report_seconds: int = 900
    tick_seconds: int = 30
    # while a check is queued: a tick this often, so a check job waits less for the scheduler to see it (half a tick on
    # average); the other ticks list only ETag-cached pages (free)
    busy_tick_seconds: int = 15
    # THE WARM LIGHT RUNNER (the owner, 2026-11-18, "10"): this many light runners kept registered and idle, so a
    # precommit-check plan job (light, no pin) never waits for a tick and a registration; within light_slots. None by
    # default, so a decision here names only the jobs it was given; ci-runner's schedulerConfig sets 1 (bitcashier infra)
    warm_light_runners: int = 0
    state_dir: str = "/run/movies-runner"
    unit: str = "movies-runner"
    token_helper: str = ""
    reader_app_id: str = ""
    reader_key: str = ""

    def __post_init__(self) -> None:
        for name in ("job_expected_gib", "job_cpus"):
            if set(getattr(self, name)) != set(CLASSES):
                raise ValueError(f"{name} must name every class {list(CLASSES)}: {getattr(self, name)}")

    @staticmethod
    def load(machine: Machine, path: str) -> Config:
        return Config.parse(json.loads(machine.read_text(path)), path)

    @staticmethod
    def parse(raw: dict[str, Any], source: str) -> Config:
        """The config a JSON object says (`source` names it in the error): an unknown key fails rather than being ignored."""
        known = {f.name for f in dataclasses.fields(Config)}
        unknown = sorted(set(raw) - known)
        if unknown:
            raise ValueError(f"{source}: unknown keys {unknown}")
        return Config(**raw)


@dataclasses.dataclass(frozen=True)
class Bitcashier:
    busy_builders: int
    busy_deployers: int


@dataclasses.dataclass(frozen=True)
class Instance:
    slot: int
    cls: str
    busy: bool
    idle_since: float  # when it was last seen not busy (its start, at first)
    started: float = 0.0  # when the scheduler started it
    # its container's own cgroup, as last read (None: not read, or no container yet)
    mem_gib: float | None = None  # working set now
    cpu_cores: float | None = None  # cores it used over the CPU window (or its life, if shorter)
    cpu_seconds: float | None = None  # CPU time it has used in all
    # the class whose slice it runs in (its CPUs, its memory.max): empty for its own; "measure" for a borrowed check
    slice_class: str = ""
    label: str = ""  # the label it registered (a measure lane's); empty: its class's
    pin: str = ""  # the run's pin label it registered beside it (PIN_PREFIX...); empty: none, it takes any of its lane's

    @property
    def lane(self) -> str:
        return self.label or LABELS[self.cls]

    @property
    def runs_in(self) -> str:
        return self.slice_class or self.cls

    @property
    def borrowed(self) -> bool:
        return self.runs_in != self.cls


@dataclasses.dataclass(frozen=True)
class ClassCpu:
    """The CPUs a class's slice may run on (its cpuset.cpus.effective) and how many of them were busy over the window
    (None: one sample, no rate yet)."""
    cpus: frozenset[int]
    busy_cores: float | None


@dataclasses.dataclass(frozen=True)
class Readings:
    """The host as it is now, in GiB and cores. A class without a slice limit has no class_free_gib entry; no CPU rate
    yet (one sample) is cpu_busy_cores None. A class whose slice's cpuset was read has a class_cpu entry, and is
    admitted on those CPUs alone; without one, on the host's."""
    mem_available_gib: float
    ci_free_gib: float | None
    class_free_gib: dict[str, float]
    cpu_busy_cores: float | None
    cpu_cores: int
    busy_builder_gib: tuple[float, ...] = ()  # the working set of each builder unit running a job
    cpu_span_seconds: float | None = None  # the span cpu_busy_cores averages over
    class_cpu: dict[str, ClassCpu] = dataclasses.field(default_factory=dict[str, ClassCpu])  # class -> ClassCpu
    class_max_gib: dict[str, float] = dataclasses.field(default_factory=dict[str, float])  # class -> its slice's memory.max (limited ones only)


@dataclasses.dataclass(frozen=True)
class Decision:
    room: int  # how many measurements memory, CPU and slots held this tick, past the check lane; 0 when saturated
    saturated: bool
    starts: tuple[tuple[int, str], ...]  # ((slot, cls), ...)
    stops: tuple[int, ...]  # (slot, ...)
    # what held a measurement this tick, before the starts: ADMISSION_LIMITS in whole jobs, plus the
    # charge of our settling runners (the log line names the binding term)
    limits: dict[str, object] = dataclasses.field(default_factory=dict[str, object])
    borrowed: tuple[int, ...] = ()  # the slots of `starts` that are checks started in the measure slice (borrow_check)
    labels: tuple[tuple[int, str], ...] = ()  # ((slot, label), ...): the lane each measurement start registers (MEASURE_LANES)
    pins: tuple[tuple[int, str], ...] = ()  # ((slot, pin), ...): the run's pin each lane start registers beside it ("": none; OLDEST FIRST)
    warm: tuple[int, ...] = ()  # the slots of `starts` that are light runners kept idle past the queued light jobs (THE WARM LIGHT RUNNER)



@dataclasses.dataclass(frozen=True)
class Queued:
    """A queued job of a laned class: its lane, its run's pin label ("": it asks for none) and when it was created."""
    lane: str
    pin: str = ""
    created: float = 0.0

