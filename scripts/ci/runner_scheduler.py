#!/usr/bin/env python3
"""CI-RUNNER'S MOVIES RUNNER SCHEDULER (the owner, 2026-11-06: "make movies runs take all the ci-runner runners when
bitcashier is not using them"). ci-runner is bitcashier's CI host; movies borrows what bitcashier leaves idle.

WHAT IT DECIDES, every tick (30 s): how many movies runners may exist now, and which job class each new one serves.

  * BITCASHIER ALWAYS WINS. Its 12 builders and 6 deployers are persistent runners on the host (default.nix
    `ghRunners`); a bitcashier job waits only when every runner of its kind is busy. So while every builder or every
    deployer is busy (bitcashier has, or may have, queued work beyond its free runners) NO check or measurement runner
    is started, and the idle ones are stopped. Movies jobs already running finish, under ci-movies.slice's lower CPU
    weight. Only the light lane (below) still admits: a 1 GiB, 1-core job holds no bitcashier job back.
  * ADMISSION IS WHAT IS FREE NOW, memory AND CPU (the owner, 2026-11-07: "fix the schedule so runners don't stay
    idle" ... "don't let cpu be underutilized"). The fixed worst-case budgets this replaced (8 GiB per busy bitcashier
    job, 16 GiB per movies job in its slice) held movies at 3 runners with 58 GiB free. Every tick reads, live:
      - MEMORY: the tighter of the host's MemAvailable and ci.slice's headroom (memory.max less its working set:
        memory.current without inactive page cache, which the kernel takes back first), less a reserve for the
        bitcashier jobs that may start before the next reading counts them, less `memory_margin_gib`, must hold the
        new job's class's `job_expected_gib` (a measurement 11 GiB, a landing check 10, the container's --memory=16g
        bounding the worst one; a light job 1).
        The builder reserve is `builder_burst` idle builders (or fewer, when fewer are idle), each at what a BUSY
        builder holds now: the median working set of the builder units running a job, at least
        `per_idle_builder_gib` (none busy: that floor) and at most `per_busy_builder_cap_gib`. Not every idle builder
        at full size: on 2026-11-07 13:30Z one busy builder "held" the whole ci-check.slice (the idle runners' own
        processes and caches) and twelve times that left room 0 with 87 GiB free and 10 of 32 cores busy -- the
        quieter bitcashier was, the less movies ran. A bitcashier job queues only once every runner of its kind is
        busy (saturated, below), so bitcashier's queue needs no reserve of its own; a burst beyond `builder_burst`
        is caught by the next tick (idle movies runners stop) and, under real pressure, by the kernel: movies'
        containers run at oom_score_adj 1000 (bitcashier infra, movies-runners.nix), so an OOM kill takes movies
        work, never bitcashier's, and bitcashier's tier slices keep their memory.low. An idle deployer reserves
        `per_idle_deployer_gib`.
      - THE CLASS'S SLICE likewise: its memory.max less its working set must hold the margin and the new job. Verify
        scrape recording #14 ran nine measurements in a 48 GiB slice, and on 2026-11-07 07:40-08:22Z six-then-nine did:
        its OOM killer took 18 processes (exit 137).
      - THE SLICE'S CAPS (the owner, 2026-11-08: "cap = charge"): a limited slice never holds more of its class's
        runners than its memory.max fits containers at their --memory (`container_gib`, 16 GiB: every movies job's peak
        fits it -- the United States' convergence rows 13-14 GiB, every other country's rows at most 11, a landing check
        9, over ci-runner's journal 2026-11-07..08 -- and so does the largest heap on the label, identity-live-drift's
        11g). The working set alone let settled measurements at 7 GiB admit more than the slice holds at their peaks;
        with what a job may take charged, the slice's OOM killer cannot fire on movies' own growth.
      - CPU: the busy cores over `cpu_window_seconds` (/proc/stat) OF THE CPUS THE CLASS'S SLICE RUNS ON (its
        cpuset.cpus.effective; unread, the host's), up to `cpu_start_below` of them (`light_cpu_start_below` for a
        light job), must hold a job of the class at the cores it CAN BURN, `job_cpus` (the JVM's
        ActiveProcessorCount=4 of a check or measurement container; 1 for a light job) -- and every SETTLING runner
        of ours on those CPUs is charged the same cap, less what the busy reading already holds of it (cpu.stat: its
        CPU time over the window's span). Charging new jobs what jobs USED (their median, 2026-11-07..09) fed back
        on itself: on 11-09 20:32-20:34Z a burst of drift rows was charged the 1.3 cores contended measurements had
        shown, six started beside ~11 cores of other load on the measure cpuset's 20, the CPUs read 20/20 within six
        minutes, and a UK drift row took 900 s (527 s on a quiet ci-runner, 375 s hosted). At the cap that burst
        starts one. A SETTLED runner is in the reading (its window's cores); of the settled ones only the largest
        rest of a cap is charged on top, for one job ramping up between readings (cpu_owed). Charging every settled
        job its whole cap (2026-11-10..16) held checks at 2 and measurements at 3-4 while their cpusets read half
        idle (2026-11-15/16: checks 2.8 cores at the median, measurements 2.2, the measure cpuset never past 15 of
        20) -- the owner, 2026-11-16: "allow even more in parallel as long as mem and cpu are not maxed out". CPU,
        unlike memory, is not cap = charge: an overcommitted core slows movies' jobs (bitcashier's slices outweigh
        ours), it kills none. On a saturated host another job only slows every job for no throughput, so nothing
        starts past cpu_start_below of the live reading. Per cpuset, not
        per host (the owner, 2026-11-08): measurements run on CCD1 and two CCD0 cores, checks on the rest of CCD0
        (bitcashier infra), and the host-wide reading both packed nine and ten measurements onto CCD1's 16 threads
        beside an idle CCD0 (2026-11-08 04:36-06:00Z, ~1.8 cores each: Germany's drift rows 36-51 min, 14-15 with
        three) and held them back for bitcashier's load on CPUs they cannot use. Classes on disjoint CPUs never
        spend each other's cores, so the check lane's cores are kept from measurements only where they share CPUs.
    A runner of ours that has not fully shown its memory and CPU yet -- one waiting for its job, or started within
    `settle_seconds` -- is charged what it has NOT shown yet: its class's expected size less its container's working
    set (and its cores' cap less what it showed). So a queue never starts a burst into memory
    that only looked free, and what a young runner already holds is not counted twice (15:46Z: three runners 3-4 min
    old were charged 11 GiB and 4 cores each beside what the readings already had of them: room 0 on 13 of 32
    cores). The memory margin is each reading's once, never per job. The log line names each term (`held by`).
    A job starts only when all three hold it, never past its slots; when memory runs short of what our waiting runners
    would take (bitcashier started work), the idle ones go, lowest class first.
  * PRIORITY between movies' own jobs: landing checks (`movies-check`), then measurements (`movies-measure`), in the
    `max_slots` slots. A runner is started for a QUEUED job of a class only (ephemeral, one job, the label of that
    class alone), the highest class first, so a free slot goes to a waiting landing check before a measurement.
  * THE CHECK LANE (2026-11-08 05:50Z: a landing check queued behind nine measurements, 'memory': 0; agentctl's remote
    check gives up at 80 min, and land entry ws3b bounced checking -> queued for ~2.5 h). Priority alone reserved
    nothing: measurements took every GiB the moment no check was queued, so the next check waited for one to END.
    Measurement admission now leaves `check_lane_jobs` checks' slots, memory and cores free, less the checks running:
    a queued check starts on the next tick whenever bitcashier is not saturated. Checks past the lane compete as before.
  * BORROWED CHECKS (the owner, 2026-11-16: "let landing checks borrow the idle measurement CPUs"): a check its own
    cpuset cannot hold may start in the measure slice (RUNNER_SLICE; its label stays movies-check) when the live
    reading of the measure CPUs, less what our runners there owe it (cpu_owed), holds the check's cores -- measured
    CPU, as every other start; a ledger of every job there at its whole cap never let a check borrow beside any
    measurement on 14 threads (2026-11-19) -- only while every queued red-fix or landing measurement has its runner,
    at most borrow_max_checks at once, and always leaving one measurement's memory, slice cap and slot, and its cores
    while a landing-path measurement is in flight (borrow_check). Optional measurements (movies-measure) wait behind a
    borrow (the owner, 2026-11-17, "A3").
  * CHECK CAPACITY (the owner, 2026-11-17, "A1-3": checks waited ~20 min for a slot and ran ~10): a check is charged 3
    cores, not its JVM's 4 (a settled one used 2.6 at the median), its cpuset is 18 threads (bitcashier infra), and
    its own cpuset never holds more checks than its threads fit at their p90, check_peak_cpus (the 2026-11-03 lesson).
  * THE MEASURE LANES (2026-11-16: Main red, and its fix's pre-push drift check -- Identity takes 43816825023, ten rows
    on movies-measure -- ran 1-3 rows at a time for 80+ min while 14-27 optional measurement jobs shared the label;
    GitHub hands a runner to any job its labels match). A measurement runner registers ONE lane's label
    (MEASURE_LANES): movies-red-fix, movies-landing, movies-stage (deploy staging, the owner 2026-11-19), movies-measure,
    movies-consumer (a row waiting on another row's upload, the owner 2026-11-19), movies-explore (below). Each free measure start
    goes to the highest lane with a job no runner of that lane waits for, so a landing check's rows take the next measure slot ahead of every
    optional measurement, a gate-red fix's ahead of those. In everything else a lane is a measurement: size, slice,
    admission, caps. Light runners also register LANES_MARKER, which says to a plan job (scripts/ci/measure-lane.sh)
    that this scheduler serves the lanes.
  * THE EXPLORER LANE (the owner, 2026-11-18, "7"): the nightly drift and family explorers' rows ask for movies-explore,
    a fourth measure lane below movies-measure, and none of it starts while a landing check waits for a runner; an idle
    explorer runner then goes. On 2026-11-17 21:15-21:40Z the drift explorer's rows refilled the measure CPUs each
    time one ended, and a landing check could not borrow them ("held by cpu", borrowed []). A running row is never
    stopped: the rows yield the measure CPUs as they end. measure-lane.sh asks for the lane only where a light runner
    says this scheduler serves it (EXPLORE_MARKER).
  * THE BUSY TICK: while a check job is queued, a tick every busy_tick_seconds, not tick_seconds, so it waits less
    for this to see it. (THE WARM CHECK RUNNER, 2026-11-18..21, registered a pinned check runner ahead for a running
    precommit-check plan's later checks; retired 2026-11-21, the owner: none of a landing check's jobs that wait on the
    plan runs here any more -- the old refs went to Main, identity's shards to GitHub's hosted runners -- so it held an
    idle check runner ~60 s each landing, beside bitcashier's.)
  * THE CHECK LANES (2026-11-17: a red fix still checking 41 min after it was queued, its jobs queued for
    `movies-check` runners with every other landing's): the same for the check class, CHECK_LANES
    movies-check-red-fix then movies-check; a red fix's precommit-check jobs ask for the first.
  * OLDEST FIRST (2026-11-17: precommit-check run 44061032302's `heavy rows (e2e)`, created 10:09Z, queued until 10:45Z
    while later landings' check jobs started). A runner registered with a lane's label alone is handed WHICHEVER queued
    job of the lane GitHub picks, not the oldest, so under a backlog one change's jobs were overtaken again and again.
    A job may also ask for its run's PIN label (PIN_PREFIX + the run id: precommit-check.yml's every check job), and a
    lane runner then registers the lane's label plus the pin of the OLDEST queued job of the lane no runner already
    waits for (uncovered): GitHub hands a runner only a job whose labels it has all, so that runner can take that run's
    jobs alone. A runner waiting with a pin covers its run's jobs only; a job with no pin is served as before.
  * THE LIGHT LANE (the owner, 2026-11-07: "why was it waiting so long, can you fix that?"): the light jobs (`movies`:
    timings reports, prunes, dispatches -- seconds, under 1 GiB) have `light_slots` slots of their own past
    `max_slots`, admitted at their own size whatever the heavy queue or bitcashier's saturation. Charged a
    measurement's 11 GiB and 4 cores and ranked last, identity run 42370669779's 10-second timings job waited 26 min
    (12:32-12:58Z) behind 20 queued measurements.
  * THE WARM LIGHT RUNNER (the owner, 2026-11-18, "10"): warm_light_runners light runners (ci-runner: 1) are kept
    registered and idle past the queued light jobs, so a landing check's plan job -- light, pinned to nothing, and
    every check job after it waiting for it -- is taken the moment it queues, not a tick and a registration later.
  * MAIN NEVER RUNS HERE (the owner, 2026-11-06, the same day's "main's convergence takes ci-runner's free slots"
    reversed: "run main branch convergence and Main workflow always on gh, measurements always on ci"). Main's jobs ask
    for GitHub's hosted runners alone, so nothing here serves them and nothing is published for them.

THE RUNNERS are instances of the template unit movies-runner@<slot>.service (bitcashier infra,
nix/hosts/ci-runner/movies-runners.nix): one container, GitHub's runner registered --ephemeral with the labels this
writes to <state>/<slot>.env, one job, then the unit ends and the slot is free. An instance is never stopped while it
runs a job (a Runner.Worker in its container).

  runner_scheduler.py run --config FILE             the host loop (ci-runner's movies-runner-scheduler unit)
  runner_scheduler.py tick --config FILE [--dry-run] one pass, printing what it saw and did

Tested by runner_scheduler_test.py.
"""
from __future__ import annotations

import dataclasses
import datetime
import glob
import json
import os
import statistics
import subprocess
import sys
import time
import urllib.error
import urllib.request

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
    job_expected_gib: dict = dataclasses.field(default_factory=lambda: {"check": 10, "measure": 11, "light": 1})
    job_cpus: dict = dataclasses.field(default_factory=lambda: {"check": 3, "measure": 4, "light": 1})
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

    def __post_init__(self):
        for name in ("job_expected_gib", "job_cpus"):
            if set(getattr(self, name)) != set(CLASSES):
                raise ValueError(f"{name} must name every class {list(CLASSES)}: {getattr(self, name)}")

    @staticmethod
    def load(path: str) -> "Config":
        with open(path) as f:
            raw = json.load(f)
        known = {f.name for f in dataclasses.fields(Config)}
        unknown = sorted(set(raw) - known)
        if unknown:
            raise ValueError(f"{path}: unknown keys {unknown}")
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
    cpus: frozenset
    busy_cores: float | None


@dataclasses.dataclass(frozen=True)
class Readings:
    """The host as it is now, in GiB and cores. A class without a slice limit has no class_free_gib entry; no CPU rate
    yet (one sample) is cpu_busy_cores None. A class whose slice's cpuset was read has a class_cpu entry, and is
    admitted on those CPUs alone; without one, on the host's."""
    mem_available_gib: float
    ci_free_gib: float | None
    class_free_gib: dict
    cpu_busy_cores: float | None
    cpu_cores: int
    busy_builder_gib: tuple = ()  # the working set of each builder unit running a job
    cpu_span_seconds: float | None = None  # the span cpu_busy_cores averages over
    class_cpu: dict = dataclasses.field(default_factory=dict)  # class -> ClassCpu
    class_max_gib: dict = dataclasses.field(default_factory=dict)  # class -> its slice's memory.max (limited ones only)


@dataclasses.dataclass(frozen=True)
class Decision:
    room: int  # how many measurements memory, CPU and slots held this tick, past the check lane; 0 when saturated
    saturated: bool
    starts: tuple  # ((slot, cls), ...)
    stops: tuple  # (slot, ...)
    # what held a measurement this tick, before the starts: ADMISSION_LIMITS in whole jobs, plus the
    # charge of our settling runners (the log line names the binding term)
    limits: dict = dataclasses.field(default_factory=dict)
    borrowed: tuple = ()  # the slots of `starts` that are checks started in the measure slice (borrow_check)
    labels: tuple = ()  # ((slot, label), ...): the lane each measurement start registers (MEASURE_LANES)
    pins: tuple = ()  # ((slot, pin), ...): the run's pin each lane start registers beside it ("": none; OLDEST FIRST)
    warm: tuple = ()  # the slots of `starts` that are light runners kept idle past the queued light jobs (THE WARM LIGHT RUNNER)


# ── the decision, pure ──────────────────────────────────────────────────────────────────────────────────────────────
def saturated(cfg: Config, bc: Bitcashier) -> bool:
    """Every builder or every deployer busy: a bitcashier job may be waiting for a runner, so movies starts nothing."""
    return bc.busy_builders >= cfg.builders or bc.busy_deployers >= cfg.deployers


def _whole_jobs(free: float, size: float) -> int:
    return int(free // size)


def reserved_builders(cfg: Config, bc: Bitcashier) -> int:
    """How many bitcashier builder jobs the reserve holds room for: builder_burst, or every idle builder if fewer."""
    return min(cfg.builder_burst, max(0, cfg.builders - bc.busy_builders))


def builder_reserve_gib(cfg: Config, r: Readings) -> float:
    """What one more bitcashier builder job is reserved: the median busy builder's working set, within the floor and
    the cap (none busy: the floor)."""
    if not r.busy_builder_gib:
        return cfg.per_idle_builder_gib
    return min(cfg.per_busy_builder_cap_gib, max(cfg.per_idle_builder_gib, statistics.median(r.busy_builder_gib)))


def memory_free_gib(cfg: Config, bc: Bitcashier, r: Readings, pending_gib: float) -> float:
    """The memory free for more movies jobs now: the tighter of MemAvailable and ci.slice's headroom, less a reserve for
    the bitcashier jobs that may start (builder_burst builders, every idle deployer), the margin, and `pending_gib` of
    our runners that have not shown their memory yet."""
    free = r.mem_available_gib if r.ci_free_gib is None else min(r.mem_available_gib, r.ci_free_gib)
    reserve = (builder_reserve_gib(cfg, r) * reserved_builders(cfg, bc)
               + cfg.per_idle_deployer_gib * max(0, cfg.deployers - bc.busy_deployers))
    return free - reserve - cfg.memory_margin_gib - pending_gib


def class_free_gib(cfg: Config, r: Readings, cls: str, pending_gib: float) -> float | None:
    """The memory `cls`'s own slice has for more jobs now, past the margin; None when the slice has no limit."""
    free = r.class_free_gib.get(cls)
    return None if free is None else free - cfg.memory_margin_gib - pending_gib


def cpu_of(r: Readings, cls: str | None) -> tuple:
    """(busy cores, cores) of the CPUs `cls` runs on: its slice's cpuset when read, else (or for None) the host's."""
    own = r.class_cpu.get(cls)
    return (r.cpu_busy_cores, r.cpu_cores) if own is None else (own.busy_cores, len(own.cpus))


def share_cpus(r: Readings, a: str, b: str) -> bool:
    """Whether jobs of classes `a` and `b` can run on the same CPUs: their cpusets meet, or either is unknown (the
    host's). Disjoint classes never take each other's cores: a measurement is not held by the check lane's cores."""
    ca, cb = r.class_cpu.get(a), r.class_cpu.get(b)
    return ca is None or cb is None or bool(ca.cpus & cb.cpus)


def cpu_spare(cfg: Config, r: Readings, pending_cpus: float, start_below: float, cls: str | None = None) -> float:
    """The cores free for more `cls` jobs on the CPUs it runs on (cpu_of): busy cores up to `start_below` of them, less
    `pending_cpus` of our runners on those CPUs that have not shown their CPU yet. No rate yet (one sample): none."""
    busy, cores = cpu_of(r, cls)
    if busy is None:
        return 0.0
    return start_below * cores - busy - pending_cpus


def settling(cfg: Config, inst: Instance, now: float) -> bool:
    """Not fully in the readings yet: waiting for its job, or started within settle_seconds."""
    return not inst.busy or now - inst.started < cfg.settle_seconds


def memory_charge(cfg: Config, inst: Instance) -> float:
    """What a settling runner still owes the memory readings: its class's expected size less what its container
    already holds (and so is already out of MemAvailable and the slices' headroom)."""
    return max(0.0, cfg.job_expected_gib[inst.cls] - (inst.mem_gib or 0.0))


def cpu_charge(cfg: Config, r: Readings, inst: Instance, now: float) -> float:
    """The cores a runner of ours may still grow by past what the CPU reading holds of it: its job's cap (job_cpus)
    less what it showed -- settled, the cores its container used over the window; settling, its CPU time over the
    span the busy cores average (all of it fell inside: it started within settle_seconds). Never below 0: a job past
    its cap is all in the reading. cpu_owed says how much of it admission charges."""
    if settling(cfg, inst, now):
        shown = inst.cpu_seconds / r.cpu_span_seconds if inst.cpu_seconds is not None and r.cpu_span_seconds else 0.0
    else:
        shown = inst.cpu_cores or 0.0
    return max(0.0, cfg.job_cpus[inst.cls] - shown)


def cpu_owed(cfg: Config, r: Readings, runners: list, now: float) -> float:
    """What `runners` (ours, on one cpuset) are charged past the CPU reading: every settling one the whole rest of its
    cap (a burst of young runners never starts into cores that only looked free: DriftBurstTest), and of the settled
    ones only the largest such rest -- ONE settled job ramping up between readings, not all of them at once.
    Charging every settled job its cap (2026-11-10..16) held checks at 2 on 12 threads with 5-6 busy and measurements
    at 3-4 on 20 with 4-7 busy: 2026-11-15/16, 24 h of ci-runner's journal, a settled check used 2.8 cores at the
    median, a measurement 2.2, and the measure cpuset never read more than 15 of its 20 threads busy while 1569 ticks
    had measurements queued and 1183 of them were held by the CPU. A settled job's later growth is in the next
    readings, which stop further starts at cpu_start_below; past that, CPU contention slows movies' own jobs only
    (bitcashier's slices outweigh ci-movies.slice 15-50 to 1) and kills nothing -- unlike memory, where cap = charge
    stays (an overcommitted slice's OOM killer takes jobs).
    A borrowed check is charged like a settling one, always: it took cores measurements own, so they never start into
    the rest of its cap (borrow_check)."""
    young = sum(cpu_charge(cfg, r, i, now) for i in runners if settling(cfg, i, now) or i.borrowed)
    return young + max((cpu_charge(cfg, r, i, now) for i in runners if not (settling(cfg, i, now) or i.borrowed)),
                       default=0.0)


@dataclasses.dataclass(frozen=True)
class Queued:
    """A queued job of a laned class: its lane, its run's pin label ("": it asks for none) and when it was created."""
    lane: str
    pin: str = ""
    created: float = 0.0


def uncovered(queue: list, waiting: list) -> list:
    """The jobs of `queue` (oldest first) that no runner of `waiting` ((lane, pin) each) covers: a waiting runner covers
    the oldest job of its lane and pin not covered yet -- a pinned runner can take its run's jobs alone, an unpinned
    one the unpinned (one a pinned runner happens to take leaves its run's job to the next tick)."""
    left = {}
    for key in waiting:
        left[key] = left.get(key, 0) + 1
    out = []
    for q in queue:
        if left.get((q.lane, q.pin), 0):
            left[(q.lane, q.pin)] -= 1
        else:
            out.append(q)
    return out


def queued_for(inst: Instance, demand: dict, queue: list) -> int:
    """The queued jobs `inst` can take: its lane's of its pin (a lane runner takes no other lane's job, a pinned one no
    other run's), else its class's."""
    if inst.cls in LANES:
        return sum(1 for q in queue if q.lane == inst.lane and q.pin == inst.pin)
    return demand.get(inst.cls, 0)


def _idle_to_stop(cfg: Config, bc: Bitcashier, demand: dict, instances: list, now: float, r: Readings,
                  queue: list) -> list:
    """Bitcashier saturated: the idle heavy runners (a light lane runner is too small to hold bitcashier back). Past
    max_slots, or memory short of what our settling runners still owe: idle ones, lowest class first. An idle borrowed
    check while a queued landing-path measurement has no runner of its lane waiting (it holds that one's slot, cap and
    cores). And an idle runner whose
    class (a lane runner's lane and pin) has nothing queued for longer than idle_stop_seconds."""
    rank = {c: i for i, c in enumerate(CLASSES)}
    idle = [i for i in instances if not i.busy]
    stops = [i.slot for i in idle if i.cls != "light"] if saturated(cfg, bc) else []
    excess = max(0, sum(1 for i in instances if i.slot <= cfg.max_slots) - cfg.max_slots)
    short = -memory_free_gib(cfg, bc, r, sum(memory_charge(cfg, i) for i in instances if settling(cfg, i, now)))
    for inst in sorted(idle, key=lambda i: (-rank[i.cls], i.slot)):
        if excess <= 0 and short <= 0:
            break
        if inst.slot not in stops:
            stops.append(inst.slot)
            excess -= 1
            short -= memory_charge(cfg, inst)
    # a landing-path measurement with no runner of its lane waiting for it (one that has its runner needs nothing the
    # borrow holds; an optional measurement waits behind a borrowed check, LANDING_MEASURE_LANES)
    waiting_for = [q for q in uncovered(queue, [(i.lane, i.pin) for i in idle if i.cls == "measure" and i.slot not in stops])
                   if q.lane in LANDING_MEASURE_LANES]
    stops += [i.slot for i in idle if i.slot not in stops and i.borrowed and waiting_for]
    # THE WARM LIGHT RUNNER: the lowest-slot idle light runners, up to warm_light_runners, are never idle too long
    warm_light = [i.slot for i in sorted(idle, key=lambda i: i.slot) if i.cls == "light" and i.slot not in stops]
    warm_light = warm_light[:cfg.warm_light_runners]
    return stops + [i.slot for i in idle if i.slot not in stops and i.slot not in warm_light
                    and queued_for(i, demand, queue) == 0 and now - i.idle_since >= cfg.idle_stop_seconds]


def decide(cfg: Config, bc: Bitcashier, demand: dict, instances: list, now: float, r: Readings,
           lanes: dict | None = None, queue: list | None = None) -> Decision:
    """The runners to start ((slot, class), highest class first, then the light lane) and the idle ones to stop, for
    `demand` (queued jobs per class), the laned classes' queued jobs (`queue`, queue_of; or only their counts per lane,
    `lanes`, none pinned; neither given: all in the classes' plain labels' lanes), the `instances` that exist and the
    host's live readings `r`."""
    counted = dict(lanes or {})
    jobs: list = sorted(queue if queue is not None else [Queued(lane) for lane, n in counted.items() for _ in range(n)],
                        key=lambda q: q.created)
    for cls, order in LANES.items():  # a class whose lanes nobody counted: all its demand in its plain label's lane
        if not any(lane in counted for lane in order) and not any(q.lane in order for q in jobs):
            jobs += [Queued(LABELS[cls])] * demand.get(cls, 0)
    full = saturated(cfg, bc)
    stops = _idle_to_stop(cfg, bc, demand, instances, now, r, jobs)
    alive = [i for i in instances if i.slot not in stops]
    used = {i.slot for i in instances}  # a stopping slot is not reused in the same tick
    pending = [i for i in alive if settling(cfg, i, now)]
    owed_cpus = cpu_owed(cfg, r, alive, now)
    memory = memory_free_gib(cfg, bc, r, sum(memory_charge(cfg, i) for i in pending))
    # each class's spare cores on its own CPUs, less what our runners there are charged past the reading (cpu_owed)
    cpu = {cls: cpu_spare(cfg, r, cpu_owed(cfg, r, [i for i in alive if share_cpus(r, cls, i.runs_in)], now),
                          cfg.light_cpu_start_below if cls == "light" else cfg.cpu_start_below, cls)
           for cls in CLASSES}
    # the check lane's cores are kept from measurements only where checks could run on them
    lane_shares_cpus = share_cpus(r, "measure", "check")
    held = {cls: class_free_gib(cfg, r, cls, sum(memory_charge(cfg, i) for i in pending if i.runs_in == cls))
            for cls in CLASSES}
    # each limited slice's room in whole container caps, less the runners of its class alive (busy or waiting)
    caps = {cls: _whole_jobs(r.class_max_gib[cls], cfg.container_gib) - sum(1 for i in alive if i.runs_in == cls)
            for cls in CLASSES if cls in r.class_max_gib}
    heavy_slots = [s for s in range(1, cfg.max_slots + 1) if s not in used]
    size = cfg.job_expected_gib["measure"]
    starts = []
    borrowed = []

    def lane() -> tuple:
        """(slots, GiB, cores) the check lane still holds: check_lane_jobs less the checks running or starting, each at
        what a new check is charged (none settled: job_cpus, so measured cores would leave a fresh one short). Only
        the checks on their OWN cpuset count: a borrowed one runs on the measure CPUs (2026-11-17 09:23Z: two
        borrowed checks held the lane at 0 while the checks' 12 threads were 3 busy and 6 checks queued 21 min)."""
        free = max(0, cfg.check_lane_jobs - sum(1 for i in alive if i.cls == "check" and not i.borrowed)
                   - sum(1 for s, c in starts if c == "check" and s not in borrowed))
        return free, free * cfg.job_expected_gib["check"], free * cfg.job_cpus["check"]

    def peak_room() -> int | None:
        """How many more checks the checks' own cpuset holds at their peak (check_peak_cpus), less the checks on it,
        running or starting (a borrowed one runs on the measure CPUs); None: the cpuset was not read."""
        own = r.class_cpu.get("check")
        if own is None:
            return None
        return (_whole_jobs(len(own.cpus), cfg.check_peak_cpus)
                - sum(1 for i in alive if i.cls == "check" and not i.borrowed)
                - sum(1 for s, c in starts if c == "check" and s not in borrowed))

    lane_slots, lane_gib, lane_cpus = lane()
    limits = {"memory": _whole_jobs(memory - lane_gib, size),
              "slice": None if held["measure"] is None else _whole_jobs(held["measure"], size),
              "caps": caps.get("measure"),
              "cpu": _whole_jobs(cpu["measure"] - (lane_cpus if lane_shares_cpus else 0), cfg.job_cpus["measure"]),
              "slots": max(0, len(heavy_slots) - lane_slots),
              "settling": f"{len(pending)} owe {sum(memory_charge(cfg, i) for i in pending):.0f} GiB",
              "owed_cores": round(owed_cpus, 1),
              "check_lane": f"{lane_slots} x {cfg.job_expected_gib['check']} GiB {cfg.job_cpus['check']} cores",
              # a landing check's own terms (no lane kept from it; inside the lane the CPU never holds it), so a
              # starved check names its own cause, not a measurement's (2026-11-15/16: 483 "held by nothing named")
              "check": {"memory": _whole_jobs(memory, cfg.job_expected_gib["check"]),
                        "slice": None if held["check"] is None else _whole_jobs(held["check"], cfg.job_expected_gib["check"]),
                        "caps": caps.get("check"),
                        "cpu": None if lane_slots else _whole_jobs(cpu["check"], cfg.job_cpus["check"]),
                        "slots": len(heavy_slots),
                        "peak": peak_room()}}

    labels = {}  # slot -> the lane a measurement start registers
    pins = {}  # slot -> the run's pin it registers beside it

    def unserved_jobs(label: str) -> list:
        """A lane's queued jobs, oldest first, with no runner of that lane and pin waiting for them or starting this
        tick (uncovered)."""
        waiting = ([(i.lane, i.pin) for i in alive if i.cls == CLASS_OF[label] and i.lane == label and not i.busy]
                   + [(lane_label, pins[slot]) for slot, lane_label in labels.items() if lane_label == label])
        return uncovered([q for q in jobs if q.lane == label], waiting)

    def lane_unserved(label: str) -> int:
        """A lane's queued jobs with no runner of that lane waiting for them or starting this tick."""
        return len(unserved_jobs(label))

    def unserved(cls: str, among: tuple | None = None) -> int:
        """Queued jobs of `cls` with no runner waiting for them or starting this tick (a check's or measurement's in its
        lane; `among`: in those of its lanes alone)."""
        if cls in LANES:
            return sum(lane_unserved(label) for label in (among or LANES[cls]))
        return max(0, demand.get(cls, 0) - sum(1 for i in alive if i.cls == cls and not i.busy)
                   - sum(1 for _, c in starts if c == cls))

    def take(slot: int, cls: str, home: str, among: tuple | None = None) -> None:
        """Start `cls` in `home`'s slice: charge its memory, its slice and caps, and its cores on every class sharing
        that slice's CPUs. A check or a measurement registers its class's highest lane still unserved (`among`: of
        those lanes)."""
        nonlocal memory
        size, cores = cfg.job_expected_gib[cls], cfg.job_cpus[cls]
        if cls in LANES:  # the highest lane unserved, pinned to its oldest unserved job's run (OLDEST FIRST)
            label = next(label for label in (among or LANES[cls]) if lane_unserved(label))
            pins[slot] = unserved_jobs(label)[0].pin
            labels[slot] = label
        starts.append((slot, cls))
        if home in caps:
            caps[home] -= 1
        memory -= size
        held[home] = None if held[home] is None else held[home] - size
        for other in cpu:
            if share_cpus(r, other, home):
                cpu[other] -= cores

    def room_for(cls: str, slots: list, keep_measurements: int = 0) -> bool:
        """Whether everything holds one more `cls` job past what is kept: the check lane from a measurement, and
        `keep_measurements` measurements' room."""
        size, cores = cfg.job_expected_gib[cls], cfg.job_cpus[cls]
        # a measurement never takes the check lane (a check, or a light job in its own slots, is not held by it)
        keep_slots, keep_gib, keep_cpus = lane() if cls == "measure" else (0, 0, 0)
        keep_cpus = keep_cpus if lane_shares_cpus else 0
        keep_slots += keep_measurements
        keep_gib += keep_measurements * cfg.job_expected_gib["measure"]
        keep_cpus += keep_measurements * cfg.job_cpus["measure"]
        keep_held = keep_measurements * cfg.job_expected_gib["measure"]
        # the CPUs hold the job -- or it is a check inside the lane, which the CPU never holds: a lane check waits for
        # nothing (2026-11-08 18:55-18:57Z: lane checks queued 57 min "held by cpu" on 5-8 of 12 busy cores; a busy
        # core slows it, under its slice's weight, but never queues it)
        cpu_holds = cpu[cls] - keep_cpus >= cores or (cls == "check" and lane()[0] > 0)
        return (len(slots) > keep_slots and memory - keep_gib >= size and cpu_holds
                and (cls != "check" or (room := peak_room()) is None or room > 0)
                and (held[cls] is None or held[cls] - keep_held >= size) and caps.get(cls, 1) - keep_measurements >= 1)

    def admit(cls: str, slots: list, among: tuple | None = None, keep_measurements: int = 0) -> None:
        """Start runners for `cls`'s unserved jobs (`among`: of those lanes alone) while everything holds one
        (room_for)."""
        need = unserved(cls, among)
        while need and room_for(cls, slots, keep_measurements):
            take(slots.pop(0), cls, cls, among)
            need -= 1

    def borrow_check(slots: list) -> None:
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
            (_idle_to_stop), and at most borrow_max_checks run at once. An optional measurement (movies-measure) is
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
        if share_cpus(r, "check", "measure") or "measure" not in r.class_cpu:
            return  # one cpuset (or unknown ones): nothing to borrow
        check_cores = cfg.job_cpus["check"]
        landing_rows = (any(i.cls == "measure" and i.lane in LANDING_MEASURE_LANES for i in alive)
                        or any(label in LANDING_MEASURE_LANES for label in labels.values()))
        keep_cores = cfg.measure_reserve_jobs * cfg.job_cpus["measure"] if landing_rows else 0
        keep_gib = cfg.measure_reserve_jobs * cfg.job_expected_gib["measure"]
        need = unserved("check")
        while need and len(slots) > cfg.measure_reserve_jobs:
            if (unserved("measure", LANDING_MEASURE_LANES) or sum(1 for i in alive if i.borrowed) + len(borrowed) >= cfg.borrow_max_checks
                    or cpu["measure"] - keep_cores < check_cores
                    or memory - keep_gib < cfg.job_expected_gib["check"]
                    or (held["measure"] is not None and held["measure"] - keep_gib < cfg.job_expected_gib["check"])
                    or caps.get("measure", 1) - cfg.measure_reserve_jobs < 1):
                return
            slot = slots.pop(0)
            take(slot, "check", "measure")
            borrowed.append(slot)
            need -= 1

    warm = []
    if not full:
        admit("check", heavy_slots)
        admit("measure", heavy_slots, LANDING_MEASURE_LANES)
        borrow_check(heavy_slots)
        # optional measurements after the borrows, leaving the kept measurements' room while a borrowed check runs
        any_borrowed = borrowed or any(i.borrowed for i in alive)
        keep = cfg.measure_reserve_jobs if any_borrowed else 0
        admit("measure", heavy_slots, OPTIONAL_MEASURE_LANES, keep_measurements=keep)
        # THE EXPLORER LANE last, and none of it while a check still waits for a runner: an idle explorer runner goes
        # too (what it is charged is what may hold the check's borrow back), and the running rows yield as they end
        if unserved("check"):
            stops += [i.slot for i in alive if not i.busy and i.lane == EXPLORE_LANE and i.slot not in stops]
        else:
            admit("measure", heavy_slots, (EXPLORE_LANE,), keep_measurements=keep)
    # THE LIGHT LANE: its own slots, admitted at a light job's size whatever the heavy queue or bitcashier's runners.
    light_slots = [s for s in range(cfg.max_slots + 1, cfg.max_slots + cfg.light_slots + 1) if s not in used]
    admit("light", light_slots)
    # THE WARM LIGHT RUNNER (the owner, 2026-11-18, "10": precommit-check's plan job queued p50 38-68 s, p90 664-837 s
    # over 2026-11-11..18, and every check job after it waits for it): warm_light_runners light runners kept idle
    # past the queued light jobs, so the next one -- a landing check's plan above all, which asks for no pin -- is taken
    # the moment it queues. Admitted as a light job is (1 GiB, 1 core, its own slots).
    spare = sum(1 for i in alive if i.cls == "light" and not i.busy) + sum(1 for _, c in starts if c == "light") \
        - demand.get("light", 0)
    while spare < cfg.warm_light_runners and room_for("light", light_slots):
        slot = light_slots.pop(0)
        take(slot, "light", "light")
        warm.append(slot)
        spare += 1
    room = 0 if full else max(0, min(v for k, v in limits.items() if k in ADMISSION_LIMITS and isinstance(v, int)))
    return Decision(room, full, tuple(starts), tuple(sorted(stops)), limits, tuple(borrowed),
                    tuple(sorted(labels.items())), tuple(sorted(pins.items())), tuple(warm))


def demand_of(jobs: list) -> dict:
    """Queued jobs per class, from jobs-API job dicts: a job is a class's when its labels name that class's label (or,
    for the measure class, one of its lanes')."""
    out = {c: 0 for c in CLASSES}
    for job in jobs:
        if job.get("status") != "queued":
            continue
        classes = [CLASS_OF[l] for l in job.get("labels", []) if l in CLASS_OF]
        if classes:
            out[min(classes, key=CLASSES.index)] += 1
    return out


def queue_of(jobs: list) -> list:
    """The queued jobs of the laned classes (LANES: the check and the measure lanes), oldest first, from jobs-API job
    dicts: each its lane (a job naming two lanes of a class is the higher's), its pin label (PIN_PREFIX; "" for none)
    and its created_at."""
    out = []
    for job in jobs:
        if job.get("status") != "queued":
            continue
        labels = job.get("labels", [])
        pin = next((l for l in labels if l.startswith(PIN_PREFIX)), "")
        created = iso_seconds(job["created_at"]) if job.get("created_at") else 0.0
        for order in LANES.values():
            named = [l for l in labels if l in order]
            if named:
                out.append(Queued(min(named, key=order.index), pin, created))
    return sorted(out, key=lambda q: q.created)


def pace_seconds(cfg: Config, queue: list, remaining: int) -> int:
    """The loop's pause between ticks: busy_tick_seconds while a check is queued, else tick_seconds; four times that
    when the reader's rate budget runs low (under 1500), so the App's other users never starve."""
    pace = cfg.busy_tick_seconds if any(q.lane in CHECK_LANES for q in queue) else cfg.tick_seconds
    return pace if remaining > 1500 else cfg.tick_seconds * 4


def lane_demand_of(jobs: list) -> dict:
    """Queued jobs per lane (queue_of)."""
    out = {}
    for q in queue_of(jobs):
        out[q.lane] = out.get(q.lane, 0) + 1
    return out


def iso_seconds(stamp: str) -> float:
    """A GitHub timestamp (`2026-11-08T12:23:21Z`) as epoch seconds."""
    return datetime.datetime.fromisoformat(stamp.replace("Z", "+00:00")).timestamp()


def oldest_waits(jobs: list, now: float) -> dict:
    """{class: seconds its oldest queued job has waited}, from jobs-API job dicts (`created_at`), for classes with one."""
    out = {}
    for job in jobs:
        classes = [CLASS_OF[l] for l in job.get("labels", []) if l in CLASS_OF]
        if job.get("status") != "queued" or not classes or not job.get("created_at"):
            continue
        cls = min(classes, key=CLASSES.index)
        out[cls] = max(out.get(cls, 0), int(now - iso_seconds(job["created_at"])))
    return out


def starved(cfg: Config, d: Decision, demand: dict, waits: dict) -> list:
    """A line per class queued past `starve_report_seconds` with none of it starting this tick: how many, how long the
    oldest waited, and what holds it -- bitcashier saturated, or each of the class's admission limits at zero
    (ADMISSION_LIMITS: a measurement's in d.limits, a landing check's in d.limits["check"])."""
    def holding(cls: str) -> list:
        if d.saturated and cls != "light":
            return ["bitcashier saturated"]
        terms = d.limits.get("check", {}) if cls == "check" else d.limits
        return [k for k in ADMISSION_LIMITS if terms.get(k) is not None and terms[k] <= 0]
    started = {cls for _, cls in d.starts}
    return [f"STARVED {cls}: {demand[cls]} queued, the oldest {waits[cls] // 60} min, held by {', '.join(holding(cls)) or 'nothing named'}"
            for cls in CLASSES if demand.get(cls) and cls not in started and waits.get(cls, 0) >= cfg.starve_report_seconds]


# ── the host: bitcashier's runners, ours, GitHub ───────────────────────────────────────────────────────────────────
def _comms(cgroup_dir: str, proc: str = "/proc") -> set:
    """The command names of every process in `cgroup_dir` and below (a process gone meanwhile is skipped)."""
    pids = [pid for procs in glob.glob(os.path.join(cgroup_dir, "**", "cgroup.procs"), recursive=True)
            for pid in (_read(procs) or "").split()]
    return {comm.strip() for comm in (_read(os.path.join(proc, pid, "comm")) for pid in pids) if comm is not None}


def busy_units(pattern: str, proc: str = "/proc") -> list:
    """The runner units matching `pattern` (cgroup directories) that run a job: a Runner.Worker among their processes."""
    return [d for d in glob.glob(pattern) if "Runner.Worker" in _comms(d, proc)]


def busy_runners(pattern: str) -> int:
    return len(busy_units(pattern))


def _read(path: str) -> str | None:
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        return None


def working_set_bytes(cgroup_dir: str) -> int | None:
    """A cgroup's memory.current without its inactive page cache, which reclaim takes back before anything else."""
    current, stat = _read(os.path.join(cgroup_dir, "memory.current")), _read(os.path.join(cgroup_dir, "memory.stat"))
    if current is None or stat is None:
        return None
    inactive = next((int(l.split()[1]) for l in stat.splitlines() if l.startswith("inactive_file ")), 0)
    return int(current) - inactive


def memory_max_gib(cgroup_dir: str) -> float | None:
    """memory.max in GiB; None for no limit ("max") or an unreadable cgroup."""
    limit = (_read(os.path.join(cgroup_dir, "memory.max")) or "").strip()
    return int(limit) / 2**30 if limit.isdigit() else None


def headroom_gib(cgroup_dir: str) -> float | None:
    """memory.max less the working set; None for no limit ("max") or an unreadable cgroup."""
    limit = (_read(os.path.join(cgroup_dir, "memory.max")) or "").strip()
    used = working_set_bytes(cgroup_dir)
    if not limit.isdigit() or used is None:
        return None
    return (int(limit) - used) / 2**30


def cpu_times(stat: str, cpus: frozenset | None = None) -> tuple:
    """(busy, total) jiffies off /proc/stat: the `cpu` line, or the sum of the `cpuN` lines of `cpus`. Busy is all but
    idle and iowait (guest is inside user)."""
    if cpus is None:
        lines = stat.splitlines()[:1]
    else:
        lines = [l for l in stat.splitlines() if l.startswith("cpu") and l[3:4].isdigit() and int(l.split()[0][3:]) in cpus]
    busy = total = 0
    for line in lines:
        v = [int(x) for x in line.split()[1:]]
        busy, total = busy + sum(v[:8]) - v[3] - v[4], total + sum(v[:8])
    return busy, total


def parse_cpu_list(text: str) -> frozenset:
    """A cpuset list ("6-15,22-31") as the set of CPU numbers; empty or unreadable text is the empty set."""
    cpus = set()
    for part in text.strip().split(","):
        if "-" in part:
            low, high = part.split("-")
            cpus.update(range(int(low), int(high) + 1))
        elif part:
            cpus.add(int(part))
    return frozenset(cpus)


class CpuMeter:
    """The host's busy cores over the last `window_seconds`, from /proc/stat samples a tick apart."""

    def __init__(self, window_seconds: int):
        self.window = window_seconds
        self.samples: list = []

    def add(self, now: float, busy: int, total: int) -> None:
        self.samples = [x for x in self.samples if x[0] >= now - self.window] + [(now, busy, total)]

    def span_seconds(self) -> float | None:
        return self.samples[-1][0] - self.samples[0][0] if len(self.samples) >= 2 else None

    def busy_cores(self, cores: int, released_seconds: float = 0.0) -> float | None:
        """The busy cores over the window, less `released_seconds` of CPU time the window holds of jobs that have ended
        (RELEASED ON FINISH, RunnerUsage.ended_seconds)."""
        span = self.span_seconds()
        if span is None or self.samples[-1][2] == self.samples[0][2]:
            return None
        (_, b0, t0), (_, b1, t1) = self.samples[0], self.samples[-1]
        return max(0.0, cores * (b1 - b0) / (t1 - t0) - released_seconds / span)


class CpuMeters:
    """A CpuMeter for the host's busy cores and one for each cpuset a class's slice runs on (keyed by the set, so two
    classes on the same CPUs share one, and a changed cpuset starts afresh)."""

    def __init__(self, window_seconds: int):
        self.window = window_seconds
        self.host = CpuMeter(window_seconds)
        self.sets: dict = {}

    def of(self, cpus: frozenset) -> CpuMeter:
        return self.sets.setdefault(cpus, CpuMeter(self.window))


def class_cpusets(cfg: Config) -> dict:
    """Each class's slice's cpuset.cpus.effective (the CPUs its jobs may run on); a class whose file is unreadable or
    empty has none (admitted on the host's CPUs)."""
    sets = {cls: parse_cpu_list(_read(os.path.join(cfg.class_cgroup.format(cls=cls), "cpuset.cpus.effective")) or "")
            for cls in CLASSES}
    return {cls: cpus for cls, cpus in sets.items() if cpus}


def read_readings(cfg: Config, cpu: CpuMeters, now: float, ended: "RunnerUsage | None" = None) -> Readings:
    """The host now. RELEASED ON FINISH (2026-11-19): the CPU a runner of ours used inside the window before it ended
    (`ended`: what its container's own cgroup showed up to its last tick) is taken out of the busy cores of the CPUs it
    ran on and the host's -- a check job's cores read busy for up to cpu_window_seconds after it ended, while the next
    one starting in its slot was charged its whole cap on top (ci-runner's journal, 2026-11-19: a p90 of 4.6 such
    cores on the check CPUs of the ticks a landing check waited "held by cpu")."""
    meminfo = _read(os.path.join(cfg.proc, "meminfo")) or ""
    available_kb = next((int(l.split()[1]) for l in meminfo.splitlines() if l.startswith("MemAvailable:")), 0)
    stat = _read(os.path.join(cfg.proc, "stat"))
    sets = class_cpusets(cfg)
    if stat:
        cpu.host.add(now, *cpu_times(stat))
        for cpus in set(sets.values()):
            cpu.of(cpus).add(now, *cpu_times(stat, cpus))
    cpu.sets = {cpus: meter for cpus, meter in cpu.sets.items() if cpus in sets.values()}
    cores = sum(1 for l in (stat or "").splitlines() if l.startswith("cpu") and l[3:4].isdigit()) or os.cpu_count() or 1

    def released(meter: CpuMeter, cpus: frozenset | None) -> float:
        """CPU seconds the meter's window holds of our ended runners on `cpus` (None: anywhere, the host's)."""
        if ended is None or meter.span_seconds() is None:
            return 0.0
        return sum(seconds for cls, seconds in ended.ended_seconds(cfg, meter.samples[0][0]).items()
                   if cpus is None or sets.get(cls) == cpus)
    classes = {cls: headroom_gib(cfg.class_cgroup.format(cls=cls)) for cls in CLASSES}
    limits = {cls: memory_max_gib(cfg.class_cgroup.format(cls=cls)) for cls in CLASSES}
    busy = [working_set_bytes(d) for d in busy_units(cfg.builder_cgroups, cfg.proc)]
    return Readings(available_kb / 2**20, headroom_gib(cfg.ci_cgroup),
                    {c: v for c, v in classes.items() if v is not None},
                    cpu.host.busy_cores(cores, released(cpu.host, None)), cores,
                    tuple(b / 2**30 for b in busy if b is not None), cpu.host.span_seconds(),
                    {cls: ClassCpu(cpus, cpu.of(cpus).busy_cores(len(cpus), released(cpu.of(cpus), cpus)))
                     for cls, cpus in sets.items()},
                    {c: v for c, v in limits.items() if v is not None})


def cpu_usage_seconds(cgroup_dir: str) -> float | None:
    """A cgroup's CPU time in all (cpu.stat usage_usec), in seconds; None when unreadable."""
    stat = _read(os.path.join(cgroup_dir, "cpu.stat")) or ""
    usec = next((int(l.split()[1]) for l in stat.splitlines() if l.startswith("usage_usec ")), None)
    return None if usec is None else usec / 1e6


class RunnerUsage:
    """Each runner's container, read off its own cgroup every tick: its working set now, its CPU time in all, and the
    cores it used over the last `window_seconds` (from the samples a tick apart; None from one sample). A container
    gone since the last tick keeps its samples while they reach into the window: its CPU there is still in the CPU
    readings, which release it (read_readings, ended_seconds)."""

    def __init__(self, window_seconds: int):
        self.window = window_seconds
        self.samples: dict = {}  # cgroup directory -> [(time, cpu seconds)]
        self.ended: dict = {}  # an ended container's cgroup directory -> its samples, up to its last tick

    def read(self, cgroup_dir: str, now: float) -> tuple:
        """(mem_gib, cpu_cores, cpu_seconds) of the cgroup, each None when unreadable."""
        used = working_set_bytes(cgroup_dir)
        seconds = cpu_usage_seconds(cgroup_dir)
        mem = None if used is None else used / 2**30
        if seconds is None:
            return mem, None, None
        kept = [x for x in self.samples.get(cgroup_dir, []) if x[0] >= now - self.window] + [(now, seconds)]
        self.samples[cgroup_dir] = kept
        (t0, s0), (t1, s1) = kept[0], kept[-1]
        return mem, ((s1 - s0) / (t1 - t0) if t1 > t0 else None), seconds

    def keep_only(self, cgroup_dirs: set, now: float) -> None:
        """Forget the containers not in `cgroup_dirs`, but for the CPU their samples show inside the window."""
        self.ended.update({d: v for d, v in self.samples.items() if d not in cgroup_dirs})
        self.ended = {d: v for d, v in self.ended.items() if d not in cgroup_dirs and v[-1][0] > now - self.window}
        self.samples = {d: v for d, v in self.samples.items() if d in cgroup_dirs}

    def ended_seconds(self, cfg: Config, since: float) -> dict:
        """The CPU seconds each class's slice's ended containers used after `since`, as far as their samples tell (the
        last tick to their end is not seen): class -> seconds."""
        out = {}
        for d, samples in self.ended.items():
            cls = next((c for c in CLASSES if os.path.dirname(d) == cfg.class_cgroup.format(cls=c)), None)
            inside = [s for t, s in samples if t >= since]
            if cls is not None and len(inside) >= 2:
                out[cls] = out.get(cls, 0.0) + inside[-1] - inside[0]
        return out


def _run(cmd: list, timeout: int = 30) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


class Host:
    """Our runner instances: <state>/<slot>.env says which class a slot was started for; the unit says if it lives."""

    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.idle_since: dict = {}
        self.usage = RunnerUsage(cfg.cpu_window_seconds)

    def unit(self, slot: int) -> str:
        return f"{self.cfg.unit}@{slot}.service"

    def instances(self, now: float) -> list:
        out, seen = [], set()
        for env in sorted(glob.glob(os.path.join(self.cfg.state_dir, "*.env"))):
            slot = int(os.path.basename(env)[:-4])
            state = _run(["systemctl", "is-active", self.unit(slot)]).stdout.strip()
            if state not in ("active", "activating", "reloading"):
                os.remove(env)  # its one job is done (or it failed to start): the slot is free
                self.idle_since.pop(slot, None)
                continue
            written = dict(l.split("=", 1) for l in (_read(env) or "").splitlines() if "=" in l)
            cls = written.get("RUNNER_CLASS", "light")
            # the slice it was started in (a borrowed check's is the measure slice); written as ci-movies-<class>.slice
            slice_class = written.get("RUNNER_SLICE", "")[len("ci-movies-"):-len(".slice")] or cls
            slice_class = "" if slice_class == cls else slice_class
            # Still starting (ExecStartPre mints its registration token; no container to probe yet): no job yet, so it
            # waits for one -- read busy, the next tick started a second runner for the same queued job.
            busy = state != "activating" and self.runs_job(slot)
            if busy:
                self.idle_since.pop(slot, None)
            started = os.path.getmtime(env)
            since = self.idle_since.setdefault(slot, started) if not busy else now
            cgroup = self.container_cgroup(slot, slice_class or cls) if state != "activating" else None
            usage = self.usage.read(cgroup, now) if cgroup else (None, None, None)
            seen.add(cgroup)
            # the first label it registered: a measure lane's for a measurement (a light runner's marker comes after)
            registered = written.get("RUNNER_LABELS", "").split(",")
            label = registered[0]
            out.append(Instance(slot, cls, busy, since, started, *usage, slice_class=slice_class,
                                label="" if label == LABELS.get(cls) else label,
                                pin=next((l for l in registered if l.startswith(PIN_PREFIX)), "")))
        self.usage.keep_only(seen, now)
        return out

    def container_cgroup(self, slot: int, slice_class: str) -> str | None:
        """The slot's container's own cgroup (docker's systemd driver: <the slice it runs in>/docker-<id>.scope); None
        while it has no container."""
        out = _run(["docker", "inspect", "-f", "{{.Id}}", f"{self.cfg.unit}-{slot}"])
        cid = out.stdout.strip()
        if out.returncode != 0 or not cid:
            return None
        return os.path.join(self.cfg.class_cgroup.format(cls=slice_class), f"docker-{cid}.scope")

    def start(self, slot: int, cls: str, slice_class: str = "", label: str = "", pin: str = "") -> None:
        """Start the slot for one job of `cls` (`label`, one of its lanes'; empty: its class's; `pin`, a run's pin label
        it registers beside it), in `slice_class`'s slice (its own when empty). A light runner also says it serves the
        measure lanes (LANES_MARKER)."""
        labels = (label or LABELS[cls]) + (f",{pin}" if pin else "")
        if cls == "light":
            labels += f",{LANES_MARKER},{EXPLORE_MARKER},{STAGE_MARKER},{CONSUMER_MARKER}"
        os.makedirs(self.cfg.state_dir, mode=0o700, exist_ok=True)
        with open(os.path.join(self.cfg.state_dir, f"{slot}.env"), "w") as f:
            f.write(f"RUNNER_CLASS={cls}\nRUNNER_LABELS={labels}\n"
                    f"RUNNER_SLICE=ci-movies-{slice_class or cls}.slice\n")
        _run(["systemctl", "start", "--no-block", self.unit(slot)])

    def runs_job(self, slot: int) -> bool:
        """A Runner.Worker in the slot's container -- and True when the probe cannot tell: a runner is never stopped on
        doubt. `docker top` keeps the container's rows by ps's PID column, so the options must name `pid` (`-eo comm`
        alone always failed, every runner read idle, and #1005's eight convergence rows were stopped mid-job)."""
        top = _run(["docker", "top", f"{self.cfg.unit}-{slot}", "-eo", "pid,comm"])
        if top.returncode != 0:
            print(f"slot {slot}: docker top failed ({top.returncode}): {top.stderr.strip()} -- read as busy",
                  file=sys.stderr, flush=True)
            return True
        return any(line.split()[-1:] == ["Runner.Worker"] for line in top.stdout.splitlines())

    def stop(self, slot: int) -> None:
        # Never one that took a job since the tick looked: check again right before.
        if self.runs_job(slot):
            return
        _run(["systemctl", "stop", "--no-block", self.unit(slot)])


class GitHub:
    """GitHub's REST API with an installation token of a GitHub App (the token helper bitcashier's infra ships), and
    ETags: an unchanged list answers 304, which costs no rate limit."""

    API = "https://api.github.com"

    def __init__(self, cfg: Config, app_id: str, key: str):
        self.cfg, self.app_id, self.key = cfg, app_id, key
        self.token, self.minted = "", 0.0
        self.etags: dict = {}
        self.remaining = 5000

    def _token(self) -> str:
        if not self.token or time.time() - self.minted > 45 * 60:
            out = _run(["bash", "-c", f'. "{self.cfg.token_helper}" && github_app_token "$0" "$1" "$2"',
                        self.app_id, self.key, self.cfg.repo], timeout=60)
            if out.returncode != 0 or not out.stdout.strip():
                raise RuntimeError(f"no installation token for App {self.app_id}: {out.stderr.strip()[-200:]}")
            self.token, self.minted = out.stdout.strip(), time.time()
        return self.token

    def request(self, method: str, url: str, body: bytes | None = None, content_type: str = "application/json"):
        if not url.startswith("https://"):
            url = self.API + url
        req = urllib.request.Request(url, data=body, method=method, headers={
            "Authorization": f"Bearer {self._token()}", "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28", "Content-Type": content_type})
        cached = self.etags.get(url) if method == "GET" else None
        if cached:
            req.add_header("If-None-Match", cached[0])
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                self.remaining = int(resp.headers.get("X-RateLimit-Remaining", self.remaining))
                data = resp.read()
                value = json.loads(data) if data else None
                if method == "GET" and resp.headers.get("ETag"):
                    self.etags[url] = (resp.headers["ETag"], value)
                return value
        except urllib.error.HTTPError as e:
            if e.code == 304 and cached:
                return cached[1]
            if e.code == 404 and method in ("GET", "DELETE"):
                return None
            raise

    def queued_jobs(self) -> list:
        """Every queued job of the repository's queued and in-progress runs (a matrix row waits inside a running run)."""
        jobs = []
        for status in ("queued", "in_progress"):
            runs = self.request("GET", f"/repos/{self.cfg.repo}/actions/runs?status={status}&per_page=100") or {}
            for run in runs.get("workflow_runs", []):
                page = self.request("GET", f"/repos/{self.cfg.repo}/actions/runs/{run['id']}/jobs?filter=latest&per_page=100")
                jobs += [j for j in (page or {}).get("jobs", []) if j.get("status") == "queued"]
        return jobs



# ── the loop ────────────────────────────────────────────────────────────────────────────────────────────────────────
def tick(cfg: Config, host: Host, reader: GitHub, cpu: CpuMeters, dry_run: bool = False) -> tuple[Decision, int]:
    now = time.time()
    bc = Bitcashier(busy_runners(cfg.builder_cgroups), busy_runners(cfg.deployer_cgroups))
    jobs = reader.queued_jobs()
    demand, lanes, queue = demand_of(jobs), lane_demand_of(jobs), queue_of(jobs)
    instances = host.instances(now)  # first: a container ended since the last tick releases its CPU in the readings
    r = read_readings(cfg, cpu, now, host.usage)
    d = decide(cfg, bc, demand, instances, now, r, queue=queue)
    gib = lambda v: "-" if v is None else f"{v:.0f}"  # noqa: E731
    cores = lambda v: "-" if v is None else f"{v:.1f}"  # noqa: E731
    ours = [(i.slot, i.cls + (f"@{i.runs_in}" if i.borrowed else "") + (f"/{i.label}" if i.label else "")
             + (f"/{i.pin[len(PIN_PREFIX):]}" if i.pin else ""),
             "busy" if i.busy else "idle", gib(i.mem_gib),
             cores(i.cpu_cores)) for i in instances]
    print(f"bitcashier busy {bc.busy_builders}/{cfg.builders} builders {bc.busy_deployers}/{cfg.deployers} deployers"
          f"{' SATURATED' if d.saturated else ''}; room {d.room}; reserve {reserved_builders(cfg, bc)} builders "
          f"x {builder_reserve_gib(cfg, r):.1f} GiB; held by {d.limits}; "
          f"free GiB host {gib(r.mem_available_gib)} "
          f"ci {gib(r.ci_free_gib)} {({c: round(v) for c, v in r.class_free_gib.items()})}; cpu busy "
          f"{gib(r.cpu_busy_cores)}/{r.cpu_cores} "
          f"{({c: f'{gib(v.busy_cores)}/{len(v.cpus)}' for c, v in r.class_cpu.items()})}; queued {demand} "
          f"lanes {lanes}; ours {ours}; "
          f"start {list(d.starts)} lanes {list(d.labels)} pins {[(s, p[len(PIN_PREFIX):]) for s, p in d.pins if p]} borrowed {list(d.borrowed)} warm {list(d.warm)} "
          f"stop {list(d.stops)}", flush=True)
    for line in starved(cfg, d, demand, oldest_waits(jobs, now)):
        print(line, flush=True)
    if dry_run:
        return d, pace_seconds(cfg, queue, reader.remaining)
    for slot in d.stops:
        host.stop(slot)
    labels, pins = dict(d.labels), dict(d.pins)
    for slot, cls in d.starts:
        host.start(slot, cls, "measure" if slot in d.borrowed else "", labels.get(slot, ""), pins.get(slot, ""))
    return d, pace_seconds(cfg, queue, reader.remaining)


def cmd_run(cfg: Config, dry_run: bool, once: bool) -> int:
    host = Host(cfg)
    reader = GitHub(cfg, cfg.reader_app_id, cfg.reader_key)
    cpu = CpuMeters(cfg.cpu_window_seconds)
    while True:
        started = time.time()
        pace = cfg.tick_seconds if reader.remaining > 1500 else cfg.tick_seconds * 4
        try:
            _d, pace = tick(cfg, host, reader, cpu, dry_run)
        except Exception as e:  # noqa: BLE001 -- one bad tick (GitHub down) must not end the loop
            print(f"tick failed: {e}", file=sys.stderr, flush=True)
        if once:
            return 0
        time.sleep(max(1.0, pace - (time.time() - started)))


def main(argv: list) -> int:
    flags = {a for a in argv[1:] if a == "--dry-run"}
    rest = [a for a in argv[1:] if a not in flags]
    args = dict(zip(rest[0::2], rest[1::2]))
    if argv[:1] in (["run"], ["tick"]) and "--config" in args:
        return cmd_run(Config.load(args["--config"]), "--dry-run" in flags, argv[0] == "tick")
    print(__doc__, file=sys.stderr)
    return 64


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
