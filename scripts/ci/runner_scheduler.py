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
        new job's class's `job_expected_gib` (a check or measurement 11 GiB, the container's --memory=16g bounding the
        worst one; a light job 1).
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
      - CPU: the host's busy cores over `cpu_window_seconds` (/proc/stat), up to `cpu_start_below` of the cores
        (`light_cpu_start_below` for a light job), must hold the class's `job_cpus` (the JVM's ActiveProcessorCount
        for a check or measurement; 1 for a light job). On a saturated host another job only slows every job,
        bitcashier's included, for no throughput.
    A runner of ours that has not shown its memory and CPU yet -- one waiting for its job, or started within
    `settle_seconds` -- counts at its class's expected size on every one of them, so a queue never starts a burst into
    memory that only looked free. A job starts only when all three hold it, never past its slots; when memory runs
    short of what our waiting runners would take (bitcashier started work), the idle ones go, lowest class first.
  * PRIORITY between movies' own jobs: landing checks (`movies-check`), then measurements (`movies-measure`), in the
    `max_slots` slots. A runner is started for a QUEUED job of a class only (ephemeral, one job, the label of that
    class alone), the highest class first, so a free slot goes to a waiting landing check before a measurement.
  * THE LIGHT LANE (the owner, 2026-11-07: "why was it waiting so long, can you fix that?"): the light jobs (`movies`:
    timings reports, prunes, dispatches -- seconds, under 1 GiB) have `light_slots` slots of their own past
    `max_slots`, admitted at their own size whatever the heavy queue or bitcashier's saturation. Charged a
    measurement's 11 GiB and 4 cores and ranked last, identity run 42370669779's 10-second timings job waited 26 min
    (12:32-12:58Z) behind 20 queued measurements.
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
CLASS_OF = {label: cls for cls, label in LABELS.items()}


@dataclasses.dataclass(frozen=True)
class Config:
    repo: str = "pawelkrupinski/movies"
    max_slots: int = 10
    # ADMISSION -- every number of the rule in the module docstring, here and nowhere else
    # Each class's expected working set and cores: a check or a measurement holds 10-11 GiB (2026-11-07: 31 GiB for 3,
    # peaks 13) on the JVM's 4 cores; a light job (a timings report, a prune, a dispatch) runs seconds in under 1 GiB.
    job_expected_gib: dict = dataclasses.field(default_factory=lambda: {"check": 11, "measure": 11, "light": 1})
    job_cpus: dict = dataclasses.field(default_factory=lambda: {"check": 4, "measure": 4, "light": 1})
    light_slots: int = 3  # the light lane: slots past max_slots, for light jobs only
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
    tick_seconds: int = 30
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


@dataclasses.dataclass(frozen=True)
class Readings:
    """The host as it is now, in GiB and cores. A class without a slice limit has no class_free_gib entry; no CPU rate
    yet (one sample) is cpu_busy_cores None."""
    mem_available_gib: float
    ci_free_gib: float | None
    class_free_gib: dict
    cpu_busy_cores: float | None
    cpu_cores: int
    busy_builder_gib: tuple = ()  # the working set of each builder unit running a job


@dataclasses.dataclass(frozen=True)
class Decision:
    room: int  # how many heavy (check, measure) runners memory, CPU and slots held this tick; 0 when saturated
    saturated: bool
    starts: tuple  # ((slot, cls), ...)
    stops: tuple  # (slot, ...)


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


def cpu_spare(cfg: Config, r: Readings, pending_cpus: float, start_below: float) -> float:
    """The cores free for more jobs: busy cores up to `start_below` of them, less `pending_cpus` of our runners that have
    not shown their CPU yet. No rate yet (one sample): none."""
    if r.cpu_busy_cores is None:
        return 0.0
    return start_below * r.cpu_cores - r.cpu_busy_cores - pending_cpus


def memory_room(cfg: Config, bc: Bitcashier, r: Readings, pending_gib: float, cls: str = "measure") -> int:
    """More `cls` jobs the host's free memory holds now (negative: short of our waiting runners)."""
    return _whole_jobs(memory_free_gib(cfg, bc, r, pending_gib), cfg.job_expected_gib[cls])


def cpu_room(cfg: Config, r: Readings, pending_cpus: float, cls: str = "measure") -> int:
    """More `cls` jobs the CPU has room for, up to cpu_start_below of the cores."""
    return _whole_jobs(cpu_spare(cfg, r, pending_cpus, cfg.cpu_start_below), cfg.job_cpus[cls])


def decide(cfg: Config, bc: Bitcashier, demand: dict, instances: list, now: float, r: Readings) -> Decision:
    """The runners to start ((slot, class), highest class first, then the light lane) and the idle ones to stop, for
    `demand` (queued jobs per class), the `instances` that exist and the host's live readings `r`."""
    full = saturated(cfg, bc)
    rank = {c: i for i, c in enumerate(CLASSES)}
    idle = [i for i in instances if not i.busy]

    def unsettled(insts):
        # not yet in the readings: waiting for its job, or started too recently for its memory to show
        return [i for i in insts if not i.busy or now - i.started < cfg.settle_seconds]

    def gib(insts):
        return sum(cfg.job_expected_gib[i.cls] for i in insts)

    def cpus(insts):
        return sum(cfg.job_cpus[i.cls] for i in insts)

    def heavy(insts):  # the runners in the max_slots slots (the light lane's are past them)
        return [i for i in insts if i.slot <= cfg.max_slots]

    stops = []
    # Bitcashier saturated: idle heavy runners go (a light lane runner is too small to hold bitcashier back).
    if full:
        stops += [i.slot for i in idle if i.cls != "light"]
    # Past max_slots, or memory short of what our waiting runners would take: idle runners go, lowest class first.
    excess = max(0, len(heavy(instances)) - cfg.max_slots)
    short = -memory_free_gib(cfg, bc, r, gib(unsettled(instances)))
    for inst in sorted(idle, key=lambda i: (-rank[i.cls], i.slot)):
        if inst.slot in stops:
            continue
        if excess <= 0 and short <= 0:
            break
        stops.append(inst.slot)
        excess -= 1
        short -= cfg.job_expected_gib[inst.cls]
    # An idle runner whose class has nothing queued for longer than idle_stop_seconds gives its slot back.
    for inst in idle:
        if inst.slot not in stops and demand.get(inst.cls, 0) == 0 and now - inst.idle_since >= cfg.idle_stop_seconds:
            stops.append(inst.slot)

    alive = [i for i in instances if i.slot not in stops]
    used = {i.slot for i in instances}  # a stopping slot is not reused in the same tick
    pending = unsettled(alive)
    memory = memory_free_gib(cfg, bc, r, gib(pending))
    cpu = {"heavy": cpu_spare(cfg, r, cpus(pending), cfg.cpu_start_below),
           "light": cpu_spare(cfg, r, cpus(pending), cfg.light_cpu_start_below)}
    starts = []

    def admit(cls: str, slots: list, gate: str) -> None:
        nonlocal memory
        mine = [i for i in alive if i.cls == cls]
        need = max(0, demand.get(cls, 0) - sum(1 for i in mine if not i.busy))
        held = class_free_gib(cfg, r, cls, gib(unsettled(mine)))
        size, cores = cfg.job_expected_gib[cls], cfg.job_cpus[cls]
        while need and slots and memory >= size and cpu[gate] >= cores and (held is None or held >= size):
            starts.append((slots.pop(0), cls))
            need -= 1
            memory -= size
            held = None if held is None else held - size
            for g in cpu:
                cpu[g] -= cores

    heavy_slots = [s for s in range(1, cfg.max_slots + 1) if s not in used]
    if not full:
        for cls in CLASSES[:-1]:
            admit(cls, heavy_slots, "heavy")
    # room: the heavy jobs memory, CPU and slots held this tick (those just started included)
    room = 0 if full else len(starts) + min(_whole_jobs(memory, cfg.job_expected_gib["measure"]),
                                            _whole_jobs(cpu["heavy"], cfg.job_cpus["measure"]), len(heavy_slots))
    # THE LIGHT LANE: its own slots, admitted at a light job's size whatever the heavy queue or bitcashier's runners.
    admit("light", [s for s in range(cfg.max_slots + 1, cfg.max_slots + cfg.light_slots + 1) if s not in used], "light")
    return Decision(room, full, tuple(starts), tuple(sorted(stops)))


def demand_of(jobs: list) -> dict:
    """Queued jobs per class, from jobs-API job dicts: a job is a class's when its labels name that class's label."""
    out = {c: 0 for c in CLASSES}
    for job in jobs:
        if job.get("status") != "queued":
            continue
        classes = [CLASS_OF[l] for l in job.get("labels", []) if l in CLASS_OF]
        if classes:
            out[min(classes, key=CLASSES.index)] += 1
    return out


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


def headroom_gib(cgroup_dir: str) -> float | None:
    """memory.max less the working set; None for no limit ("max") or an unreadable cgroup."""
    limit = (_read(os.path.join(cgroup_dir, "memory.max")) or "").strip()
    used = working_set_bytes(cgroup_dir)
    if not limit.isdigit() or used is None:
        return None
    return (int(limit) - used) / 2**30


def cpu_times(stat: str) -> tuple:
    """(busy, total) jiffies off /proc/stat's `cpu` line: busy is all but idle and iowait (guest is inside user)."""
    v = [int(x) for x in stat.splitlines()[0].split()[1:]]
    total = sum(v[:8])
    return total - v[3] - v[4], total


class CpuMeter:
    """The host's busy cores over the last `window_seconds`, from /proc/stat samples a tick apart."""

    def __init__(self, window_seconds: int):
        self.window = window_seconds
        self.samples: list = []

    def add(self, now: float, busy: int, total: int) -> None:
        self.samples = [x for x in self.samples if x[0] >= now - self.window] + [(now, busy, total)]

    def busy_cores(self, cores: int) -> float | None:
        if len(self.samples) < 2 or self.samples[-1][2] == self.samples[0][2]:
            return None
        (_, b0, t0), (_, b1, t1) = self.samples[0], self.samples[-1]
        return cores * (b1 - b0) / (t1 - t0)


def read_readings(cfg: Config, cpu: CpuMeter, now: float) -> Readings:
    meminfo = _read(os.path.join(cfg.proc, "meminfo")) or ""
    available_kb = next((int(l.split()[1]) for l in meminfo.splitlines() if l.startswith("MemAvailable:")), 0)
    stat = _read(os.path.join(cfg.proc, "stat"))
    if stat:
        cpu.add(now, *cpu_times(stat))
    cores = sum(1 for l in (stat or "").splitlines() if l.startswith("cpu") and l[3:4].isdigit()) or os.cpu_count() or 1
    classes = {cls: headroom_gib(cfg.class_cgroup.format(cls=cls)) for cls in CLASSES}
    busy = [working_set_bytes(d) for d in busy_units(cfg.builder_cgroups, cfg.proc)]
    return Readings(available_kb / 2**20, headroom_gib(cfg.ci_cgroup),
                    {c: v for c, v in classes.items() if v is not None}, cpu.busy_cores(cores), cores,
                    tuple(b / 2**30 for b in busy if b is not None))


def _run(cmd: list, timeout: int = 30) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)


class Host:
    """Our runner instances: <state>/<slot>.env says which class a slot was started for; the unit says if it lives."""

    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.idle_since: dict = {}

    def unit(self, slot: int) -> str:
        return f"{self.cfg.unit}@{slot}.service"

    def instances(self, now: float) -> list:
        out = []
        for env in sorted(glob.glob(os.path.join(self.cfg.state_dir, "*.env"))):
            slot = int(os.path.basename(env)[:-4])
            state = _run(["systemctl", "is-active", self.unit(slot)]).stdout.strip()
            if state not in ("active", "activating", "reloading"):
                os.remove(env)  # its one job is done (or it failed to start): the slot is free
                self.idle_since.pop(slot, None)
                continue
            cls = dict(l.split("=", 1) for l in (_read(env) or "").splitlines() if "=" in l).get("RUNNER_CLASS", "light")
            # Still starting (ExecStartPre mints its registration token; no container to probe yet): no job yet, so it
            # waits for one -- read busy, the next tick started a second runner for the same queued job.
            busy = state != "activating" and self.runs_job(slot)
            if busy:
                self.idle_since.pop(slot, None)
            started = os.path.getmtime(env)
            since = self.idle_since.setdefault(slot, started) if not busy else now
            out.append(Instance(slot, cls, busy, since, started))
        return out

    def start(self, slot: int, cls: str) -> None:
        os.makedirs(self.cfg.state_dir, mode=0o700, exist_ok=True)
        with open(os.path.join(self.cfg.state_dir, f"{slot}.env"), "w") as f:
            f.write(f"RUNNER_CLASS={cls}\nRUNNER_LABELS={LABELS[cls]}\nRUNNER_SLICE=ci-movies-{cls}.slice\n")
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
def tick(cfg: Config, host: Host, reader: GitHub, cpu: CpuMeter, dry_run: bool = False) -> Decision:
    now = time.time()
    bc = Bitcashier(busy_runners(cfg.builder_cgroups), busy_runners(cfg.deployer_cgroups))
    demand = demand_of(reader.queued_jobs())
    instances = host.instances(now)
    r = read_readings(cfg, cpu, now)
    d = decide(cfg, bc, demand, instances, now, r)
    gib = lambda v: "-" if v is None else f"{v:.0f}"  # noqa: E731
    print(f"bitcashier busy {bc.busy_builders}/{cfg.builders} builders {bc.busy_deployers}/{cfg.deployers} deployers"
          f"{' SATURATED' if d.saturated else ''}; room {d.room}; reserve {reserved_builders(cfg, bc)} builders "
          f"x {builder_reserve_gib(cfg, r):.1f} GiB; "
          f"free GiB host {gib(r.mem_available_gib)} "
          f"ci {gib(r.ci_free_gib)} {({c: round(v) for c, v in r.class_free_gib.items()})}; cpu busy "
          f"{gib(r.cpu_busy_cores)}/{r.cpu_cores}; queued {demand}; "
          f"ours {[(i.slot, i.cls, 'busy' if i.busy else 'idle') for i in instances]}; "
          f"start {list(d.starts)} stop {list(d.stops)}", flush=True)
    if dry_run:
        return d
    for slot in d.stops:
        host.stop(slot)
    for slot, cls in d.starts:
        host.start(slot, cls)
    return d


def cmd_run(cfg: Config, dry_run: bool, once: bool) -> int:
    host = Host(cfg)
    reader = GitHub(cfg, cfg.reader_app_id, cfg.reader_key)
    cpu = CpuMeter(cfg.cpu_window_seconds)
    while True:
        started = time.time()
        try:
            tick(cfg, host, reader, cpu, dry_run)
        except Exception as e:  # noqa: BLE001 -- one bad tick (GitHub down) must not end the loop
            print(f"tick failed: {e}", file=sys.stderr, flush=True)
        if once:
            return 0
        # a reader budget running low slows the loop rather than starving the App's other users
        pace = cfg.tick_seconds if reader.remaining > 1500 else cfg.tick_seconds * 4
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
