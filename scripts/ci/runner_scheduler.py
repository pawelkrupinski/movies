#!/usr/bin/env python3
"""CI-RUNNER'S MOVIES RUNNER SCHEDULER (the owner, 2026-11-06: "make movies runs take all the ci-runner runners when
bitcashier is not using them"). ci-runner is bitcashier's CI host; movies borrows what bitcashier leaves idle.

WHAT IT DECIDES, every tick (30 s): how many movies runners may exist now, and which job class each new one serves.

  * BITCASHIER ALWAYS WINS. Its 12 builders and 6 deployers are persistent runners on the host (default.nix
    `ghRunners`); a bitcashier job waits only when every runner of its kind is busy. So while every builder or every
    deployer is busy (bitcashier has, or may have, queued work beyond its free runners) NO movies runner is started, and
    idle movies runners are stopped. Movies jobs already running finish, under ci-movies.slice's lower CPU weight.
  * ADMISSION IS WHAT IS FREE NOW, memory AND CPU (the owner, 2026-11-07: "fix the schedule so runners don't stay
    idle" ... "don't let cpu be underutilized"). The fixed worst-case budgets this replaced (8 GiB per busy bitcashier
    job, 16 GiB per movies job in its slice) held movies at 3 runners with 58 GiB free. Every tick reads, live:
      - MEMORY: the tighter of the host's MemAvailable and ci.slice's headroom (memory.max less its working set:
        memory.current without inactive page cache, which the kernel takes back first), less a reserve for every
        bitcashier runner that is idle and may start (an idle builder: what a busy one holds now, at least
        `per_idle_builder_gib`; an idle deployer `per_idle_deployer_gib`), less `memory_margin_gib`, in whole jobs of
        `job_expected_gib` (a measurement holds 10-11 GiB; the container's --memory=16g bounds the worst one).
      - THE CLASS'S SLICE likewise: its memory.max less its working set must hold the margin and the new job. Verify
        scrape recording #14 ran nine measurements in a 48 GiB slice, and on 2026-11-07 07:40-08:22Z six-then-nine did:
        its OOM killer took 18 processes (exit 137).
      - CPU: the host's busy cores over `cpu_window_seconds` (/proc/stat), up to `cpu_start_below` of the cores, in
        jobs of `job_cpus` (the JVM's ActiveProcessorCount). On a saturated host another job only slows every job,
        bitcashier's included, for no throughput.
    A runner of ours that has not shown its memory and CPU yet -- one waiting for its job, or started within
    `settle_seconds` -- counts at the expected size on every one of them, so a queue never starts a burst into memory
    that only looked free. The start rule is the smallest of the three rooms, never past `max_slots`; when memory runs
    short of what our waiting runners would take (bitcashier started work), the idle ones go.
  * PRIORITY between movies' own jobs: landing checks (`movies-check`), then measurements (`movies-measure`), then the
    light jobs around them (`movies`). A runner is started for a QUEUED job of a class only (ephemeral, one job, the
    label of that class alone), the highest class first, so a free slot goes to a waiting landing check before a
    measurement.
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
    job_expected_gib: float = 11  # a measurement's working set, 2026-11-07: 31 GiB for 3, peaks 13
    memory_margin_gib: float = 6  # left free beyond every reserve, for the running jobs' growth
    # an idle builder's reserve: what a busy one holds now (2026-11-07: 34 GiB of working set for 7), at least this
    per_idle_builder_gib: float = 4
    per_idle_deployer_gib: float = 1  # a deploy job holds little (ci-deploy.slice: 0.3 GiB)
    settle_seconds: int = 300  # a runner this young counts at its expected size: its memory has not shown yet
    cpu_start_below: float = 0.85  # of the host's cores, busy over the window: above it, nothing starts
    cpu_window_seconds: int = 300
    job_cpus: float = 4  # movies-runner@.service: JAVA_TOOL_OPTIONS=-XX:ActiveProcessorCount=4
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
    builders_working_set_gib: float


@dataclasses.dataclass(frozen=True)
class Decision:
    room: int  # how many more runners memory and CPU hold now (negative: memory short of our waiting runners)
    saturated: bool
    starts: tuple  # ((slot, cls), ...)
    stops: tuple  # (slot, ...)


# ── the decision, pure ──────────────────────────────────────────────────────────────────────────────────────────────
def saturated(cfg: Config, bc: Bitcashier) -> bool:
    """Every builder or every deployer busy: a bitcashier job may be waiting for a runner, so movies starts nothing."""
    return bc.busy_builders >= cfg.builders or bc.busy_deployers >= cfg.deployers


def _whole_jobs(free: float, size: float) -> int:
    return int(free // size)


def memory_room(cfg: Config, bc: Bitcashier, r: Readings, pending: int) -> int:
    """More movies jobs the host's free memory holds now: the tighter of MemAvailable and ci.slice's headroom, less a
    reserve per idle bitcashier runner, the margin, and `pending` runners of ours at their expected size."""
    free = r.mem_available_gib if r.ci_free_gib is None else min(r.mem_available_gib, r.ci_free_gib)
    per_builder = max(cfg.per_idle_builder_gib, r.builders_working_set_gib / bc.busy_builders if bc.busy_builders else 0)
    reserve = (per_builder * max(0, cfg.builders - bc.busy_builders)
               + cfg.per_idle_deployer_gib * max(0, cfg.deployers - bc.busy_deployers))
    return _whole_jobs(free - reserve - cfg.memory_margin_gib - pending * cfg.job_expected_gib, cfg.job_expected_gib)


def class_room(cfg: Config, r: Readings, cls: str, pending: int) -> int | None:
    """More jobs `cls`'s own slice holds now; None when the slice has no limit."""
    free = r.class_free_gib.get(cls)
    if free is None:
        return None
    return _whole_jobs(free - cfg.memory_margin_gib - pending * cfg.job_expected_gib, cfg.job_expected_gib)


def cpu_room(cfg: Config, r: Readings, pending: int) -> int:
    """More jobs the CPU has room for: busy cores up to cpu_start_below of them, `pending` runners at job_cpus each."""
    if r.cpu_busy_cores is None:
        return 0
    spare = cfg.cpu_start_below * r.cpu_cores - r.cpu_busy_cores - pending * cfg.job_cpus
    return _whole_jobs(spare, cfg.job_cpus)


def decide(cfg: Config, bc: Bitcashier, demand: dict, instances: list, now: float, r: Readings) -> Decision:
    """The runners to start ((slot, class), highest class first) and the idle ones to stop, for `demand` (queued jobs
    per class), the `instances` that exist and the host's live readings `r`."""
    full = saturated(cfg, bc)
    rank = {c: i for i, c in enumerate(CLASSES)}
    idle = [i for i in instances if not i.busy]

    def unsettled(insts):
        # not yet in the readings: waiting for its job, or started too recently for its memory to show
        return sum(1 for i in insts if not i.busy or now - i.started < cfg.settle_seconds)

    memory = memory_room(cfg, bc, r, unsettled(instances))
    stops = []
    # Bitcashier saturated, or memory short of what our waiting runners would take: idle runners go, lowest class first.
    excess = len(idle) if full else max(len(instances) - cfg.max_slots, -memory, 0)
    for inst in sorted(idle, key=lambda i: (-rank[i.cls], i.slot)):
        if excess <= 0:
            break
        stops.append(inst.slot)
        excess -= 1
    # An idle runner whose class has nothing queued for longer than idle_stop_seconds gives its slot back.
    for inst in idle:
        if inst.slot not in stops and demand.get(inst.cls, 0) == 0 and now - inst.idle_since >= cfg.idle_stop_seconds:
            stops.append(inst.slot)

    alive = [i for i in instances if i.slot not in stops]
    used = {i.slot for i in instances}  # a stopping slot is not reused in the same tick
    free_slots = [s for s in range(1, cfg.max_slots + 1) if s not in used]
    pending = unsettled(alive)
    room = min(memory_room(cfg, bc, r, pending), cpu_room(cfg, r, pending), cfg.max_slots - len(alive))
    starts = []
    for cls in CLASSES:
        mine = [i for i in alive if i.cls == cls]
        need = max(0, demand.get(cls, 0) - sum(1 for i in mine if not i.busy))
        held = class_room(cfg, r, cls, unsettled(mine))
        if held is not None:
            need = min(need, max(0, held))
        for _ in range(0 if full else max(0, min(need, room, len(free_slots)))):
            starts.append((free_slots.pop(0), cls))
            room -= 1
    return Decision(room + len(starts), full, tuple(starts), tuple(sorted(stops)))


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
def _comms(cgroup_dir: str) -> set:
    names = set()
    for procs in glob.glob(os.path.join(cgroup_dir, "**", "cgroup.procs"), recursive=True):
        try:
            pids = open(procs).read().split()
        except OSError:
            continue
        for pid in pids:
            try:
                names.add(open(f"/proc/{pid}/comm").read().strip())
            except OSError:
                pass
    return names


def busy_runners(pattern: str) -> int:
    """How many runner units matching `pattern` (cgroup directories) run a job: a Runner.Worker among their processes."""
    return sum(1 for d in glob.glob(pattern) if "Runner.Worker" in _comms(d))


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
    builders = sum(working_set_bytes(d) or 0 for d in glob.glob(cfg.builder_cgroups))
    return Readings(available_kb / 2**20, headroom_gib(cfg.ci_cgroup),
                    {c: v for c, v in classes.items() if v is not None}, cpu.busy_cores(cores), cores,
                    builders / 2**30)


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
          f"{' SATURATED' if d.saturated else ''}; room {d.room}; free GiB host {gib(r.mem_available_gib)} "
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
