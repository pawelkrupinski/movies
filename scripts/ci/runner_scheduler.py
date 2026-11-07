#!/usr/bin/env python3
"""CI-RUNNER'S MOVIES RUNNER SCHEDULER (the owner, 2026-11-06: "make movies runs take all the ci-runner runners when
bitcashier is not using them"). ci-runner is bitcashier's CI host; movies borrows what bitcashier leaves idle.

WHAT IT DECIDES, every tick (30 s): how many movies runners may exist now, and which job class each new one serves.

  * BITCASHIER ALWAYS WINS. Its 12 builders and 6 deployers are persistent runners on the host (default.nix
    `ghRunners`); a bitcashier job waits only when every runner of its kind is busy. So while every builder or every
    deployer is busy (bitcashier has, or may have, queued work beyond its free runners) NO movies runner is started, and
    idle movies runners are stopped. Movies jobs already running finish, under ci-movies.slice's lower CPU weight.
  * THE ELASTIC CAP is memory: a movies job holds ~10 GiB (measured 7-13 GiB), so the ceiling is what ci.slice's
    MemoryMax leaves after bitcashier's reserve -- a floor for an idle bitcashier plus a share per busy bitcashier job
    (its builders peaked at 93 GiB together) -- in whole jobs, and never past `max_slots`. With bitcashier idle that is
    ~10 movies jobs, falling as bitcashier's jobs start.
  * EACH CLASS'S SLICE has its own MemoryMax (ci-movies-measure.slice: 48 GiB), so a class never has more runners than
    its slice holds at a job's worst case (`job_max_gib`, the container's --memory): Verify scrape recording #14 ran nine
    measurements in 48 GiB and the slice's OOM killer took five (exit 137).
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
    job_gib: int = 10
    # a job's worst case: the container's --memory (movies-runner@.service), what a class's own slice must hold per runner
    job_max_gib: int = 16
    # each class's slice (RUNNER_SLICE), whose memory.max caps that class on its own ({cls} is the class)
    class_cgroup: str = "/sys/fs/cgroup/ci.slice/ci-movies.slice/ci-movies-{cls}.slice"
    ci_memory_gib: int = 115
    bitcashier_reserve_gib: int = 8
    per_busy_bitcashier_gib: int = 8
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


@dataclasses.dataclass(frozen=True)
class Decision:
    ceiling: int
    saturated: bool
    starts: tuple  # ((slot, cls), ...)
    stops: tuple  # (slot, ...)


# ── the decision, pure ──────────────────────────────────────────────────────────────────────────────────────────────
def saturated(cfg: Config, bc: Bitcashier) -> bool:
    """Every builder or every deployer busy: a bitcashier job may be waiting for a runner, so movies starts nothing."""
    return bc.busy_builders >= cfg.builders or bc.busy_deployers >= cfg.deployers


def ceiling(cfg: Config, bc: Bitcashier) -> int:
    """How many movies runners may exist now: ci.slice's memory less bitcashier's reserve, in whole movies jobs."""
    busy = bc.busy_builders + bc.busy_deployers
    left = cfg.ci_memory_gib - cfg.bitcashier_reserve_gib - cfg.per_busy_bitcashier_gib * busy
    return max(0, min(cfg.max_slots, left // cfg.job_gib))


def class_caps(cfg: Config) -> dict:
    """The most runners each class's slice holds: its memory.max in worst-case jobs. A slice without a limit ("max") or
    unreadable caps nothing -- the ceiling still holds every class together."""
    caps = {}
    for cls in CLASSES:
        try:
            limit = open(os.path.join(cfg.class_cgroup.format(cls=cls), "memory.max")).read().strip()
        except OSError:
            continue
        if limit.isdigit():
            caps[cls] = int(limit) // (cfg.job_max_gib * 2**30)
    return caps


def decide(cfg: Config, bc: Bitcashier, demand: dict, instances: list, now: float, caps: dict | None = None) -> Decision:
    """The runners to start ((slot, class), highest class first) and the idle ones to stop, for `demand` (queued jobs
    per class) and the `instances` that exist; `caps` the most runners a class's own slice holds (class_caps)."""
    top = ceiling(cfg, bc)
    full = saturated(cfg, bc)
    rank = {c: i for i, c in enumerate(CLASSES)}
    idle = [i for i in instances if not i.busy]

    stops = []
    # Over the ceiling (bitcashier started work) or bitcashier saturated: idle runners go, lowest class first.
    excess = len(instances) - (0 if full else top)
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
    room = 0 if full else max(0, top - len(alive))
    starts = []
    for cls in CLASSES:
        waiting = sum(1 for i in alive if i.cls == cls and not i.busy)
        need = max(0, demand.get(cls, 0) - waiting)
        if cls in (caps or {}):
            need = min(need, max(0, caps[cls] - sum(1 for i in alive if i.cls == cls)))
        for _ in range(min(need, room, len(free_slots))):
            starts.append((free_slots.pop(0), cls))
            room -= 1
    return Decision(top, full, tuple(starts), tuple(sorted(stops)))


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
            cls = dict(l.split("=", 1) for l in open(env).read().splitlines() if "=" in l).get("RUNNER_CLASS", "light")
            # Still starting (ExecStartPre mints its registration token; no container to probe yet): no job yet, so it
            # waits for one -- read busy, the next tick started a second runner for the same queued job.
            busy = state != "activating" and self.runs_job(slot)
            if busy:
                self.idle_since.pop(slot, None)
            since = self.idle_since.setdefault(slot, os.path.getmtime(env)) if not busy else now
            out.append(Instance(slot, cls, busy, since))
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
def tick(cfg: Config, host: Host, reader: GitHub, dry_run: bool = False) -> Decision:
    now = time.time()
    bc = Bitcashier(busy_runners(cfg.builder_cgroups), busy_runners(cfg.deployer_cgroups))
    demand = demand_of(reader.queued_jobs())
    instances = host.instances(now)
    caps = class_caps(cfg)
    d = decide(cfg, bc, demand, instances, now, caps)
    print(f"bitcashier busy {bc.busy_builders}/{cfg.builders} builders {bc.busy_deployers}/{cfg.deployers} deployers"
          f"{' SATURATED' if d.saturated else ''}; ceiling {d.ceiling}; class caps {caps}; queued {demand}; "
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
    while True:
        started = time.time()
        try:
            tick(cfg, host, reader, dry_run)
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
