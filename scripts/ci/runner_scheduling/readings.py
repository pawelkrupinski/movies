"""The host's live readings (read_readings): its memory and ci.slice's, each class's slice's headroom and limit, bitcashier's
busy builders' working sets, and the busy cores of the host and of each class's cpuset over the window -- every read
through the Machine it is given, every reading's arithmetic here (runner_scheduler.py's docstring, "ADMISSION IS WHAT IS
FREE NOW")."""
from __future__ import annotations

import os
from typing import Callable, Dict, List, Optional, Set, Tuple

from .machine import Machine
from .model import CLASSES, ClassCpu, Config, Readings

GIB = 2**30


# ── bitcashier's runner units ───────────────────────────────────────────────────────────────────────────────────────
def _comms(machine: Machine, cgroup_dir: str, proc: str = "/proc") -> Set[str]:
    """The command names of every process in `cgroup_dir` and below (a process gone meanwhile is skipped)."""
    pids = [pid for procs in machine.glob(os.path.join(cgroup_dir, "**", "cgroup.procs"), recursive=True)
            for pid in (machine.read(procs) or "").split()]
    return {comm.strip() for comm in (machine.read(os.path.join(proc, pid, "comm")) for pid in pids) if comm is not None}


def busy_units(machine: Machine, pattern: str, proc: str = "/proc") -> List[str]:
    """The runner units matching `pattern` (cgroup directories) that run a job: a Runner.Worker among their processes."""
    return [d for d in machine.glob(pattern) if "Runner.Worker" in _comms(machine, d, proc)]


def busy_runners(machine: Machine, pattern: str) -> int:
    return len(busy_units(machine, pattern))


# ── a cgroup's memory and CPU ───────────────────────────────────────────────────────────────────────────────────────
def stat_value(text: str, key: str) -> Optional[int]:
    """The value of a `key value` line (memory.stat, cpu.stat, meminfo's `MemAvailable: N kB`); None without one."""
    return next((int(line.split()[1]) for line in text.splitlines() if line.startswith(key)), None)


def working_set_bytes(machine: Machine, cgroup_dir: str) -> Optional[int]:
    """A cgroup's memory.current without its inactive page cache, which reclaim takes back before anything else."""
    current = machine.read(os.path.join(cgroup_dir, "memory.current"))
    stat = machine.read(os.path.join(cgroup_dir, "memory.stat"))
    if current is None or stat is None:
        return None
    return int(current) - (stat_value(stat, "inactive_file ") or 0)


def memory_max_gib(machine: Machine, cgroup_dir: str) -> Optional[float]:
    """memory.max in GiB; None for no limit ("max") or an unreadable cgroup."""
    limit = (machine.read(os.path.join(cgroup_dir, "memory.max")) or "").strip()
    return int(limit) / GIB if limit.isdigit() else None


def headroom_gib(machine: Machine, cgroup_dir: str) -> Optional[float]:
    """memory.max less the working set; None for no limit ("max") or an unreadable cgroup."""
    limit = (machine.read(os.path.join(cgroup_dir, "memory.max")) or "").strip()
    used = working_set_bytes(machine, cgroup_dir)
    if not limit.isdigit() or used is None:
        return None
    return (int(limit) - used) / GIB


def cpu_usage_seconds(machine: Machine, cgroup_dir: str) -> Optional[float]:
    """A cgroup's CPU time in all (cpu.stat usage_usec), in seconds; None when unreadable."""
    usec = stat_value(machine.read(os.path.join(cgroup_dir, "cpu.stat")) or "", "usage_usec ")
    return None if usec is None else usec / 1e6


# ── /proc/stat and the cpusets ──────────────────────────────────────────────────────────────────────────────────────
def cpu_times(stat: str, cpus: Optional[frozenset[int]] = None) -> Tuple[int, int]:
    """(busy, total) jiffies off /proc/stat: the `cpu` line, or the sum of the `cpuN` lines of `cpus`. Busy is all but
    idle and iowait (guest is inside user)."""
    lines = stat.splitlines()[:1] if cpus is None else [l for l in stat.splitlines() if _cpu_number(l) in cpus]
    busy = total = 0
    for line in lines:
        v = [int(x) for x in line.split()[1:]]
        busy, total = busy + sum(v[:8]) - v[3] - v[4], total + sum(v[:8])
    return busy, total


def _cpu_number(line: str) -> Optional[int]:
    """N of a `cpuN` line of /proc/stat; None for any other line."""
    return int(line.split()[0][3:]) if line.startswith("cpu") and line[3:4].isdigit() else None


def parse_cpu_list(text: str) -> frozenset[int]:
    """A cpuset list ("6-15,22-31") as the set of CPU numbers; empty or unreadable text is the empty set."""
    cpus: Set[int] = set()
    for part in text.strip().split(","):
        if "-" in part:
            low, high = part.split("-")
            cpus.update(range(int(low), int(high) + 1))
        elif part:
            cpus.add(int(part))
    return frozenset(cpus)


def class_cpusets(machine: Machine, cfg: Config) -> Dict[str, frozenset[int]]:
    """Each class's slice's cpuset.cpus.effective (the CPUs its jobs may run on); a class whose file is unreadable or
    empty has none (admitted on the host's CPUs)."""
    sets = {cls: parse_cpu_list(machine.read(os.path.join(cfg.class_cgroup.format(cls=cls), "cpuset.cpus.effective")) or "")
            for cls in CLASSES}
    return {cls: cpus for cls, cpus in sets.items() if cpus}


class CpuMeter:
    """The host's busy cores over the last `window_seconds`, from /proc/stat samples a tick apart."""

    def __init__(self, window_seconds: int):
        self.window = window_seconds
        self.samples: List[Tuple[float, int, int]] = []

    def add(self, now: float, busy: int, total: int) -> None:
        self.samples = [x for x in self.samples if x[0] >= now - self.window] + [(now, busy, total)]

    def span_seconds(self) -> Optional[float]:
        return self.samples[-1][0] - self.samples[0][0] if len(self.samples) >= 2 else None

    def busy_cores(self, cores: int, released_seconds: float = 0.0) -> Optional[float]:
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
        self.sets: Dict[frozenset[int], CpuMeter] = {}

    def of(self, cpus: frozenset[int]) -> CpuMeter:
        return self.sets.setdefault(cpus, CpuMeter(self.window))

    def sample(self, now: float, stat: Optional[str], sets: Dict[str, frozenset[int]]) -> None:
        """One /proc/stat sample for the host and each of `sets`' cpusets; the meters of a cpuset no class has any more
        go."""
        if stat:
            self.host.add(now, *cpu_times(stat))
            for cpus in set(sets.values()):
                self.of(cpus).add(now, *cpu_times(stat, cpus))
        self.sets = {cpus: meter for cpus, meter in self.sets.items() if cpus in sets.values()}


# ── our runners' containers ─────────────────────────────────────────────────────────────────────────────────────────
class RunnerUsage:
    """Each runner's container, read off its own cgroup every tick: its working set now, its CPU time in all, and the
    cores it used over the last `window_seconds` (from the samples a tick apart; None from one sample). A container
    gone since the last tick keeps its samples while they reach into the window: its CPU there is still in the CPU
    readings, which release it (read_readings, ended_seconds)."""

    def __init__(self, machine: Machine, window_seconds: int):
        self.machine = machine
        self.window = window_seconds
        self.samples: Dict[str, List[Tuple[float, float]]] = {}  # cgroup directory -> [(time, cpu seconds)]
        self.ended: Dict[str, List[Tuple[float, float]]] = {}  # an ended container's cgroup directory -> its samples

    def read(self, cgroup_dir: str, now: float) -> Tuple[Optional[float], Optional[float], Optional[float]]:
        """(mem_gib, cpu_cores, cpu_seconds) of the cgroup, each None when unreadable."""
        used = working_set_bytes(self.machine, cgroup_dir)
        seconds = cpu_usage_seconds(self.machine, cgroup_dir)
        mem = None if used is None else used / GIB
        if seconds is None:
            return mem, None, None
        kept = [x for x in self.samples.get(cgroup_dir, []) if x[0] >= now - self.window] + [(now, seconds)]
        self.samples[cgroup_dir] = kept
        (t0, s0), (t1, s1) = kept[0], kept[-1]
        return mem, ((s1 - s0) / (t1 - t0) if t1 > t0 else None), seconds

    def keep_only(self, cgroup_dirs: Set[Optional[str]], now: float) -> None:
        """Forget the containers not in `cgroup_dirs`, but for the CPU their samples show inside the window."""
        self.ended.update({d: v for d, v in self.samples.items() if d not in cgroup_dirs})
        self.ended = {d: v for d, v in self.ended.items() if d not in cgroup_dirs and v[-1][0] > now - self.window}
        self.samples = {d: v for d, v in self.samples.items() if d in cgroup_dirs}

    def ended_seconds(self, cfg: Config, since: float) -> Dict[str, float]:
        """The CPU seconds each class's slice's ended containers used after `since`, as far as their samples tell (the
        last tick to their end is not seen): class -> seconds."""
        out: Dict[str, float] = {}
        for d, samples in self.ended.items():
            cls = next((c for c in CLASSES if os.path.dirname(d) == cfg.class_cgroup.format(cls=c)), None)
            inside = [s for t, s in samples if t >= since]
            if cls is not None and len(inside) >= 2:
                out[cls] = out.get(cls, 0.0) + inside[-1] - inside[0]
        return out


# ── the readings ────────────────────────────────────────────────────────────────────────────────────────────────────
def read_readings(machine: Machine, cfg: Config, cpu: CpuMeters, now: float,
                  ended: Optional[RunnerUsage] = None) -> Readings:
    """The host now. RELEASED ON FINISH (2026-11-19): the CPU a runner of ours used inside the window before it ended
    (`ended`: what its container's own cgroup showed up to its last tick) is taken out of the busy cores of the CPUs it
    ran on and the host's -- a check job's cores read busy for up to cpu_window_seconds after it ended, while the next
    one starting in its slot was charged its whole cap on top (ci-runner's journal, 2026-11-19: a p90 of 4.6 such
    cores on the check CPUs of the ticks a landing check waited "held by cpu")."""
    stat = machine.read(os.path.join(cfg.proc, "stat"))
    sets = class_cpusets(machine, cfg)
    cpu.sample(now, stat, sets)
    cores = sum(1 for l in (stat or "").splitlines() if _cpu_number(l) is not None) or machine.cpu_count()
    meminfo = machine.read(os.path.join(cfg.proc, "meminfo")) or ""
    return Readings((stat_value(meminfo, "MemAvailable:") or 0) / 2**20, headroom_gib(machine, cfg.ci_cgroup),
                    _known(cfg, lambda d: headroom_gib(machine, d)),
                    cpu.host.busy_cores(cores, released_seconds(cfg, ended, sets, cpu.host, None)), cores,
                    busy_builder_gib(machine, cfg), cpu.host.span_seconds(),
                    {cls: ClassCpu(cpus, cpu.of(cpus).busy_cores(len(cpus), released_seconds(cfg, ended, sets, cpu.of(cpus), cpus)))
                     for cls, cpus in sets.items()},
                    _known(cfg, lambda d: memory_max_gib(machine, d)))


def _known(cfg: Config, read: Callable[[str], Optional[float]]) -> Dict[str, float]:
    """`read` of each class's slice, for the classes it reads a value of (a limited slice's)."""
    values = {cls: read(cfg.class_cgroup.format(cls=cls)) for cls in CLASSES}
    return {cls: v for cls, v in values.items() if v is not None}


def busy_builder_gib(machine: Machine, cfg: Config) -> Tuple[float, ...]:
    """The working set of each bitcashier builder unit running a job."""
    busy = [working_set_bytes(machine, d) for d in busy_units(machine, cfg.builder_cgroups, cfg.proc)]
    return tuple(b / GIB for b in busy if b is not None)


def released_seconds(cfg: Config, ended: Optional[RunnerUsage], sets: Dict[str, frozenset[int]], meter: CpuMeter,
                     cpus: Optional[frozenset[int]]) -> float:
    """CPU seconds the meter's window holds of our ended runners on `cpus` (None: anywhere, the host's)."""
    if ended is None or meter.span_seconds() is None:
        return 0.0
    return sum(seconds for cls, seconds in ended.ended_seconds(cfg, meter.samples[0][0]).items()
               if cpus is None or sets.get(cls) == cpus)
