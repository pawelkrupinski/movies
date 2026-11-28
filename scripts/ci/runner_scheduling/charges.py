"""What admission charges, pure: bitcashier's saturation and its reserve, the memory and cores free for a class, and
what our runners that have not shown themselves in the readings yet still owe them (runner_scheduler.py's docstring,
"ADMISSION IS WHAT IS FREE NOW")."""
from __future__ import annotations

import statistics
from typing import Iterable, Optional

from .model import Bitcashier, Config, Instance, Readings

def saturated(cfg: Config, bc: Bitcashier) -> bool:
    """Every builder or every deployer busy: a bitcashier job may be waiting for a runner, so movies starts nothing."""
    return bc.busy_builders >= cfg.builders or bc.busy_deployers >= cfg.deployers


def whole_jobs(free: float, size: float) -> int:
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


def cpu_of(r: Readings, cls: Optional[str]) -> tuple[Optional[float], int]:
    """(busy cores, cores) of the CPUs `cls` runs on: its slice's cpuset when read, else (or for None) the host's."""
    own = None if cls is None else r.class_cpu.get(cls)
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


def cpu_owed(cfg: Config, r: Readings, runners: list[Instance], now: float) -> float:
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


def memory_owed(cfg: Config, runners: Iterable[Instance]) -> float:
    """What `runners` still owe the memory readings in all (memory_charge each)."""
    return sum(memory_charge(cfg, i) for i in runners)
