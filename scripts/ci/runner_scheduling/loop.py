"""The loop: a tick reads bitcashier's runners, GitHub's queue, ours and the host, decides, says what it saw and did in
one line, and starts and stops runners; the loop paces the ticks (pace_seconds)."""
from __future__ import annotations

import sys
from typing import Dict, List, Optional, Tuple

from .admission import decide
from .charges import builder_reserve_gib, reserved_builders
from .github import GitHub
from .host import Host
from .jobs import demand_of, lane_demand_of, oldest_waits, pace_seconds, queue_of, starved
from .machine import Machine
from .model import PIN_PREFIX, Bitcashier, Config, Decision, Instance, Readings
from .readings import CpuMeters, busy_runners, read_readings


def tick(machine: Machine, cfg: Config, host: Host, reader: GitHub, cpu: CpuMeters,
         dry_run: bool = False) -> Tuple[Decision, int]:
    now = machine.now()
    bc = Bitcashier(busy_runners(machine, cfg.builder_cgroups), busy_runners(machine, cfg.deployer_cgroups))
    jobs = reader.queued_jobs()
    demand, lanes, queue = demand_of(jobs), lane_demand_of(jobs), queue_of(jobs)
    instances = host.instances(now)  # first: a container ended since the last tick releases its CPU in the readings
    r = read_readings(machine, cfg, cpu, now, host.usage)
    d = decide(cfg, bc, demand, instances, now, r, queue=queue)
    print(status_line(cfg, bc, d, r, demand, lanes, instances), flush=True)
    for line in starved(cfg, d, demand, oldest_waits(jobs, now)):
        print(line, flush=True)
    if not dry_run:
        carry_out(host, d)
    return d, pace_seconds(cfg, queue, reader.remaining)


def carry_out(host: Host, d: Decision) -> None:
    """The decision's stops, then its starts (a borrowed check in the measure slice, each lane start with its label and
    pin)."""
    for slot in d.stops:
        host.stop(slot)
    labels, pins = dict(d.labels), dict(d.pins)
    for slot, cls in d.starts:
        host.start(slot, cls, "measure" if slot in d.borrowed else "", labels.get(slot, ""), pins.get(slot, ""))


def _gib(v: Optional[float]) -> str:
    return "-" if v is None else f"{v:.0f}"


def _cores(v: Optional[float]) -> str:
    return "-" if v is None else f"{v:.1f}"


def runner_name(i: Instance) -> str:
    """A runner of ours as the log line names it: its class, @ the slice it borrowed, / its lane, / its pinned run."""
    return (i.cls + (f"@{i.runs_in}" if i.borrowed else "") + (f"/{i.label}" if i.label else "")
            + (f"/{i.pin[len(PIN_PREFIX):]}" if i.pin else ""))


def status_line(cfg: Config, bc: Bitcashier, d: Decision, r: Readings, demand: Dict[str, int], lanes: Dict[str, int],
                instances: List[Instance]) -> str:
    """What the tick saw and did, in one line: bitcashier's runners, the room and what held it, the readings, the queue,
    ours, and the starts and stops."""
    ours = [(i.slot, runner_name(i), "busy" if i.busy else "idle", _gib(i.mem_gib), _cores(i.cpu_cores)) for i in instances]
    return (f"bitcashier busy {bc.busy_builders}/{cfg.builders} builders {bc.busy_deployers}/{cfg.deployers} deployers"
            f"{' SATURATED' if d.saturated else ''}; room {d.room}; reserve {reserved_builders(cfg, bc)} builders "
            f"x {builder_reserve_gib(cfg, r):.1f} GiB; held by {d.limits}; "
            f"free GiB host {_gib(r.mem_available_gib)} "
            f"ci {_gib(r.ci_free_gib)} {({c: round(v) for c, v in r.class_free_gib.items()})}; cpu busy "
            f"{_gib(r.cpu_busy_cores)}/{r.cpu_cores} "
            f"{({c: f'{_gib(v.busy_cores)}/{len(v.cpus)}' for c, v in r.class_cpu.items()})}; queued {demand} "
            f"lanes {lanes}; ours {ours}; "
            f"start {list(d.starts)} lanes {list(d.labels)} pins {[(s, p[len(PIN_PREFIX):]) for s, p in d.pins if p]} borrowed {list(d.borrowed)} warm {list(d.warm)} "
            f"stop {list(d.stops)}")


def loop(machine: Machine, cfg: Config, host: Host, reader: GitHub, cpu: CpuMeters, dry_run: bool, once: bool) -> int:
    """Tick (once, or for ever), each tick pace_seconds after the last one started."""
    while True:
        started = machine.now()
        pace = guarded_tick(machine, cfg, host, reader, cpu, dry_run)
        if once:
            return 0
        machine.sleep(max(1.0, pace - (machine.now() - started)))


def guarded_tick(machine: Machine, cfg: Config, host: Host, reader: GitHub, cpu: CpuMeters, dry_run: bool) -> int:
    """One tick and the pace it asks for; one bad tick (GitHub down) must not end the loop: said, at the plain pace."""
    pace = cfg.tick_seconds if reader.remaining > 1500 else cfg.tick_seconds * 4
    try:
        _d, pace = tick(machine, cfg, host, reader, cpu, dry_run)
    except Exception as e:  # noqa: BLE001 -- one bad tick (GitHub down) must not end the loop
        print(f"tick failed: {e}", file=sys.stderr, flush=True)
    return pace
