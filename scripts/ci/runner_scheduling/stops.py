"""The idle runners a tick stops, pure (decide's first step): bitcashier saturated, past max_slots or memory short of
what our settling runners still owe, an idle borrowed check while a landing-path measurement waits, and a runner idle
too long with nothing of its own queued."""
from __future__ import annotations

from typing import Dict, List, Tuple

from .charges import memory_charge, memory_free_gib, memory_owed, saturated, settling
from .jobs import queued_for, uncovered
from .model import CLASSES, LANDING_MEASURE_LANES, Bitcashier, Config, Instance, Queued, Readings


def idle_to_stop(cfg: Config, bc: Bitcashier, demand: Dict[str, int], instances: List[Instance], now: float, r: Readings,
                 queue: List[Queued]) -> List[int]:
    """Bitcashier saturated: the idle heavy runners (a light lane runner is too small to hold bitcashier back). Past
    max_slots, or memory short of what our settling runners still owe: idle ones, lowest class first. An idle borrowed
    check while a queued landing-path measurement has no runner of its lane waiting (it holds that one's slot, cap and
    cores). And an idle runner whose
    class (a lane runner's lane and pin) has nothing queued for longer than idle_stop_seconds."""
    idle = [i for i in instances if not i.busy]
    stops = _shed(cfg, idle, _saturated_stops(cfg, bc, idle), *_overflow(cfg, bc, instances, now, r))
    stops += _borrowed_stops(idle, stops, queue)
    kept = stops + _warm_light(cfg, idle, stops)
    return stops + [i.slot for i in idle if i.slot not in kept and _idle_too_long(cfg, i, demand, queue, now)]


def _saturated_stops(cfg: Config, bc: Bitcashier, idle: List[Instance]) -> List[int]:
    return [i.slot for i in idle if i.cls != "light"] if saturated(cfg, bc) else []


def _overflow(cfg: Config, bc: Bitcashier, instances: List[Instance], now: float, r: Readings) -> Tuple[int, float]:
    """(runners past max_slots, GiB the memory is short of what our settling runners still owe)."""
    excess = max(0, sum(1 for i in instances if i.slot <= cfg.max_slots) - cfg.max_slots)
    return excess, -memory_free_gib(cfg, bc, r, memory_owed(cfg, [i for i in instances if settling(cfg, i, now)]))


def _shed(cfg: Config, idle: List[Instance], stops: List[int], excess: int, short: float) -> List[int]:
    """`stops` and the idle runners, lowest class first, that end the excess and the shortfall."""
    for inst in sorted(idle, key=lambda i: (-CLASSES.index(i.cls), i.slot)):
        if excess <= 0 and short <= 0:
            break
        if inst.slot not in stops:
            stops.append(inst.slot)
            excess -= 1
            short -= memory_charge(cfg, inst)
    return stops


def _borrowed_stops(idle: List[Instance], stops: List[int], queue: List[Queued]) -> List[int]:
    """The idle borrowed checks, while a landing-path measurement has no runner of its lane waiting for it (one that has
    its runner needs nothing the borrow holds; an optional measurement waits behind a borrowed check,
    LANDING_MEASURE_LANES)."""
    waiting = [(i.lane, i.pin) for i in idle if i.cls == "measure" and i.slot not in stops]
    waiting_for = [q for q in uncovered(queue, waiting) if q.lane in LANDING_MEASURE_LANES]
    return [i.slot for i in idle if i.slot not in stops and i.borrowed and waiting_for]


def _warm_light(cfg: Config, idle: List[Instance], stops: List[int]) -> List[int]:
    """THE WARM LIGHT RUNNER: the lowest-slot idle light runners, up to warm_light_runners, are never idle too long."""
    light = [i.slot for i in sorted(idle, key=lambda i: i.slot) if i.cls == "light" and i.slot not in stops]
    return light[:cfg.warm_light_runners]


def _idle_too_long(cfg: Config, inst: Instance, demand: Dict[str, int], queue: List[Queued], now: float) -> bool:
    return queued_for(inst, demand, queue) == 0 and now - inst.idle_since >= cfg.idle_stop_seconds
