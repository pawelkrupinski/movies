"""The queue, pure: the jobs-API's queued jobs as each class's demand and each lane's queued jobs (oldest first, with
their runs' pins), which of them a waiting runner covers, the loop's pace, and the classes starved past
starve_report_seconds (runner_scheduler.py's docstring: THE MEASURE LANES, THE CHECK LANES, OLDEST FIRST)."""
from __future__ import annotations

import datetime
from typing import Any, Dict, List, Optional, Tuple, cast

from .model import CHECK_LANES, CLASS_OF, CLASSES, LABELS, LANES, PIN_PREFIX, ADMISSION_LIMITS, Config, Decision, Instance, Queued

Job = Dict[str, Any]  # a job of the jobs API, as GitHub lists it


def uncovered(queue: List[Queued], waiting: List[Tuple[str, str]]) -> List[Queued]:
    """The jobs of `queue` (oldest first) that no runner of `waiting` ((lane, pin) each) covers: a waiting runner covers
    the oldest job of its lane and pin not covered yet -- a pinned runner can take its run's jobs alone, an unpinned
    one the unpinned (one a pinned runner happens to take leaves its run's job to the next tick)."""
    left: Dict[Tuple[str, str], int] = {}
    for key in waiting:
        left[key] = left.get(key, 0) + 1
    out: List[Queued] = []
    for q in queue:
        if left.get((q.lane, q.pin), 0):
            left[(q.lane, q.pin)] -= 1
        else:
            out.append(q)
    return out


def queued_for(inst: Instance, demand: Dict[str, int], queue: List[Queued]) -> int:
    """The queued jobs `inst` can take: its lane's of its pin (a lane runner takes no other lane's job, a pinned one no
    other run's), else its class's."""
    if inst.cls in LANES:
        return sum(1 for q in queue if q.lane == inst.lane and q.pin == inst.pin)
    return demand.get(inst.cls, 0)


def queued_jobs(demand: Dict[str, int], lanes: Optional[Dict[str, int]], queue: Optional[List[Queued]]) -> List[Queued]:
    """The laned classes' queued jobs, oldest first: `queue` (queue_of), or only their counts per lane (`lanes`, none
    pinned); a class whose lanes neither names has all its `demand` in its plain label's lane."""
    counted = dict(lanes or {})
    jobs = sorted(queue if queue is not None else _unpinned(counted), key=lambda q: q.created)
    for cls, order in LANES.items():
        if not _names_lane(order, counted, jobs):
            jobs += [Queued(LABELS[cls])] * demand.get(cls, 0)
    return jobs


def _unpinned(counted: Dict[str, int]) -> List[Queued]:
    return [Queued(lane) for lane, n in counted.items() for _ in range(n)]


def _names_lane(order: Tuple[str, ...], counted: Dict[str, int], jobs: List[Queued]) -> bool:
    return any(lane in counted for lane in order) or any(q.lane in order for q in jobs)


def demand_of(jobs: List[Job]) -> Dict[str, int]:
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


def queue_of(jobs: List[Job]) -> List[Queued]:
    """The queued jobs of the laned classes (LANES: the check and the measure lanes), oldest first, from jobs-API job
    dicts: each its lane (a job naming two lanes of a class is the higher's), its pin label (PIN_PREFIX; "" for none)
    and its created_at."""
    out: List[Queued] = []
    for job in jobs:
        if job.get("status") != "queued":
            continue
        labels: List[str] = job.get("labels", [])
        pin = next((l for l in labels if l.startswith(PIN_PREFIX)), "")
        created = iso_seconds(job["created_at"]) if job.get("created_at") else 0.0
        for order in LANES.values():
            named = [l for l in labels if l in order]
            if named:
                out.append(Queued(min(named, key=order.index), pin, created))
    return sorted(out, key=lambda q: q.created)


def pace_seconds(cfg: Config, queue: List[Queued], remaining: int) -> int:
    """The loop's pause between ticks: busy_tick_seconds while a check is queued, else tick_seconds; four times that
    when the reader's rate budget runs low (under 1500), so the App's other users never starve."""
    pace = cfg.busy_tick_seconds if any(q.lane in CHECK_LANES for q in queue) else cfg.tick_seconds
    return pace if remaining > 1500 else cfg.tick_seconds * 4


def lane_demand_of(jobs: List[Job]) -> Dict[str, int]:
    """Queued jobs per lane (queue_of)."""
    out: Dict[str, int] = {}
    for q in queue_of(jobs):
        out[q.lane] = out.get(q.lane, 0) + 1
    return out


def iso_seconds(stamp: str) -> float:
    """A GitHub timestamp (`2026-11-08T12:23:21Z`) as epoch seconds."""
    return datetime.datetime.fromisoformat(stamp.replace("Z", "+00:00")).timestamp()


def oldest_waits(jobs: List[Job], now: float) -> Dict[str, int]:
    """{class: seconds its oldest queued job has waited}, from jobs-API job dicts (`created_at`), for classes with one."""
    out: Dict[str, int] = {}
    for job in jobs:
        classes = [CLASS_OF[l] for l in job.get("labels", []) if l in CLASS_OF]
        if job.get("status") != "queued" or not classes or not job.get("created_at"):
            continue
        cls = min(classes, key=CLASSES.index)
        out[cls] = max(out.get(cls, 0), int(now - iso_seconds(job["created_at"])))
    return out


def starved(cfg: Config, d: Decision, demand: Dict[str, int], waits: Dict[str, int]) -> List[str]:
    """A line per class queued past `starve_report_seconds` with none of it starting this tick: how many, how long the
    oldest waited, and what holds it (held_by)."""
    started = {cls for _, cls in d.starts}
    return [f"STARVED {cls}: {demand[cls]} queued, the oldest {waits[cls] // 60} min, held by {', '.join(held_by(d, cls)) or 'nothing named'}"
            for cls in CLASSES if demand.get(cls) and cls not in started and waits.get(cls, 0) >= cfg.starve_report_seconds]


def held_by(d: Decision, cls: str) -> List[str]:
    """What holds `cls`: bitcashier saturated, or each of the class's admission limits at zero (ADMISSION_LIMITS: a
    measurement's in d.limits, a landing check's in d.limits["check"])."""
    if d.saturated and cls != "light":
        return ["bitcashier saturated"]
    terms = cast(Dict[str, object], d.limits.get("check", {})) if cls == "check" else d.limits
    return [k for k in ADMISSION_LIMITS if _at_zero(terms.get(k))]


def _at_zero(limit: object) -> bool:
    return isinstance(limit, (int, float)) and limit <= 0
