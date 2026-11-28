"""The alert backtest's PURE arithmetic over time series (infra/bin/backtest-alerts; unit-tested in
infra/test/test_backtest_alerts.py): Prometheus durations and timestamps, the `for:` hold, firing episodes, a rollout's
first appearance, and an alert's verdict against its budget. Nothing here reads a file, the clock or the network."""

from __future__ import annotations

import datetime
import json
import re
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Set, Tuple

Run = Tuple[float, float]
Stats = Dict[str, Any]

_DURATION_UNITS = {"ms": 0.001, "s": 1, "m": 60, "h": 3600, "d": 86400, "w": 604800, "y": 31536000}


def seconds(duration: Any) -> float:
    """A Prometheus duration ("1h30m", "15s", "2d") in seconds; 0 when unset."""
    if duration in (None, "", 0):
        return 0
    if isinstance(duration, (int, float)):
        return float(duration)
    parts = re.findall(r"(\d+)(ms|[smhdwy])", duration)
    if not parts or "".join(n + u for n, u in parts) != duration:
        raise ValueError("not a Prometheus duration: %r" % duration)
    return sum(int(n) * _DURATION_UNITS[u] for n, u in parts)


def parse_time(value: Any) -> float:
    if isinstance(value, datetime.datetime):
        stamp = value
    else:
        stamp = datetime.datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    if stamp.tzinfo is None:
        stamp = stamp.replace(tzinfo=datetime.timezone.utc)
    return stamp.timestamp()


def iso(ts: float) -> str:
    return datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def grid_of(start: float, end: float, step: float) -> List[float]:
    n = int((end - start) // step)
    return [start + i * step for i in range(n + 1)]


def snap_up(t: float, step: float) -> float:
    return t if t % step == 0 else t - (t % step) + step


def firing_steps(present: Set[float], grid: Sequence[float], hold: float, keep_firing_for: float = 0) -> Set[float]:
    """The set of grid timestamps at which one series is FIRING.

    `present` is the set of timestamps at which the expression returned this series. A series is
    pending from the first step of an unbroken run of presence and fires once the run is at least
    `hold` seconds old -- which is what the rule evaluator does, at the grid's resolution. A step
    with no sample ends the run and resets the hold, as a real gap does. `keep_firing_for` keeps an
    already-firing series firing that long past its last sample.
    """
    firing: Set[float] = set()
    run_start: Optional[float] = None
    last_fired: Optional[float] = None
    for t in grid:
        if t not in present:
            run_start = None
            if _kept_firing(t, last_fired, keep_firing_for):
                firing.add(t)
            continue
        run_start = _run_start(run_start, t)
        if t - run_start >= hold:
            firing.add(t)
            last_fired = t
    return firing


def _run_start(run_start: Optional[float], t: float) -> float:
    """The start of the unbroken run `t` belongs to: the open one, or `t` itself when none is."""
    return t if run_start is None else run_start


def _kept_firing(t: float, last_fired: Optional[float], keep_firing_for: float) -> bool:
    return last_fired is not None and t - last_fired <= keep_firing_for


def episodes(steps: Iterable[float], grid: Sequence[float]) -> List[Run]:
    """[(start, end)] contiguous runs of `steps` along `grid` (end is the last firing step)."""
    wanted = set(steps)
    runs: List[Run] = []
    start = prev = None
    for t in grid:
        if t in wanted:
            if start is None:
                start = t
            prev = t
        elif start is not None and prev is not None:
            runs.append((start, prev))
            start = None
    if start is not None and prev is not None:
        runs.append((start, prev))
    return runs


def _inside(t: float, windows: Sequence[Run]) -> bool:
    return any(a <= t <= b for a, b in windows)


def _outside(steps: Iterable[float], windows: Sequence[Run]) -> Set[float]:
    return {t for t in steps if not _inside(t, windows)}


def _series_runs(fired: Mapping[str, Set[float]], excused: Sequence[Run], in_window: Sequence[float]) -> List[Tuple[str, float, float]]:
    return [(key, a, b) for key, steps in fired.items() for a, b in episodes(_outside(steps, excused), in_window)]


def _fraction(count: int, total: int) -> float:
    return count / total if total else 0.0


def _peak(fired: Mapping[str, Set[float]]) -> int:
    counts: Dict[float, int] = {}
    for steps in fired.values():
        for t in steps:
            counts[t] = counts.get(t, 0) + 1
    return max(counts.values()) if counts else 0


def summarise(series_present: Mapping[str, Set[float]], grid: Sequence[float], hold: float, keep_firing_for: float,
              window_start: float, excused: Sequence[Run] = ()) -> Stats:
    """Firing statistics for one alert over the part of `grid` at or after `window_start`.

    `series_present` maps a series key to the timestamps it was returned at. The grid starts early
    by the hold so an alert already pending when the window opens is not reset by its edge.
    `excused` is [(start, end)] windows of catalogued incidents in which this alert is EXPECTED to
    fire: firing inside them is reported, but does not count against the noise budget.
    """
    in_window = [t for t in grid if t >= window_start]
    fired = {key: {t for t in firing_steps(present, grid, hold, keep_firing_for) if t >= window_start}
             for key, present in series_present.items()}
    union: Set[float] = set().union(*fired.values())
    budget_grid = list(_outside(in_window, excused))
    budget_grid.sort()
    # each series' firing outside the catalogued incidents: the evidence a noisy verdict is judged on
    series_runs = _series_runs(fired, excused, in_window)
    unexcused = _outside(union, excused)
    return {
        "firing_fraction": _fraction(len(union), len(in_window)),
        "episodes": len(episodes(union, in_window)),
        "budget_firing_fraction": _fraction(len(unexcused), len(budget_grid)),
        "budget_episodes": len(episodes(unexcused, budget_grid)),
        "series_episodes": sum(len(episodes(steps, in_window)) for steps in fired.values()),
        "peak_series": _peak(fired),
        "returned_data": any(t >= window_start for present in series_present.values() for t in present),
        "runs": episodes(union, in_window),
        "series_runs": sorted(series_runs, key=lambda run: (run[1], run[0])),
    }


def noisy_episodes(stats: Stats, limit: int = 60) -> Tuple[int, List[Dict[str, Any]]]:
    """(how many, the first `limit` by start) of a noisy alert's firing runs outside catalogued incidents, each
    with its series' labels: whether it was a real incident (catalogue it), a rule meaning the wrong thing (fix
    it) or a fair rate (budget it) is read off these, not off one longest episode (Alert backtest #1)."""
    runs = sorted(stats.get("series_runs", []), key=lambda run: (run[1], run[0]))
    return len(runs), [{"series": json.loads(key), "from": iso(a), "to": iso(b)} for key, a, b in runs[:limit]]


def rollout_start(presence: Set[float], grid: Sequence[float], min_coverage: float = 0.9) -> Optional[float]:
    """When a metric was ROLLED OUT inside the window, the time it first appeared; else None.

    A metric that appears part-way through the window and is then present for (nearly) all of
    the rest was deployed then, and an alert over it -- above all an `absent`-style one -- was
    "firing" before that only because the rule is newer than the data. A metric that comes and
    goes (published only while a host is blocked, a pod is waiting...) is not a rollout, and is
    not clipped: its gaps are the signal.
    """
    if not presence or not grid:
        return None
    first = min(presence)
    if first <= grid[0]:
        return None
    after = [t for t in grid if t >= first]
    if len([t for t in after if t in presence]) / float(len(after)) < min_coverage:
        return None
    return first


JUDGED_BY = ("fraction and episodes", "episodes")


def _verdict_without_data(allow: Mapping[str, Any], missing_metrics: Sequence[str], deployed: bool) -> Tuple[str, Optional[str]]:
    if not missing_metrics:
        return "quiet", None
    if not deployed:
        return "not-deployed", None
    if allow.get("dead"):
        return "dead", None
    return "dead", ("returned no data for the whole window and reads metrics with no "
                    "series in it: %s" % ", ".join(sorted(missing_metrics)))


def _budget_problems(stats: Stats, budget: Mapping[str, Any], allow: Mapping[str, Any], judged_by: str) -> List[str]:
    weeks = max(stats.get("days", budget["days"]), 1.0) / 7.0
    max_fraction = allow.get("max_firing_fraction", budget["max_firing_fraction"])
    max_per_week = allow.get("max_episodes_per_week", budget["max_episodes_per_week"])
    fraction, count = stats["budget_firing_fraction"], stats["budget_episodes"]
    problems: List[str] = []
    if judged_by != "episodes" and fraction > max_fraction:
        problems.append("fired %.1f%% of the window outside catalogued incidents (budget %.1f%%)"
                        % (100 * fraction, 100 * max_fraction))
    if count / weeks > max_per_week:
        problems.append("fired %d distinct times outside catalogued incidents, %.1f/week "
                        "(budget %s/week)" % (count, count / weeks, max_per_week))
    return problems


def verdict(stats: Stats, budget: Mapping[str, Any], allow: Mapping[str, Any], missing_metrics: Sequence[str],
            deployed: bool) -> Tuple[str, Optional[str]]:
    """(status, problem or None) for one alert's statistics against its budget and allowlist.

    status is one of: ok, quiet, noisy, young, dead, not-deployed, error. YOUNG: noisy over a window shorter than
    `min_judged_days` -- its metrics rolled out that recently -- reported, never a failure: a fraction of a two-day life
    is no verdict on a rule (the owner, 2026-11-18: rules newer than the snapshot are skipped and reported; Alert
    backtest #5's CI-regression and poster alerts failed at "100%" of under three days). It is judged once it has
    lived the minimum. Dead is judged at any age: no data is never healthy.

    An allowlist entry `judged_by: episodes` drops the firing-fraction budget and keeps the episode one: for an alert
    whose firing IS a third party's state (a host's breaker held open), how long it fires is how long the outage
    lasted, and only how OFTEN it starts says whether the rule misreads blips (Alert backtest #15: one host's
    multi-day certificate outage failed WorkerHttpHostCircuitOpen at 11.8% from three episodes).
    """
    judged_by = allow.get("judged_by", "fraction and episodes")
    if judged_by not in JUDGED_BY:
        return "error", "allowlist judged_by %r is not one of: %s" % (judged_by, ", ".join(JUDGED_BY))
    if stats.get("error"):
        return "error", "query failed: %s" % stats["error"]
    if not stats["returned_data"]:
        return _verdict_without_data(allow, missing_metrics, deployed)
    problems = _budget_problems(stats, budget, allow, judged_by)
    if not problems:
        return "ok", None
    if allow.get("noisy"):
        return "noisy", None
    if stats.get("days", budget["days"]) < budget.get("min_judged_days", 0):
        return "young", None
    return "noisy", "; ".join(problems)
