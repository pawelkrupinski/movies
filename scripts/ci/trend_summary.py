"""The aggregate of a selection of runs (scripts/ci/ci_trends.py): percentiles over each run's measures, per job, per
job's steps and per category, and the flat (row name, value) list one aggregate is charted by.

    from trend_summary import summarize, metric_rows, percentile, stats
"""
from __future__ import annotations

import math
import statistics
from typing import Any, Callable, Dict, Iterable, List, Optional, Sequence, Tuple

from trend_measures import CATEGORY_ORDER, bound_by
from trend_records import Record, section

Number = Optional[float]


def percentile(values: Iterable[Number], fraction: float) -> Number:
    present = sorted(v for v in values if v is not None)
    if not present:
        return None
    if len(present) == 1:
        return present[0]
    position = (len(present) - 1) * fraction
    low = int(math.floor(position))
    high = min(low + 1, len(present) - 1)
    return present[low] + (present[high] - present[low]) * (position - low)


def stats(values: Iterable[Number]) -> Record:
    present = [v for v in values if v is not None]
    if not present:
        return {"n": 0, "p50": None, "p90": None, "max": None}
    return {"n": len(present), "p50": percentile(present, 0.5), "p90": percentile(present, 0.9), "max": max(present)}


def pearson(xs: Sequence[Number], ys: Sequence[Number]) -> Number:
    pairs = [(x, y) for x, y in zip(xs, ys) if x is not None and y is not None]
    return _correlation(pairs) if len(pairs) >= 5 else None


def _correlation(pairs: Sequence[Tuple[float, float]]) -> Number:
    mx = statistics.mean(x for x, _ in pairs)
    my = statistics.mean(y for _, y in pairs)
    sx = math.sqrt(sum((x - mx) ** 2 for x, _ in pairs))
    sy = math.sqrt(sum((y - my) ** 2 for _, y in pairs))
    if sx == 0 or sy == 0:
        return None
    return sum((x - mx) * (y - my) for x, y in pairs) / (sx * sy)


def hit_rate(statuses: Sequence[Any]) -> Number:
    return round(sum(1 for status in statuses if status == "hit") / len(statuses), 3) if statuses else None


def counts(values: Iterable[Any]) -> Dict[Any, int]:
    """Each distinct value with how often it occurs, commonest first."""
    counted: Dict[Any, int] = {}
    for value in values:
        counted[value] = counted.get(value, 0) + 1
    return dict(sorted(counted.items(), key=lambda item: -item[1]))


def _chain(measures: Record) -> str:
    return " → ".join(hop[0] for hop in measures["path"])


def _per_run_row(measures: Record) -> Record:
    return {key: measures.get(key) for key in ("id", "sha", "change", "start", "wall", "elapsed", "path_queue",
                                               "path_work", "queue_total", "runner_min")} | {"path": _chain(measures)}


def _run_level(runs: Sequence[Record]) -> Record:
    return {
        "runs": len(runs), "run_ids": [m["id"] for m in runs],
        **{key: stats([m.get(key) for m in runs])
           for key in ("wall", "runner_min", "peak", "path_queue", "path_work", "queue_total", "elapsed")},
        "per_run": [_per_run_row(m) for m in runs],
        "sat_mean": stats([section(m, "sat").get("mean") for m in runs]),
        "sat_full": stats([section(m, "sat").get("full") for m in runs]),
        "skewed_jobs": sum(m["skewed_jobs"] for m in runs),
    }


def _category_stats(seconds_by_category: Sequence[Dict[str, float]]) -> Record:
    """The stats of every category with any time, in CATEGORY_ORDER."""
    found: Record = {}
    for category in CATEGORY_ORDER:
        values = [seconds.get(category, 0.0) for seconds in seconds_by_category]
        if any(values):
            found[category] = stats(values)
    return found


def _step_stats(rows: Sequence[Record]) -> Record:
    steps: Dict[str, List[float]] = {}
    for row in rows:
        for step, seconds in row["steps"].items():
            steps.setdefault(step, []).append(seconds)
    return {step: stats(values) for step, values in steps.items()}


def _job_cache_rates(rows: Sequence[Record]) -> Record:
    gathered: Dict[str, List[Any]] = {}
    for row in rows:
        for cache, status in row["cache"].items():
            gathered.setdefault(cache, []).append(status)
    return {cache: hit_rate(statuses) for cache, statuses in gathered.items()}


def _medians(items: Sequence[Record], keys: Iterable[str]) -> Record:
    return {key: percentile([item.get(key) for item in items], 0.5) for key in keys}


def _pressure_entry(rows: Sequence[Record]) -> Record:
    """The job's median resources and GC, each only when some run measured it."""
    res = [row["res"] for row in rows if row["res"]]
    gcs = [row["gc"] for row in rows if row["gc"]]
    entry: Record = {}
    if res:
        entry["res"] = _medians(res, res[0])
    if gcs:
        entry["gc"] = _medians(gcs, ("pause_s", "max_pause_ms", "heap_after_max_mb", "of_wall", "collections"))
    return entry


_CORRELATED: Sequence[Tuple[str, Callable[[Record], Number]]] = (
    ("gc", lambda row: section(row, "gc").get("of_wall")),
    ("psi_mem", lambda row: section(row, "res").get("psi_mem_some")),
    ("cpu", lambda row: section(row, "res").get("cpu")),
    ("queue", lambda row: row["queue"]),
)


def _correlations(rows: Sequence[Record], durations: Sequence[float]) -> Dict[str, float]:
    """How closely each pressure figure tracks the job's duration, where there are enough runs to say."""
    correlations: Dict[str, float] = {}
    for label, pick in _CORRELATED:
        r = pearson([pick(row) for row in rows], durations)
        if r is not None:
            correlations[label] = round(r, 2)
    return correlations


def _machines(rows: Sequence[Record]) -> Record:
    models = [row["machine"].get("cpu_model") for row in rows if row["machine"].get("cpu_model")]
    images = [row["machine"].get("image_version") for row in rows if row["machine"].get("image_version")]
    return {"cpu_model": counts(models), "image_version": counts(images)}


def _tests_entry(rows: Sequence[Record]) -> Record:
    tests = [row["tests"] for row in rows if row["tests"]]
    return {"tests": _medians(tests, ("tests", "failed", "skipped", "time_s"))} if tests else {}


def _job_summary(rows: Sequence[Record]) -> Record:
    durations = [row["dur"] for row in rows]
    entry: Record = {"dur": stats(durations), "queue": stats([row["queue"] for row in rows]),
                     "steps": _step_stats(rows), "categories": _category_stats([row["categories"] for row in rows]),
                     "red": sum(1 for row in rows if row["concl"] not in ("success", "skipped")),
                     "cache": _job_cache_rates(rows)}
    entry.update(_pressure_entry(rows))
    entry["bound"], entry["notes"] = bound_by(entry.get("res"), entry.get("gc"))
    entry["corr"] = _correlations(rows, durations)
    entry["machines"] = _machines(rows)
    entry.update(_tests_entry(rows))
    return entry


def _paths(runs: Sequence[Record]) -> List[List[Any]]:
    """The distinct critical paths, commonest first: [chain, runs, queue p50, work p50]."""
    paths: Dict[str, List[Record]] = {}
    for m in runs:
        paths.setdefault(_chain(m), []).append(m)
    return sorted(([chain, len(ms), percentile([x["path_queue"] for x in ms], 0.5),
                    percentile([x["path_work"] for x in ms], 0.5)] for chain, ms in paths.items()),
                  key=lambda row: -row[1])


def _jobs_summary(runs: Sequence[Record]) -> Record:
    names = sorted({name for m in runs for name in m["jobs"]})
    return {name: _job_summary([row for m in runs for row in m["jobs"].get(name, [])]) for name in names}


def _cache_summary(runs: Sequence[Record]) -> Record:
    names = sorted({cache for m in runs for cache in m["cache"]})
    return {name: hit_rate([status for m in runs for status in m["cache"].get(name, [])]) for name in names}


def summarize(measures_list: Iterable[Optional[Record]]) -> Record:
    """The aggregate `report` prints and `compare`/`trend` read: run-level, per job, per job's steps, per category."""
    runs = [m for m in measures_list if m]
    summary = _run_level(runs)
    summary["categories"] = _category_stats([m["categories"] for m in runs])
    summary["jobs"] = _jobs_summary(runs)
    summary["cache"] = _cache_summary(runs)
    summary["paths"] = _paths(runs)
    return summary


def _run_rows(summary: Record) -> List[Tuple[str, Any]]:
    rows = [("run: wall p50", summary["wall"]["p50"]), ("run: wall p90", summary["wall"]["p90"]),
            ("run: critical-path queue p50", summary["path_queue"]["p50"]),
            ("run: critical-path work p50", summary["path_work"]["p50"]),
            ("run: all-jobs queue p50", summary["queue_total"]["p50"]),
            ("run: created-to-end p50", summary["elapsed"]["p50"]),
            ("run: runner-min p50", summary["runner_min"]["p50"]), ("run: peak jobs p50", summary["peak"]["p50"]),
            ("run: slots busy mean", summary["sat_mean"]["p50"]),
            ("run: slots full share", summary["sat_full"]["p50"])]
    rows += [("category: %s p50" % category, figures["p50"]) for category, figures in summary["categories"].items()]
    rows += [("cache: %s hit rate" % name, cache) for name, cache in summary["cache"].items()]
    return rows


def _job_rows(name: str, entry: Record) -> List[Tuple[str, Any]]:
    rows: List[Tuple[str, Any]] = [("job: %s p50" % name, entry["dur"]["p50"]),
                                   ("job: %s p90" % name, entry["dur"]["p90"]),
                                   ("job: %s queue p50" % name, entry["queue"]["p50"])]
    rows += [("job: %s [%s] p50" % (name, category), figures["p50"]) for category, figures in entry["categories"].items()]
    rows += [("step: %s / %s p50" % (name, step), figures["p50"]) for step, figures in entry["steps"].items()
             if (figures["p50"] or 0) >= 20]
    rows += [("res: %s %s" % (name, key), entry["res"][key])
             for key in ("cpu", "psi_mem_some", "psi_mem_full", "psi_cpu_some", "steal")
             if entry.get("res", {}).get(key) is not None]
    if entry.get("gc"):
        rows.append(("gc: %s share of wall" % name, entry["gc"]["of_wall"]))
    return rows


def metric_rows(summary: Record) -> List[Tuple[str, Any]]:
    """The flat (row name, value) list one aggregate is charted by: what `trend` and `compare` line up."""
    rows = _run_rows(summary)
    for name, entry in summary["jobs"].items():
        rows += _job_rows(name, entry)
    return rows
