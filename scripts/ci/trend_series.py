"""A selection's aggregates over time (scripts/ci/ci_trends.py): time buckets, a metric row's sparkline, slope and
change, the rows that regressed against the previous range, and where a regression started (the change point, with the
machine change that lines up with it).

    from trend_series import trend, buckets, bucket_start, regressions, sparkline
"""
from __future__ import annotations

import datetime
import statistics
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

from trend_records import Record, iso
from trend_summary import Number, counts, metric_rows, summarize

SPARK = "▁▂▃▄▅▆▇█"
_BUCKET_SECONDS = {"hour": 3600, "day": 86400, "week": 604800}
Point = Tuple[Record, Number]
Split = Tuple[Tuple[float, float], Record, Record, float, float]


def bucket_start(seconds: float, by: str) -> float:
    moment = datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc)
    if by == "hour":
        moment = moment.replace(minute=0, second=0, microsecond=0)
    elif by == "day":
        moment = moment.replace(hour=0, minute=0, second=0, microsecond=0)
    elif by == "week":
        moment = (moment - datetime.timedelta(days=moment.weekday())).replace(hour=0, minute=0, second=0,
                                                                               microsecond=0)
    else:
        raise ValueError("bucket by hour, day or week, not %r" % by)
    return moment.timestamp()


def bucket_label(seconds: float, by: str) -> str:
    moment = datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc)
    return moment.strftime("%m-%d %H:00" if by == "hour" else ("%Y-%m-%d" if by == "day" else "wk %Y-%m-%d"))


def _bucket_keys(first: float, last: float, by: str) -> List[float]:
    keys: List[float] = []
    t = first
    step = _BUCKET_SECONDS[by]
    while t <= last + 1:
        keys.append(t)
        t = bucket_start(t + step + 3600, by)  # +1h: a DST-free UTC clock needs none, but never loop on a bad step
    return keys


def buckets(measures_list: Iterable[Optional[Record]], by: str, since: Optional[float] = None,
            until: Optional[float] = None) -> List[Tuple[float, List[Record]]]:
    """[(bucket start, [measures])], every bucket from the first to the last, empty ones included."""
    runs = [m for m in measures_list if m]
    if not runs:
        return []
    starts = [m["start"] for m in runs]
    keys = _bucket_keys(bucket_start(min(starts) if since is None else since, by),
                        bucket_start(max(starts) if until is None else until, by), by)
    grouped: Dict[float, List[Record]] = {key: [] for key in keys}
    for m in runs:
        grouped.get(bucket_start(m["start"], by), []).append(m)
    return [(key, grouped[key]) for key in keys]


def sparkline(values: Sequence[Number]) -> str:
    present = [v for v in values if v is not None]
    if not present:
        return " " * len(values)
    low, high = min(present), max(present)
    span = high - low or 1.0
    return "".join(" " if v is None else SPARK[min(len(SPARK) - 1, int((v - low) / span * (len(SPARK) - 1) + 0.5))]
                   for v in values)


def slope(values: Sequence[Number]) -> Number:
    """Least-squares change per bucket over the buckets that have a value."""
    points = [(index, value) for index, value in enumerate(values) if value is not None]
    if len(points) < 2:
        return None
    mx = statistics.mean(i for i, _ in points)
    my = statistics.mean(v for _, v in points)
    denominator = sum((i - mx) ** 2 for i, _ in points)
    return sum((i - mx) * (v - my) for i, v in points) / denominator if denominator else None


def change(first: Number, last: Number) -> Number:
    if first is None or first == 0 or last is None:
        return None
    return (last - first) / abs(first)


def _split_at(points: Sequence[Tuple[Record, float]], k: int) -> Split:
    """The split before point `k`: its score (the gap between the medians either side; ties go to the split whose
    sides are most uniform), the last run before it and the first after, and the two medians."""
    left = [v for _, v in points[:k]]
    right = [v for _, v in points[k:]]
    before, after = statistics.median(left), statistics.median(right)
    spread = sum(abs(v - before) for v in left) + sum(abs(v - after) for v in right)
    return (abs(after - before), -spread), points[k - 1][0], points[k][0], before, after


def change_point(series: Iterable[Point], minimum: int = 3) -> Optional[Tuple[Record, Record, float, float]]:
    """Where a chronological [(measures, value)] series stepped: the split with the largest gap between the medians
    either side (each side at least `minimum` runs). (last run before, first run after, before median, after median)."""
    points = [(m, v) for m, v in series if v is not None]
    splits = [_split_at(points, k) for k in range(minimum, len(points) - minimum + 1)]
    if not splits:
        return None
    best = max(splits, key=lambda split: split[0])
    return best[1], best[2], best[3], best[4]


def per_run_value(row_name: str, m: Record) -> Any:
    """One run's value of a metric row, for change_point: the same row computed over that run alone."""
    for name, value in metric_rows(summarize([m])):
        if name == row_name:
            return value
    return None


# Rows that can regress: time-like, where up is worse. Shares of slots and cache hit rates are context, not verdicts.
def regresses(name: str) -> bool:
    return not name.startswith(("run: slots", "cache:", "run: peak"))


def _regression(name: str, old: float, value: float, threshold: float, minimum_seconds: float
                ) -> Optional[Tuple[str, float, float, float]]:
    if name.startswith("cache:"):
        return (name, old, value, value - old) if old - value > threshold else None
    if not regresses(name) or old <= 0:
        return None
    absolute = name.startswith(("res:", "gc:")) or value - old >= minimum_seconds
    return (name, old, value, (value - old) / old) if (value - old) / old > threshold and absolute else None


def regressions(current: Iterable[Optional[Record]], previous: Iterable[Optional[Record]], threshold: float,
                minimum_seconds: float = 10.0) -> List[Tuple[str, float, float, float]]:
    """The rows whose p50 rose more than `threshold` (0.2 = 20 %) and by at least `minimum_seconds` from the previous
    range to this one, worst first; a cache's hit rate falling by more than the threshold, too."""
    before = dict(metric_rows(summarize(previous)))
    flagged = [_regression(name, before[name], value, threshold, minimum_seconds)
               for name, value in metric_rows(summarize(current))
               if value is not None and before.get(name) is not None]
    return sorted((row for row in flagged if row), key=lambda row: -abs(row[3]))


def measures_machine_shift(before: Sequence[Record], after: Sequence[Record]) -> List[str]:
    """The commonest image version / CPU model across run measures before a change point and after, when they differ."""
    def common(measures: Sequence[Record], key: str) -> Optional[str]:
        counted = counts([row["machine"].get(key) for m in measures for rows in m["jobs"].values() for row in rows
                          if row["machine"].get(key)])
        return next(iter(counted), None)
    shifts: List[str] = []
    for key in ("image_version", "cpu_model"):
        a, b = common(before, key), common(after, key)
        if a and b and a != b:
            shifts.append("%s %s → %s" % (key, a, b))
    return shifts


def _row_table(per_bucket: Sequence[Dict[str, Any]]) -> List[Record]:
    names: List[str] = []
    for rows in per_bucket:
        names += [name for name in rows if name not in names]
    table: List[Record] = []
    for name in names:
        values = [rows.get(name) for rows in per_bucket]
        present = [v for v in values if v is not None]
        table.append({"row": name, "values": values, "spark": sparkline(values), "slope": slope(values),
                      "change": change(present[0], present[-1]) if present else None})
    return table


def _moment(measures: Record) -> Record:
    return {"run": measures["id"], "sha": measures["sha"], "at": iso(measures["start"])}


def _flag(name: str, old: float, new: float, delta: float, chronological: Sequence[Record]) -> Record:
    """A regressed row, pointed at the push window where it started and the machine change that lines up with it."""
    point = change_point([(m, per_run_value(name, m)) for m in chronological])
    shift: List[str] = []
    if point:
        shift = measures_machine_shift([m for m in chronological if m["start"] <= point[0]["start"]],
                                       [m for m in chronological if m["start"] >= point[1]["start"]])
    return {"row": name, "previous": old, "current": new, "change": delta, "machine_shift": shift,
            "between": None if not point else {
                "last_before": _moment(point[0]), "first_after": _moment(point[1]),
                "median_before": point[2], "median_after": point[3]}}


def trend(measures_list: Sequence[Optional[Record]], by: str, since: Optional[float], until: Optional[float],
          threshold: float, previous: Optional[Sequence[Optional[Record]]] = None) -> Record:
    """Every metric row per bucket: values, sparkline, slope per bucket, change first→last bucket; and the rows that
    regressed against `previous` (the range before), each pointed at the push window where it started."""
    grouped = buckets(measures_list, by, since, until)
    per_bucket = [dict(metric_rows(summarize(ms))) if ms else {} for _, ms in grouped]
    flagged: List[Record] = []
    if previous:
        chronological = sorted([m for m in list(previous) + list(measures_list) if m], key=lambda m: m["start"])
        flagged = [_flag(name, old, new, delta, chronological)
                   for name, old, new, delta in regressions(measures_list, previous, threshold)]
    return {"by": by, "buckets": [bucket_label(key, by) for key, _ in grouped],
            "runs": [len(ms) for _, ms in grouped], "rows": _row_table(per_bucket), "regressions": flagged}
