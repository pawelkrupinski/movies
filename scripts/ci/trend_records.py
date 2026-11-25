"""A CI run as the trend tools read it (scripts/ci/ci_trends.py's header explains the timestamps): instants, one API
job as a record whose window is read off its STEPS, the job's metrics lines folded in, and the run around them.

    from trend_records import epoch, iso, parse_when, job_record, run_record, job_group, job_leg
"""
from __future__ import annotations

import datetime
import math
import re
import statistics
from typing import Any, Callable, Dict, List, Mapping, Optional, Sequence, cast

Record = Dict[str, Any]

RECORD_VERSION = 1

# A job name's caller prefixes (`ci / `, `leg (germany) / `, `timings / `), then its matrix suffix.
CALLER_PREFIX = re.compile(r"^(?:[^()/]+(?:\([^)]*\))? / )+")
LEG = re.compile(r"^leg \(([^)]+)\) / ")
MATRIX = re.compile(r" \(.*\)$")
_INSTANT = re.compile(r"^(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)(?:\.(\d+))?(Z|[+-]\d\d:\d\d)?$")
_UNIT_SECONDS = {"w": 604800, "d": 86400, "h": 3600, "m": 60}


def section(record: Mapping[str, Any], key: str) -> Record:
    """`record[key]` as a dict; an empty one when it is absent or empty."""
    found: Record = record.get(key) or {}
    return found


def listing(record: Mapping[str, Any], key: str) -> List[Any]:
    """`record[key]` as a list; an empty one when it is absent or empty."""
    found: List[Any] = record.get(key) or []
    return found


def as_record(value: Any) -> Optional[Record]:
    """`value` when it is a dict, else None."""
    return cast(Record, value) if isinstance(value, dict) else None


def cache_figures(value: Any) -> Record:
    """A job's cache entry as figures: older records held the bare status."""
    figures = as_record(value)
    return {"status": value} if figures is None else figures


def epoch(text: Optional[str]) -> Optional[float]:
    """An ISO-8601 instant (`Z`, any fraction) as UTC epoch seconds; None for none."""
    if not text:
        return None
    text = text.strip()
    match = _INSTANT.match(text)
    if not match:
        raise ValueError("not an instant: %r" % text)
    base = datetime.datetime.strptime(match.group(1), "%Y-%m-%dT%H:%M:%S")
    zone = match.group(3) or "Z"
    offset = 0 if zone == "Z" else (int(zone[1:3]) * 3600 + int(zone[4:6]) * 60) * (1 if zone[0] == "+" else -1)
    fraction = float("0." + match.group(2)) if match.group(2) else 0.0
    return base.replace(tzinfo=datetime.timezone.utc).timestamp() - offset + fraction


def instant(text: str) -> float:
    """`epoch` of an instant that is there."""
    seconds = epoch(text)
    if seconds is None:
        raise ValueError("not an instant: %r" % text)
    return seconds


def iso(seconds: float) -> str:
    return datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def parse_when(text: str, now: Optional[float] = None) -> float:
    """`2026-11-01`, `2026-11-01T10:00:00Z`, `7d`/`12h`/`30m` ago, or `now`, as epoch seconds."""
    now = datetime.datetime.now(datetime.timezone.utc).timestamp() if now is None else now
    text = text.strip()
    if text == "now":
        return now
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([wdhm])", text)
    if match:
        return now - float(match.group(1)) * _UNIT_SECONDS[match.group(2)]
    if re.fullmatch(r"\d{4}-\d\d-\d\d", text):
        text += "T00:00:00Z"
    return instant(text)


def skew_of(job: Record) -> int:
    """How far GitHub's own timestamps for this job are off, in seconds, judged against its steps (see the header):
    started_at against the first step's start, rounded to the quarter hour once it is ten minutes or more; else a
    created_at AFTER the first step began (impossible) is pulled back by whole quarter hours. 0 when they agree."""
    starts = [instant(step["started_at"]) for step in listing(job, "steps") if step.get("started_at")]
    if not starts:
        return 0
    first = min(starts)
    started = epoch(job.get("started_at"))
    if started is not None and abs(started - first) >= 600:
        return int(round((started - first) / 900.0) * 900)
    created = epoch(job.get("created_at"))
    if created is not None and created - first > 60:
        return int(math.ceil((created - first) / 900.0) * 900)
    return 0


def _ran_steps(job: Record) -> List[Record]:
    return [step for step in listing(job, "steps") if step.get("started_at") and step.get("completed_at")
            and step.get("conclusion") != "skipped"]


def job_record(job: Record) -> Optional[Record]:
    """One job, from the API: its window from its STEPS, runnable at created_at less its skew. None when it never ran
    a step (skipped, or cancelled while queued)."""
    steps = _ran_steps(job)
    if not steps:
        return None
    start = min(instant(step["started_at"]) for step in steps)
    end = max(instant(step["completed_at"]) for step in steps)
    skew = skew_of(job)
    created = epoch(job.get("created_at"))
    runnable = created - skew if created is not None else start
    return {
        "id": job.get("id"), "n": job["name"], "c": job.get("conclusion") or "unknown",
        "r": round(min(runnable, start), 1), "s": round(start, 1), "e": round(end, 1), "skew": skew,
        "st": [[step["name"], round(instant(step["started_at"]) - start, 1),
                round(instant(step["completed_at"]) - instant(step["started_at"]), 1)] for step in steps],
    }


def job_group(name: str) -> str:
    """`ci / page tests (safari / pw phones 1/2)` → `page tests`; `leg (germany) / takes-drift-1 (germany)` →
    `takes-drift-1`: what a job is, across its matrix rows and callers."""
    group = MATRIX.sub("", CALLER_PREFIX.sub("", name)).strip()
    # the page tests were `page tests (<engine> / <variant>)` until 2026-11-01 (now `page-tests-<engine> (<variant>)`): one group
    return "page tests" if re.fullmatch(r"page-tests-(safari|chrome)", group) else group


def job_leg(name: str, label: Optional[str] = None) -> Optional[str]:
    """A convergence row's country (its caller row, `leg (<country>) / ...`), or the telemetry label it was given."""
    match = LEG.match(name)
    return match.group(1) if match else (label or None)


def series_dicts(line: Mapping[str, Any]) -> List[Dict[str, Any]]:
    """A `series` metrics line (columns + rows) as a list of dicts of numbers."""
    columns = listing(line, "columns")
    return [{name: value for name, value in zip(columns, row) if isinstance(value, (int, float))}
            for row in listing(line, "rows")]


def _without_kind(line: Mapping[str, Any]) -> Record:
    return {key: value for key, value in line.items() if key != "kind"}


def _machine(job: Record, line: Record) -> None:
    job["machine"] = {key: value for key, value in line.items() if key != "kind" and value not in ("", None)}


def _job_start(job: Record, line: Record) -> None:
    if line.get("label"):
        job["label"] = line["label"]
    if line.get("rate_remaining") is not None:
        job.setdefault("rate", {})["start"] = line["rate_remaining"]


def _cache(job: Record, line: Record) -> None:
    job.setdefault("cache", {})[line.get("cache") or "cache"] = {
        key: line.get(key) for key in ("status", "key", "size_mb", "restore_s")}


def _rate_limit(job: Record, line: Record) -> None:
    job.setdefault("rate", {})["end"] = line.get("remaining")


def _series(job: Record, line: Record) -> None:
    job["series"] = series_dicts(line)


def _procs(job: Record, line: Record) -> None:
    job["procs"] = {"columns": line.get("columns"), "rows": line.get("rows")}


def _jvm(job: Record, line: Record) -> None:
    job.setdefault("jvms", []).append(_without_kind(line))


def _tests(job: Record, line: Record) -> None:
    job["tests"] = _without_kind(line)


def _slow(job: Record, line: Record) -> None:
    job.setdefault("slow", []).append(line)


# What each kind of metrics line (job_metrics.py) adds to the job's record.
_ATTACHERS: Dict[str, Callable[[Record, Record], None]] = {
    "machine": _machine, "job_start": _job_start, "rate_limit": _rate_limit, "series": _series, "procs": _procs,
    "jvm": _jvm, "cache": _cache, "tests": _tests, "suite": _slow, "test": _slow,
}


def attach_metrics(job: Record, lines: Sequence[Record]) -> Record:
    """Fold a job's metrics lines (job_metrics.py) into its record."""
    for line in lines:
        attach = _ATTACHERS.get(line.get("kind") or "")
        if attach:
            attach(job, line)
    return job


def metrics_for(record: Record, runner: Optional[str], metrics: Optional[Mapping[str, Sequence[Record]]]
                ) -> Optional[Sequence[Record]]:
    """The metrics lines of the job `record` ran on `runner`. One runner serves several jobs of a run (the landing check's
    `plan`, a heavy row and an identity shard all on ci-runner-movies-check-5, run 41604183761), so the runner's name
    alone attached one job's figures to all three: the set is the one whose `job_start` falls in this job's window."""
    if not runner or not metrics:
        return None
    for lines in metrics.values():
        start = next((line for line in lines if line.get("kind") == "job_start"), None)
        if start is None or start.get("runner_name") != runner:
            continue
        if start.get("t") is None or record["s"] - 60 <= start["t"] <= record["e"]:
            return lines
    return None


def _job_records(jobs: Sequence[Record], metrics: Optional[Mapping[str, Sequence[Record]]]) -> List[Record]:
    records: List[Record] = []
    for job in jobs:
        record = job_record(job)
        if record is None:
            continue
        record["runner"] = job.get("runner_name")
        lines = metrics_for(record, job.get("runner_name"), metrics)
        if lines:
            attach_metrics(record, lines)
        records.append(record)
    return records


def run_record(run: Record, jobs: Sequence[Record], metrics: Optional[Mapping[str, Sequence[Record]]] = None
               ) -> Record:
    """One run as the analysis reads it (see the header). `metrics` maps each metrics artifact's name (older caches: a
    runner name) to its lines; a job takes the set from its runner that STARTED inside its window (metrics_for)."""
    records = _job_records(jobs, metrics)
    skews = [job["skew"] for job in records if job["skew"]]
    created = epoch(run.get("created_at"))
    if created is not None and skews:
        created -= statistics.median(skews)
    return {
        "v": RECORD_VERSION, "repo": section(run, "repository").get("full_name"), "wf": run.get("name"),
        "path": run.get("path"), "id": run.get("id"), "num": run.get("run_number"), "attempt": run.get("run_attempt"),
        "branch": run.get("head_branch"), "sha": run.get("head_sha"), "event": run.get("event"),
        "concl": run.get("conclusion"), "created": round(created, 1) if created else None, "jobs": records,
        "skipped": sorted(job["name"] for job in jobs if job.get("conclusion") == "skipped"),
    }
