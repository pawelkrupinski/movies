"""A run as flat JSON events for VictoriaLogs and back (scripts/ci/ci_trends.py's `export`; the schema is in
docs/investigations/2026-11-01-twenty-runners.md): the run, job, step, jvm, cache, suite/test and process events of a
record, the POST that sends them, the LogsQL query that reads them, and the records rebuilt from what it returns.

    from trend_events import events_of, records_from_events, post_events, vl_query, commit_change
"""
from __future__ import annotations

import json
import re
import urllib.parse
import urllib.request
from typing import Any, Callable, Dict, Iterable, List, Optional, Tuple, cast

from trend_format import mins
from trend_github import GitHub
from trend_measures import (SETUP_CATEGORIES, bound_by, categorize, gc_summary, job_categories, job_resources,
                            job_steps, run_measures, top_processes)
from trend_records import RECORD_VERSION, Record, cache_figures, iso, job_group, job_leg, listing, section

VL_STREAM_FIELDS = "source,repo,workflow"
Opener = Callable[..., Any]
JobKey = Tuple[int, Any]


def _flat(prefix: str, figures: Optional[Dict[str, Any]]) -> Record:
    return {prefix + key: value for key, value in (figures or {}).items() if isinstance(value, (int, float, str))}


def _run_event(record: Record, m: Record, common: Record) -> Record:
    sat = section(record, "sat")
    return dict(common, _time=iso(m["start"]), kind="run",
                _msg="%s run %s: wall %s, path queue %s + work %s" % (
                    record.get("wf"), record["id"], mins(m["wall"]), mins(m["path_queue"]), mins(m["path_work"])),
                wall_s=round(m["wall"], 1), runner_min=round(m["runner_min"], 2), peak_jobs=m["peak"],
                path_queue_s=round(m["path_queue"], 1), path_work_s=round(m["path_work"], 1),
                critical_path=" → ".join(hop[0] for hop in m["path"]), sat_mean=sat.get("mean"),
                sat_peak=sat.get("peak"), sat_full=sat.get("full"), jobs=len(record["jobs"]),
                skewed_jobs=m["skewed_jobs"], skipped_jobs=len(record.get("skipped") or []),
                skipped=", ".join(record.get("skipped") or [])[:2000], start=m["start"], end=m["end"],
                **_flat("change_", record.get("change")))


def _job_event(job: Record, base: Record, resources: Optional[Record]) -> Record:
    categories = job_categories(job)
    gc = gc_summary(job)
    bound, notes = bound_by(resources, gc)
    setup = sum(v for k, v in categories.items() if k in SETUP_CATEGORIES)
    rate = section(job, "rate")
    event = dict(base, _time=iso(job["s"]), kind="job",
                 _msg="%s: %s (queue %s, setup %s)%s" % (job["n"], mins(job["e"] - job["s"]),
                                                         mins(max(0, job["s"] - job["r"])), mins(setup),
                                                         (" — " + ", ".join(notes)) if notes else ""),
                 conclusion=job["c"], duration_s=round(job["e"] - job["s"], 1),
                 queue_s=round(max(0.0, job["s"] - job["r"]), 1), skew_s=job.get("skew"),
                 runner_name=job.get("runner"), setup_s=round(setup, 1),
                 work_s=round(sum(categories.values()) - setup, 1), bound=bound,
                 start=job["s"], end=job["e"], runnable=job["r"],
                 rate_start=rate.get("start"), rate_end=rate.get("end"))
    event.update(_flat("machine_", job.get("machine")))
    event.update(_flat("res_", resources))
    if gc:
        event.update(_flat("gc_", {k: v for k, v in gc.items() if k != "flags"}))
    event.update(_flat("tests_", job.get("tests")))
    return event


def _step_events(job: Record, base: Record, step_resources: Dict[str, Record]) -> List[Record]:
    events: List[Record] = []
    for index, (name, offset, seconds) in enumerate(job_steps(job)):
        step = dict(base, _time=iso(job["s"] + offset), kind="step", _msg="%s / %s: %s" % (job["n"], name, mins(seconds)),
                    step=name, step_index=index, category=categorize(name), offset_s=offset, duration_s=seconds)
        step.update(_flat("res_", step_resources.get(name)))
        events.append(step)
    return events


def _jvm_events(job: Record, base: Record) -> List[Record]:
    return [dict(base, _time=iso(job["s"]), kind="jvm",
                 _msg="%s: JVM %s GC %.1f s (%s collections)" % (
                     job["n"], jvm.get("main") or jvm.get("pid"), (jvm.get("gc_pause_ms") or 0) / 1000.0,
                     jvm.get("gc_count")),
                 **{k: v for k, v in jvm.items() if isinstance(v, (int, float, str))})
            for jvm in listing(job, "jvms")]


def _cache_events(job: Record, base: Record) -> List[Record]:
    events: List[Record] = []
    for name, entry in section(job, "cache").items():
        cache = cache_figures(entry)
        events.append(dict(base, _time=iso(job["s"]), kind="cache", _msg="%s: cache %s %s" % (job["n"], name, cache.get("status")),
                           cache=name, family=re.sub(r"-[0-9a-f]{16,}$", "", cache.get("key") or name), **cache))
    return events


def _slow_events(job: Record, base: Record) -> List[Record]:
    return [dict(base, _time=iso(job["s"]), _msg="%s: %s %s %.1fs" % (
        job["n"], line.get("kind"), line.get("test") or line.get("suite"), line.get("time_s") or 0),
        **{k: v for k, v in line.items() if isinstance(v, (int, float, str))})
        for line in listing(job, "slow")]


def _process_events(job: Record, base: Record) -> List[Record]:
    return [dict(base, _time=iso(job["s"]), kind="process",
                 _msg="%s: %s peak %d MB, %.0f CPU-s" % (job["n"], proc["comm"], proc["rss_mb"], proc["cpu_s"]), **proc)
            for proc in top_processes(job)]


def _job_events(job: Record, common: Record) -> List[Record]:
    base = dict(common, job=job["n"], job_id=job.get("id"), group=job_group(job["n"]),
                leg=job_leg(job["n"], job.get("label")), label=job.get("label"))
    resources, step_resources = job_resources(job)
    return ([_job_event(job, base, resources)] + _step_events(job, base, step_resources) + _jvm_events(job, base)
            + _cache_events(job, base) + _slow_events(job, base) + _process_events(job, base))


def events_of(record: Record) -> List[Record]:
    """The run as flat JSON events (the doc's schema table): run, job, step, jvm, cache, suite/test, process."""
    if not record["jobs"]:
        return []
    common = {"source": "ci", "repo": record.get("repo"), "workflow": record.get("wf"),
              "workflow_file": (record.get("path") or "").rsplit("/", 1)[-1], "run_id": record["id"],
              "run_number": record.get("num"), "run_attempt": record.get("attempt"), "sha": record.get("sha"),
              "branch": record.get("branch"), "event": record.get("event"), "run_conclusion": record.get("concl")}
    events = [_run_event(record, cast(Record, run_measures(record)), common)]
    for job in record["jobs"]:
        events += _job_events(job, common)
    return events


def commit_change(github: GitHub, sha: Optional[str]) -> Optional[Record]:
    """The commit's size: files changed, lines added and deleted (one cached API call; None when unreadable)."""
    if not sha or not hasattr(github, "cached"):
        return None
    try:
        commit = github.cached("commit-%s.json" % sha, lambda: github.api("commits/%s" % sha))
    except Exception:  # noqa: BLE001 -- a size is context, never a reason to drop the run
        return None
    stats = section(commit, "stats")
    return {"files": len(commit.get("files") or []), "additions": stats.get("additions"),
            "deletions": stats.get("deletions"), "parents": len(commit.get("parents") or [])}


def post_events(url: str, events: Iterable[Record], opener: Opener = urllib.request.urlopen) -> None:
    body = "".join(json.dumps(event, separators=(",", ":"), default=str) + "\n" for event in events).encode()
    query = urllib.parse.urlencode({"_stream_fields": VL_STREAM_FIELDS, "_time_field": "_time", "_msg_field": "_msg"})
    request = urllib.request.Request(url.rstrip("/") + "/insert/jsonline?" + query, data=body, method="POST",
                                     headers={"Content-Type": "application/stream+json"})
    with opener(request, timeout=60) as response:
        if response.status >= 300:
            raise RuntimeError("VictoriaLogs answered HTTP %s" % response.status)


def vl_query(url: str, query: str, start: Optional[float] = None, end: Optional[float] = None, limit: int = 200000,
             opener: Opener = urllib.request.urlopen) -> List[Record]:
    """LogsQL over VictoriaLogs' /select/logsql/query: one JSON object per line."""
    form = {"query": query, "limit": str(limit)}
    if start is not None:
        form["start"] = iso(start)
    if end is not None:
        form["end"] = iso(end)
    request = urllib.request.Request(url.rstrip("/") + "/select/logsql/query",
                                     data=urllib.parse.urlencode(form).encode(), method="POST")
    with opener(request, timeout=120) as response:
        text = response.read().decode(errors="replace")
    return [json.loads(line) for line in text.splitlines() if line.strip().startswith("{")]


def _number(value: Any) -> Optional[float]:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def _numbers(event: Record, prefix: str) -> Dict[str, Optional[float]]:
    """The event's `<prefix>*` fields as numbers, keyed without the prefix."""
    return {k[len(prefix):]: _number(v) for k, v in event.items() if k.startswith(prefix)}


def _job_of(jobs: Dict[JobKey, Record], event: Record, run_id: int) -> Record:
    return jobs.setdefault((run_id, event.get("job_id") or event.get("job")), {"st": [], "run": run_id})


def _fold_run(runs: Dict[int, Record], jobs: Dict[JobKey, Record], event: Record, run_id: int) -> None:
    runs[run_id] = {"v": RECORD_VERSION, "repo": event.get("repo"), "wf": event.get("workflow"),
                    "path": ".github/workflows/" + (event.get("workflow_file") or ""), "id": run_id,
                    "num": _number(event.get("run_number")), "attempt": _number(event.get("run_attempt")),
                    "branch": event.get("branch"), "sha": event.get("sha"), "event": event.get("event"),
                    "concl": event.get("run_conclusion"), "created": None, "jobs": [],
                    "sat": {"mean": _number(event.get("sat_mean")), "peak": _number(event.get("sat_peak")),
                            "full": _number(event.get("sat_full"))} if event.get("sat_mean") else None}


def _fold_job(runs: Dict[int, Record], jobs: Dict[JobKey, Record], event: Record, run_id: int) -> None:
    job = _job_of(jobs, event, run_id)
    job.update({"id": event.get("job_id"), "n": event.get("job"), "c": event.get("conclusion"),
                "s": _number(event.get("start")), "e": _number(event.get("end")),
                "r": _number(event.get("runnable")), "skew": _number(event.get("skew_s")) or 0,
                "label": event.get("label") or None, "runner": event.get("runner_name"),
                "machine": {k[8:]: v for k, v in event.items() if k.startswith("machine_")}})
    job["res_sum"] = _numbers(event, "res_") or None
    job["gc_sum"] = _numbers(event, "gc_") or None
    tests = _numbers(event, "tests_")
    if tests:
        job["tests"] = tests


def _fold_step(runs: Dict[int, Record], jobs: Dict[JobKey, Record], event: Record, run_id: int) -> None:
    job = _job_of(jobs, event, run_id)
    job["st"].append([event.get("step"), _number(event.get("offset_s")) or 0.0,
                      _number(event.get("duration_s")) or 0.0, _number(event.get("step_index")) or 0])
    res = _numbers(event, "res_")
    if res:
        job.setdefault("step_res", {})[event.get("step")] = res


def _fold_cache(runs: Dict[int, Record], jobs: Dict[JobKey, Record], event: Record, run_id: int) -> None:
    _job_of(jobs, event, run_id).setdefault("cache", {})[event.get("cache")] = {
        "status": event.get("status"), "key": event.get("key")}


_FOLDERS: Dict[str, Callable[[Dict[int, Record], Dict[JobKey, Record], Record, int], None]] = {
    "run": _fold_run, "job": _fold_job, "step": _fold_step, "cache": _fold_cache}


def _attach_jobs(runs: Dict[int, Record], jobs: Dict[JobKey, Record]) -> None:
    """Each rebuilt job that started, on its run, with its steps in order."""
    for job in jobs.values():
        if job.get("s") is None or job["run"] not in runs:
            continue
        job["st"] = [step[:3] for step in sorted(job["st"], key=lambda step: step[3])]
        if job.get("res_sum") is None:
            job.pop("res_sum", None)
        runs[job.pop("run")]["jobs"].append(job)


def records_from_events(events: Iterable[Record]) -> List[Record]:
    """Rebuild run records from VictoriaLogs' events (strings everywhere: VictoriaLogs stores every field as text)."""
    runs: Dict[int, Record] = {}
    jobs: Dict[JobKey, Record] = {}
    for event in events:
        run_id = int(float(event.get("run_id") or 0))
        fold = _FOLDERS.get(event.get("kind") or "")
        if fold:
            fold(runs, jobs, event, run_id)
    _attach_jobs(runs, jobs)
    return list(runs.values())
