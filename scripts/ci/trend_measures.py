"""What one run contributes to an aggregate (scripts/ci/ci_trends.py's header explains the critical path and the slot
saturation): a step's category, a job's setup against work, the critical path, the resource figures of a window, the GC
and the largest processes, what a job was bound by, and `run_measures` gathering them.

    from trend_measures import run_measures, categorize, critical_path, saturation, bound_by
"""
from __future__ import annotations

import re
import statistics
from typing import Any, Dict, Iterable, Iterator, List, Optional, Sequence, Tuple, cast

from trend_records import Record, cache_figures, job_group, job_leg, listing, section

SLOTS = 20  # movies' runner count; --slots / CI_TIMINGS_SLOTS overrides it (bitcashier's collector passes 12)
SATURATION_SAMPLE_SECONDS = 10

# A step's category, first match wins. A composite action is one API step; the record's log steps (its inner steps,
# from the job's `##[group]Run` lines) are classified instead when the record has them.
CATEGORIES = [
    ("runner", r"^Set up job$|^Complete job$|^Post job cleanup|^\(runner\)$"),
    ("publish", r"upload-artifact|junit-report|Publish|publish|imagetools|Record that|Mark deploy|report-timings"
                r"|Hand the staged|push.*image|image was built|Report |report \(|verdict"),
    ("services", r"[Mm]ongo|docker|buildx|setup-buildx|[Bb]rowser|playwright install|install-playwright|setup-chrome"
                 r"|FixtureServer|fixture-server|[Bb]oot |APT_CACHE|apt-get|Android SDK|setup-android|setup-swift|Install Swift"
                 r"|Gradle|cgroup-sampler|cpu-sampler|in-background.sh start"),
    ("fixtures", r"[Ff]ixture|corpus|[Rr]ecording|enrichment|convergence-setup|download-artifact|Fetch the"),
    ("compile", r"compile|[Ss]tage\b|sbt .*stage|web/stage|Key the sbt"),
    ("toolchain/cache", r"setup-sbt-build|setup-jdk|setup-java|setup-sbt|setup-node|sbt-target-cache|actions/cache"
                        r"|[Cc]ache|npm ci|warm-browsers|sbt-guard|SBT_TOOLPATH|JDK_VERSION|Set up "),
    ("checkout", r"actions/checkout|[Cc]heck out|git clone|sparse-checkout"),
    ("tests", r"sbt |test|Test|itAll|e2e|run-page-test|[Pp]laywright|[Rr]eplay|[Dd]rift|[Cc]onvergence|[Oo]rder"
              r"|[Tt]akes|LocalServer|lint|[Cc]heck|[Vv]erify|hook|PREPUSH|[Ss]pec"),
]
CATEGORY_ORDER = ["runner", "checkout", "toolchain/cache", "compile", "services", "fixtures", "tests", "publish",
                  "other"]
SETUP_CATEGORIES = {"runner", "checkout", "toolchain/cache", "compile", "services", "fixtures"}
_COMPILED = [(name, re.compile(pattern)) for name, pattern in CATEGORIES]


def categorize(name: str) -> str:
    stripped = re.sub(r"^Post (Run )?", "", re.sub(r"^Run ", "", name))
    for category, pattern in _COMPILED:
        if pattern.search(stripped) or (category == "runner" and pattern.search(name)):
            return category
    return "other"


def job_steps(job: Record) -> List[List[Any]]:
    """The job's steps as [name, offset into the job, seconds], from the API."""
    return job.get("st") or []


def job_categories(job: Record) -> Dict[str, float]:
    totals: Dict[str, float] = {}
    for name, _, seconds in job_steps(job):
        category = categorize(name)
        totals[category] = totals.get(category, 0.0) + seconds
    gap = (job["e"] - job["s"]) - sum(totals.values())
    if gap > 1:
        totals["runner"] = totals.get("runner", 0.0) + gap
    return totals


def run_window(record: Record) -> Tuple[Optional[float], Optional[float]]:
    jobs = record["jobs"]
    return (min(job["s"] for job in jobs), max(job["e"] for job in jobs)) if jobs else (None, None)


def peak_concurrency(intervals: Sequence[Tuple[float, float]]) -> int:
    events = sorted([(start, 1) for start, _ in intervals] + [(end, -1) for _, end in intervals],
                    key=lambda event: (event[0], event[1]))
    running = peak = 0
    for _, delta in events:
        running += delta
        peak = max(peak, running)
    return peak


def critical_path(record: Record) -> List[Tuple[str, float, float]]:
    """[(job name, queue seconds, own seconds)] from the run's start to its last job's end (see the header)."""
    jobs = sorted(record["jobs"], key=lambda job: job["e"])
    if not jobs:
        return []
    start = min(job["s"] for job in jobs)
    path: List[Tuple[str, float, float]] = []
    current: Optional[Record] = jobs[-1]
    seen: set[int] = set()
    while current is not None and id(current) not in seen:
        seen.add(id(current))
        path.append((current["n"], max(0.0, current["s"] - current["r"]), current["e"] - current["s"]))
        if current["r"] <= start + 10:
            break
        before = [job for job in jobs if job["e"] <= current["r"] + 5 and id(job) not in seen]
        current = max(before, key=lambda job: job["e"]) if before else None
    return list(reversed(path))


def _sample_times(start: float, end: float, every: float) -> Iterator[float]:
    t = start
    while t < end:
        yield t
        t += every


def _busy_at(intervals: Sequence[Tuple[float, float]], t: float) -> int:
    return sum(1 for s, e in intervals if s <= t < e)


def saturation(window: Tuple[Optional[float], Optional[float]], intervals: Iterable[Tuple[float, float]],
               slots: int = SLOTS, every: float = SATURATION_SAMPLE_SECONDS) -> Optional[Dict[str, float]]:
    """Busy slots over `window` from every (start, end) in `intervals` (all the repository's jobs): mean, peak, and the
    share of samples at all `slots`."""
    start, end = window
    if start is None or end is None or end <= start:
        return None
    relevant = [(s, e) for s, e in intervals if e > start and s < end]
    samples = [_busy_at(relevant, t) for t in _sample_times(start, end, every)]
    return {"mean": round(statistics.mean(samples), 1), "peak": max(samples),
            "full": round(sum(1 for value in samples if value >= slots) / len(samples), 3)}


def _column(rows: Sequence[Record], name: str) -> List[Any]:
    return [row[name] for row in rows if name in row]


def _worst(pick: Any, values: Sequence[float], scale: float = 1.0) -> Optional[int]:
    return round(pick(values) / scale) if values else None


def _delta(first: Record, last: Record, column: str) -> float:
    return last.get(column, 0) - first.get(column, 0)


def _cpu_shares(first: Record, last: Record) -> Record:
    ticks = max(_delta(first, last, "cpu_total"), 1e-9)
    return {"cpu": round(_delta(first, last, "cpu_busy") / ticks, 3),
            "user": round(_delta(first, last, "cpu_user") / ticks, 3),
            "sys": round(_delta(first, last, "cpu_sys") / ticks, 3),
            "iowait": round(_delta(first, last, "cpu_iowait") / ticks, 3),
            "steal": round(_delta(first, last, "cpu_steal") / ticks, 3)}


def _load(inside: Sequence[Record]) -> Record:
    loads, queue = _column(inside, "load1"), _column(inside, "procs_running")
    return {"load_mean": round(statistics.mean(loads), 2) if loads else None, "load_max": max(loads) if loads else None,
            "runq_mean": round(statistics.mean(queue), 2) if queue else None,
            "runq_max": max(queue) if queue else None}


def _memory(inside: Sequence[Record]) -> Record:
    used = [row["mem_total_kb"] - row["mem_avail_kb"] for row in inside if "mem_total_kb" in row and "mem_avail_kb" in row]
    swap = [row["swap_total_kb"] - row["swap_free_kb"] for row in inside if "swap_total_kb" in row and "swap_free_kb" in row]
    return {"mem_used_max_mb": _worst(max, used, 1024), "mem_avail_min_mb": _worst(min, _column(inside, "mem_avail_kb"), 1024),
            "swap_used_max_mb": _worst(max, swap, 1024)}


def _pressure(first: Record, last: Record, seconds: float) -> Record:
    def psi(column: str) -> Optional[float]:
        if column not in first or column not in last:
            return None
        return round((last[column] - first[column]) / (seconds * 1e6), 3)
    return {"psi_cpu_some": psi("psi_cpu_some_us"), "psi_mem_some": psi("psi_mem_some_us"),
            "psi_mem_full": psi("psi_mem_full_us"), "psi_io_some": psi("psi_io_some_us"),
            "psi_io_full": psi("psi_io_full_us")}


def _traffic(inside: Sequence[Record], first: Record, last: Record) -> Record:
    return {
        "disk_read_mb": round(_delta(first, last, "disk_read_sectors") * 512 / 1e6),
        "disk_write_mb": round(_delta(first, last, "disk_write_sectors") * 512 / 1e6),
        "disk_free_min_mb": _worst(min, _column(inside, "disk_free_kb"), 1024),
        "net_rx_mb": round(_delta(first, last, "net_rx_bytes") / 1e6, 1),
        "net_tx_mb": round(_delta(first, last, "net_tx_bytes") / 1e6, 1),
        "open_files_max": max(_column(inside, "open_files"), default=None),
        "oom_kills": int(_delta(first, last, "oom_kill")),
    }


def window_resources(series: Sequence[Record], start: float, end: float) -> Optional[Record]:
    """The resource figures of `series` (parse_series) between two instants: CPU shares of all cores, PSI shares of
    the wall time, memory and swap at their worst, disk MB, OOM kills. None without two samples in reach."""
    inside = [row for row in series if start - 6 <= row.get("t", 0) <= end + 6]
    if len(inside) < 2:
        return None
    first, last = inside[0], inside[-1]
    seconds = max(last["t"] - first["t"], 1e-9)
    return {**_cpu_shares(first, last), **_load(inside), **_memory(inside), **_pressure(first, last, seconds),
            **_traffic(inside, first, last)}


def _total(jvms: Sequence[Record], key: str) -> Any:
    return sum(jvm.get(key) or 0 for jvm in jvms)


def _largest(jvms: Sequence[Record], key: str) -> Any:
    return max(jvm.get(key) or 0 for jvm in jvms)


def gc_summary(job: Record) -> Optional[Record]:
    """The job's JVMs together: collections, total and max pause, GC share of the job's wall, the largest after-GC
    heap, and each long-lived JVM's flags. Precomputed (`gc_sum`) on a record rebuilt from VictoriaLogs."""
    if job.get("gc_sum"):
        return job["gc_sum"]
    jvms = listing(job, "jvms")
    if not jvms:
        return None
    wall = max(job["e"] - job["s"], 1e-9)
    pause = _total(jvms, "gc_pause_ms") / 1000.0
    overheads = [jvm["gc_overhead"] for jvm in jvms if jvm.get("gc_overhead") is not None and (jvm.get("uptime_s") or 0) >= 30]
    return {"jvms": len(jvms), "collections": _total(jvms, "gc_count"), "pause_s": round(pause, 1),
            "max_pause_ms": _largest(jvms, "gc_max_pause_ms"), "heap_after_max_mb": _largest(jvms, "heap_after_max_mb"),
            "of_wall": round(pause / wall, 3), "worst_jvm_overhead": round(max(overheads), 3) if overheads else None}


def job_resources(job: Record) -> Tuple[Optional[Record], Dict[str, Record]]:
    """The job's resources over its own window, and per step (≥15 s) by the step's window. Precomputed (`res_sum`,
    `step_res`) on a record rebuilt from VictoriaLogs."""
    if job.get("res_sum") is not None:
        return job["res_sum"], job.get("step_res") or {}
    series = job.get("series")
    if not series:
        return None, {}
    whole = window_resources(series, job["s"], job["e"])
    steps: Dict[str, Record] = {}
    for name, offset, seconds in job_steps(job):
        if seconds < 15:
            continue
        figures = window_resources(series, job["s"] + offset, job["s"] + offset + seconds)
        if figures:
            steps[name] = figures
    return whole, steps


def _fold_process(by_comm: Dict[str, Record], last_ticks: Dict[str, float], row: Sequence[Any],
                  index: Dict[str, int]) -> None:
    comm = row[index["comm"]]
    entry = by_comm.setdefault(comm, {"comm": comm, "rss_mb": 0, "cpu_s": 0.0, "threads": 0, "procs": 0})
    entry["rss_mb"] = max(entry["rss_mb"], round((row[index["rss_kb"]] or 0) / 1024))
    entry["threads"] = max(entry["threads"], row[index["threads"]] or 0)
    entry["procs"] = max(entry["procs"], row[index["procs"]] or 0)
    ticks = row[index["ticks"]] or 0
    if comm in last_ticks and ticks > last_ticks[comm]:
        entry["cpu_s"] += (ticks - last_ticks[comm]) / 100.0
    last_ticks[comm] = ticks


def top_processes(job: Record, top: int = 5) -> List[Record]:
    """The commands with the largest RSS over the job: peak RSS (MB), CPU seconds, peak threads, peak processes."""
    procs = section(job, "procs")
    columns, rows = listing(procs, "columns"), listing(procs, "rows")
    if not columns:
        return []
    index = {name: columns.index(name) for name in columns}
    by_comm: Dict[str, Record] = {}
    last_ticks: Dict[str, float] = {}
    for row in rows:
        _fold_process(by_comm, last_ticks, row, index)
    for entry in by_comm.values():
        entry["cpu_s"] = round(entry["cpu_s"], 1)
    return sorted(by_comm.values(), key=lambda entry: -entry["rss_mb"])[:top]


def _memory_finding(resources: Record, notes: List[str], verdict: Optional[str]) -> Optional[str]:
    full, some = resources.get("psi_mem_full") or 0, resources.get("psi_mem_some") or 0
    if full >= 0.01 or some >= 0.10 or (resources.get("swap_used_max_mb") or 0) >= 256:
        notes.append("PSI mem full %.0f%% some %.0f%%" % (full * 100, some * 100))
        verdict = verdict or "memory"
    if (resources.get("oom_kills") or 0) > 0:
        notes.append("%d OOM kill(s)" % resources["oom_kills"])
        verdict = "memory"
    return verdict


def _io_finding(resources: Record, notes: List[str], verdict: Optional[str]) -> Optional[str]:
    if (resources.get("psi_io_full") or 0) >= 0.05 or (resources.get("iowait") or 0) >= 0.10:
        notes.append("io full %.0f%% iowait %.0f%%" % ((resources.get("psi_io_full") or 0) * 100,
                                                      (resources.get("iowait") or 0) * 100))
        verdict = verdict or "io"
    return verdict


def _resource_findings(resources: Record, notes: List[str], verdict: Optional[str]) -> Optional[str]:
    verdict = _io_finding(resources, notes, _memory_finding(resources, notes, verdict))
    if (resources.get("steal") or 0) >= 0.05:
        notes.append("steal %.0f%%" % (resources["steal"] * 100))
    cpu = resources.get("cpu") or 0
    notes.append("CPU %.0f%%" % (cpu * 100))
    if verdict is None:
        verdict = "cpu" if cpu >= 0.85 else ("waiting" if cpu < 0.35 else "mixed")
    return verdict


def bound_by(resources: Optional[Record], gc: Optional[Record]) -> Tuple[Optional[str], List[str]]:
    """What a job's wall time was bound by, from its resource figures and GC: the label `report` prints beside it."""
    notes: List[str] = []
    verdict: Optional[str] = None
    if gc and gc.get("of_wall") is not None and gc["of_wall"] >= 0.05:
        notes.append("GC %.0f%% of wall" % (gc["of_wall"] * 100))
        verdict = "memory/GC"
    if resources:
        verdict = _resource_findings(resources, notes, verdict)
    return verdict, notes


def run_change(record: Record) -> str:
    """What kind of change a landing check ran on: `identity` when any of its jobs is an identity check, else `other`
    (the plan selects the identity shards only for a change that can touch them)."""
    return "identity" if any("identity" in job["n"] for job in record["jobs"]) else "other"


def _queued(job: Record) -> float:
    """How long the job waited for a runner."""
    return max(0.0, job["s"] - job["r"])


def _job_measures(job: Record) -> Record:
    resources, step_resources = job_resources(job)
    caches = {name: cache_figures(value).get("status") if isinstance(value, dict) else value
              for name, value in section(job, "cache").items()}
    return {
        "dur": job["e"] - job["s"], "queue": _queued(job), "concl": job["c"],
        "start": job["s"], "end": job["e"], "machine": job.get("machine") or {},
        "leg": job_leg(job["n"], job.get("label")), "group": job_group(job["n"]), "label": job.get("label"),
        "steps": {name: seconds for name, _, seconds in job_steps(job)}, "categories": job_categories(job),
        "res": resources, "step_res": step_resources, "gc": gc_summary(job), "cache": caches,
        "tests": job.get("tests"), "procs": top_processes(job),
    }


def _run_totals(record: Record) -> Record:
    jobs = record["jobs"]
    start, end = cast(Tuple[float, float], run_window(record))
    path = critical_path(record)
    return {
        "id": record["id"], "num": record.get("num"), "sha": record.get("sha"), "branch": record.get("branch"),
        "concl": record.get("concl"), "start": start, "end": end, "wall": end - start,
        "runner_min": sum(job["e"] - job["s"] for job in jobs) / 60.0,
        "peak": peak_concurrency([(job["s"], job["e"]) for job in jobs]),
        "path": path, "path_queue": sum(hop[1] for hop in path), "path_work": sum(hop[2] for hop in path),
        "queue_total": sum(_queued(job) for job in jobs),
        "elapsed": end - min(start, record.get("created") or start),
        "change": run_change(record),
        "sat": record.get("sat"), "skewed_jobs": sum(1 for job in jobs if job.get("skew")),
        "jobs": {}, "categories": {}, "cache": {},
    }


def run_measures(record: Record) -> Optional[Record]:
    """Everything one run contributes to an aggregate."""
    if not record["jobs"]:
        return None
    measures = _run_totals(record)
    for job in record["jobs"]:
        entry = _job_measures(job)
        measures["jobs"].setdefault(job["n"], []).append(entry)
        for category, seconds in entry["categories"].items():
            measures["categories"][category] = measures["categories"].get(category, 0.0) + seconds
        for name, status in entry["cache"].items():
            measures["cache"].setdefault(name, []).append(status)
    return measures
