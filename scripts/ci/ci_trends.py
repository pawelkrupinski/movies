#!/usr/bin/env python3
"""Where CI's time goes, across many runs: per job and step, setup against work, the critical path, queueing, the
account's 20 runner slots, caches, machines, CPU/memory/GC, tests -- over a selection of runs, per time bucket with
trends and regression flags, grouped by leg/machine/CPU model, or as a before/after comparison.
`scripts/ci/ci-timings` runs it; docs/investigations/2026-11-01-twenty-runners.md ("How to measure") is the manual
and holds the event schema.

    ci-timings report  --workflow main.yml --last 10 [--critical] [--by cpu-model|machine|image|leg|group|label]
    ci-timings report  --workflow main.yml --runs 41298328467,41296820502
    ci-timings trend   --workflow main.yml --since 14d --by day [--threshold 20] [--format text|json|csv]
    ci-timings trend   --workflow identity-model-convergence.yml --since 7d --by leg
    ci-timings compare --workflow main.yml --before main:10 --after ci/my-branch:3
    ci-timings export  --since 2d --state FILE --vl-url URL   # the collector on ci-runner: runs → VictoriaLogs

A SELECTION (--before/--after, and report's --select): `<branch>:<N>` (the newest N completed runs on it),
`<sha>..<sha>` (runs of the commits in that range, read with the local git), `runs:<id>,<id>`, or
`<date|7d>..<date|now>` (runs that started in that window).

WHERE THE DATA COMES FROM. Timings: the GitHub API's jobs and steps. Everything else: each job's `ci-metrics-*`
artifact (scripts/ci/job_metrics.py through .github/actions/job-telemetry: machine-id.sh's machine, cgroup-sampler.sh's
resource series, each JVM's GC, JUnit suites and tests, cache-marker.sh's caches, the API rate limit), joined to its
API job by runner name. `export` turns both into flat JSON events for VictoriaLogs (the collector on ci-runner runs
it every few minutes; runners never talk to VictoriaLogs). With --vl-url (or CI_TIMINGS_VL_URL) every other command
reads those events first and the API only for runs not yet ingested.

TIMESTAMPS: only STEP timestamps are trusted. On 2026-11-01 GitHub's run and job created_at/started_at/completed_at
read an hour ahead (the DST change) while the steps' were right. Every job's skew is measured against its own steps
(`skew_of`) and taken off its created_at before a queue wait is computed; `report` says how many jobs it corrected.

QUEUE WAIT is a job's first step's start minus the moment it became runnable: GitHub creates a job when its `needs`
are met (or at the run's start), so that is its created_at, corrected for skew.

THE CRITICAL PATH is read off the timestamps rather than the YAML: from the job that ended last, step back to the job
that ended last before it became runnable (the `needs` that released it), and so on to the run's start. Each hop is
that job's queue wait plus its own time.

SLOT SATURATION counts every job of the repository's runs (every workflow, not just the one asked about) running in
the run's window, sampled every 10 s: the mean busy slots, the peak, and the share of the window all 20 were busy.

Run:  scripts/ci/ci-timings --help
Test: python3 scripts/ci/ci_trends_test.py
"""
import argparse
import csv
import datetime
import io
import json
import math
import os
import re
import statistics
import subprocess
import sys
import time
import urllib.parse
import urllib.request
import zipfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

RECORD_VERSION = 1
SLOTS = 20
SATURATION_SAMPLE_SECONDS = 10
METRICS_ARTIFACT_PREFIX = "ci-metrics-"
VL_STREAM_FIELDS = "source,repo,workflow"
# Where completed runs' jobs and metrics are kept: CI_TIMINGS_CACHE, else systemd's STATE_DIRECTORY (the collector),
# else the user's cache. A cache that cannot be written is skipped, never an error.
CACHE_DIR = os.environ.get("CI_TIMINGS_CACHE") or (
    os.path.join(os.environ["STATE_DIRECTORY"].split(":")[0], "cache") if os.environ.get("STATE_DIRECTORY")
    else os.path.join(os.environ.get("XDG_CACHE_HOME") or os.path.expanduser("~/.cache"), "ci-timings"))
GH_TIMEOUT_SECONDS = 120
SPARK = "▁▂▃▄▅▆▇█"

# A step's category, first match wins. A composite action is one API step; the record's log steps (its inner steps,
# from the job's `##[group]Run` lines) are classified instead when the record has them.
CATEGORIES = [
    ("runner", r"^Set up job$|^Complete job$|^Post job cleanup|^\(runner\)$"),
    ("publish", r"upload-artifact|junit-report|Publish|publish|imagetools|Record that|Mark deploy|report-timings"
                r"|Hand the staged|push.*image|image was built|Report |report \(|verdict"),
    ("services", r"[Mm]ongo|docker|buildx|setup-buildx|[Bb]rowser|playwright install|install-playwright|setup-chrome"
                 r"|FixtureServer|fixture-server|[Bb]oot |APT_CACHE|apt-get|Android SDK|setup-android|setup-swift"
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

# A job name's caller prefixes (`ci / `, `leg (germany) / `, `timings / `), then its matrix suffix.
CALLER_PREFIX = re.compile(r"^(?:[^()/]+(?:\([^)]*\))? / )+")
LEG = re.compile(r"^leg \(([^)]+)\) / ")
MATRIX = re.compile(r" \(.*\)$")


# ── time ────────────────────────────────────────────────────────────────────────────────────────────────────────────

def epoch(text):
    """An ISO-8601 instant (`Z`, any fraction) as UTC epoch seconds; None for none."""
    if not text:
        return None
    text = text.strip()
    match = re.match(r"^(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)(?:\.(\d+))?(Z|[+-]\d\d:\d\d)?$", text)
    if not match:
        raise ValueError("not an instant: %r" % text)
    base = datetime.datetime.strptime(match.group(1), "%Y-%m-%dT%H:%M:%S")
    zone = match.group(3) or "Z"
    offset = 0 if zone == "Z" else (int(zone[1:3]) * 3600 + int(zone[4:6]) * 60) * (1 if zone[0] == "+" else -1)
    fraction = float("0." + match.group(2)) if match.group(2) else 0.0
    return base.replace(tzinfo=datetime.timezone.utc).timestamp() - offset + fraction


def iso(seconds):
    return datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def parse_when(text, now=None):
    """`2026-11-01`, `2026-11-01T10:00:00Z`, `7d`/`12h`/`30m` ago, or `now`, as epoch seconds."""
    now = datetime.datetime.now(datetime.timezone.utc).timestamp() if now is None else now
    text = text.strip()
    if text == "now":
        return now
    match = re.fullmatch(r"(\d+(?:\.\d+)?)([wdhm])", text)
    if match:
        return now - float(match.group(1)) * {"w": 604800, "d": 86400, "h": 3600, "m": 60}[match.group(2)]
    if re.fullmatch(r"\d{4}-\d\d-\d\d", text):
        text += "T00:00:00Z"
    return epoch(text)


def skew_of(job):
    """How far GitHub's own timestamps for this job are off, in seconds, judged against its steps (see the header):
    started_at against the first step's start, rounded to the quarter hour once it is ten minutes or more; else a
    created_at AFTER the first step began (impossible) is pulled back by whole quarter hours. 0 when they agree."""
    starts = [epoch(step.get("started_at")) for step in job.get("steps") or [] if step.get("started_at")]
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


# ── records ─────────────────────────────────────────────────────────────────────────────────────────────────────────

def job_record(job):
    """One job, from the API: its window from its STEPS, runnable at created_at less its skew. None when it never ran
    a step (skipped, or cancelled while queued)."""
    steps = [step for step in job.get("steps") or [] if step.get("started_at") and step.get("completed_at")
             and step.get("conclusion") != "skipped"]
    if not steps:
        return None
    start = min(epoch(step["started_at"]) for step in steps)
    end = max(epoch(step["completed_at"]) for step in steps)
    skew = skew_of(job)
    created = epoch(job.get("created_at"))
    runnable = created - skew if created is not None else start
    return {
        "id": job.get("id"), "n": job["name"], "c": job.get("conclusion") or "unknown",
        "r": round(min(runnable, start), 1), "s": round(start, 1), "e": round(end, 1), "skew": skew,
        "st": [[step["name"], round(epoch(step["started_at"]) - start, 1),
                round(epoch(step["completed_at"]) - epoch(step["started_at"]), 1)] for step in steps],
    }


def job_group(name):
    """`ci / page tests (safari / pw phones 1/2)` → `page tests`; `leg (germany) / takes-drift-1 (germany)` →
    `takes-drift-1`: what a job is, across its matrix rows and callers."""
    return MATRIX.sub("", CALLER_PREFIX.sub("", name)).strip()


def job_leg(name, label=None):
    """A convergence row's country (its caller row, `leg (<country>) / ...`), or the telemetry label it was given."""
    match = LEG.match(name)
    return match.group(1) if match else (label or None)


def series_dicts(line):
    """A `series` metrics line (columns + rows) as a list of dicts of numbers."""
    columns = line.get("columns") or []
    rows = []
    for row in line.get("rows") or []:
        rows.append({name: value for name, value in zip(columns, row) if isinstance(value, (int, float))})
    return rows


def attach_metrics(job, lines):
    """Fold a job's metrics lines (job_metrics.py) into its record."""
    for line in lines:
        kind = line.get("kind")
        if kind == "machine":
            job["machine"] = {key: value for key, value in line.items() if key != "kind" and value not in ("", None)}
        elif kind == "job_start":
            if line.get("label"):
                job["label"] = line["label"]
            if line.get("rate_remaining") is not None:
                job.setdefault("rate", {})["start"] = line["rate_remaining"]
        elif kind == "rate_limit":
            job.setdefault("rate", {})["end"] = line.get("remaining")
        elif kind == "series":
            job["series"] = series_dicts(line)
        elif kind == "procs":
            job["procs"] = {"columns": line.get("columns"), "rows": line.get("rows")}
        elif kind == "jvm":
            job.setdefault("jvms", []).append({key: value for key, value in line.items() if key != "kind"})
        elif kind == "cache":
            job.setdefault("cache", {})[line.get("cache") or "cache"] = {
                key: line.get(key) for key in ("status", "key", "size_mb", "restore_s")}
        elif kind == "tests":
            job["tests"] = {key: value for key, value in line.items() if key != "kind"}
        elif kind in ("suite", "test"):
            job.setdefault("slow", []).append(line)
    return job


def run_record(run, jobs, metrics=None):
    """One run as the analysis reads it (see the header). `metrics` maps a runner name to that job's metrics lines."""
    records = []
    for job in jobs:
        record = job_record(job)
        if record is None:
            continue
        record["runner"] = job.get("runner_name")
        lines = (metrics or {}).get(job.get("runner_name") or "")
        if lines:
            attach_metrics(record, lines)
        records.append(record)
    skews = [job["skew"] for job in records if job["skew"]]
    created = epoch(run.get("created_at"))
    if created is not None and skews:
        created -= statistics.median(skews)
    return {
        "v": RECORD_VERSION, "repo": (run.get("repository") or {}).get("full_name"), "wf": run.get("name"),
        "path": run.get("path"), "id": run.get("id"), "num": run.get("run_number"), "attempt": run.get("run_attempt"),
        "branch": run.get("head_branch"), "sha": run.get("head_sha"), "event": run.get("event"),
        "concl": run.get("conclusion"), "created": round(created, 1) if created else None, "jobs": records,
        "skipped": sorted(job["name"] for job in jobs if job.get("conclusion") == "skipped"),
    }


# ── per-run measures ────────────────────────────────────────────────────────────────────────────────────────────────

def categorize(name):
    stripped = re.sub(r"^Post (Run )?", "", re.sub(r"^Run ", "", name))
    for category, pattern in _COMPILED:
        if pattern.search(stripped) or (category == "runner" and pattern.search(name)):
            return category
    return "other"


def job_steps(job):
    """The job's steps as [name, offset into the job, seconds], from the API."""
    return job.get("st") or []


def job_categories(job):
    totals = {}
    for name, _, seconds in job_steps(job):
        category = categorize(name)
        totals[category] = totals.get(category, 0.0) + seconds
    gap = (job["e"] - job["s"]) - sum(totals.values())
    if gap > 1:
        totals["runner"] = totals.get("runner", 0.0) + gap
    return totals


def run_window(record):
    jobs = record["jobs"]
    return (min(job["s"] for job in jobs), max(job["e"] for job in jobs)) if jobs else (None, None)


def peak_concurrency(intervals):
    events = sorted([(start, 1) for start, _ in intervals] + [(end, -1) for _, end in intervals],
                    key=lambda event: (event[0], event[1]))
    running = peak = 0
    for _, delta in events:
        running += delta
        peak = max(peak, running)
    return peak


def critical_path(record):
    """[(job name, queue seconds, own seconds)] from the run's start to its last job's end (see the header)."""
    jobs = sorted(record["jobs"], key=lambda job: job["e"])
    if not jobs:
        return []
    start = min(job["s"] for job in jobs)
    path, current, seen = [], jobs[-1], set()
    while current is not None and id(current) not in seen:
        seen.add(id(current))
        path.append((current["n"], max(0.0, current["s"] - current["r"]), current["e"] - current["s"]))
        if current["r"] <= start + 10:
            break
        before = [job for job in jobs if job["e"] <= current["r"] + 5 and id(job) not in seen]
        current = max(before, key=lambda job: job["e"]) if before else None
    return list(reversed(path))


def saturation(window, intervals, slots=SLOTS, every=SATURATION_SAMPLE_SECONDS):
    """Busy slots over `window` from every (start, end) in `intervals` (all the repository's jobs): mean, peak, and the
    share of samples at all `slots`."""
    start, end = window
    if start is None or end <= start:
        return None
    samples, t = [], start
    relevant = [(s, e) for s, e in intervals if e > start and s < end]
    while t < end:
        samples.append(sum(1 for s, e in relevant if s <= t < e))
        t += every
    return {"mean": round(statistics.mean(samples), 1), "peak": max(samples),
            "full": round(sum(1 for value in samples if value >= slots) / len(samples), 3)}


def window_resources(series, start, end):
    """The resource figures of `series` (parse_series) between two instants: CPU shares of all cores, PSI shares of
    the wall time, memory and swap at their worst, disk MB, OOM kills. None without two samples in reach."""
    inside = [row for row in series if start - 6 <= row.get("t", 0) <= end + 6]
    if len(inside) < 2:
        return None
    a, b = inside[0], inside[-1]
    seconds = max(b["t"] - a["t"], 1e-9)
    ticks = max(b.get("cpu_total", 0) - a.get("cpu_total", 0), 1e-9)

    def share(column):
        return round((b.get(column, 0) - a.get(column, 0)) / ticks, 3)

    def psi(column):
        if column not in a or column not in b:
            return None
        return round((b[column] - a[column]) / (seconds * 1e6), 3)

    used = [row["mem_total_kb"] - row["mem_avail_kb"] for row in inside if "mem_total_kb" in row and "mem_avail_kb" in row]
    swap = [row["swap_total_kb"] - row["swap_free_kb"] for row in inside if "swap_total_kb" in row and "swap_free_kb" in row]
    free = [row["disk_free_kb"] for row in inside if "disk_free_kb" in row]
    loads = [row["load1"] for row in inside if "load1" in row]
    queue = [row["procs_running"] for row in inside if "procs_running" in row]
    return {
        "cpu": share("cpu_busy"), "user": share("cpu_user"), "sys": share("cpu_sys"), "iowait": share("cpu_iowait"),
        "steal": share("cpu_steal"),
        "load_mean": round(statistics.mean(loads), 2) if loads else None, "load_max": max(loads) if loads else None,
        "runq_mean": round(statistics.mean(queue), 2) if queue else None, "runq_max": max(queue) if queue else None,
        "mem_used_max_mb": round(max(used) / 1024) if used else None,
        "mem_avail_min_mb": round(min(row["mem_avail_kb"] for row in inside if "mem_avail_kb" in row) / 1024)
        if any("mem_avail_kb" in row for row in inside) else None,
        "swap_used_max_mb": round(max(swap) / 1024) if swap else None,
        "psi_cpu_some": psi("psi_cpu_some_us"), "psi_mem_some": psi("psi_mem_some_us"),
        "psi_mem_full": psi("psi_mem_full_us"), "psi_io_some": psi("psi_io_some_us"), "psi_io_full": psi("psi_io_full_us"),
        "disk_read_mb": round((b.get("disk_read_sectors", 0) - a.get("disk_read_sectors", 0)) * 512 / 1e6),
        "disk_write_mb": round((b.get("disk_write_sectors", 0) - a.get("disk_write_sectors", 0)) * 512 / 1e6),
        "disk_free_min_mb": round(min(free) / 1024) if free else None,
        "net_rx_mb": round((b.get("net_rx_bytes", 0) - a.get("net_rx_bytes", 0)) / 1e6, 1),
        "net_tx_mb": round((b.get("net_tx_bytes", 0) - a.get("net_tx_bytes", 0)) / 1e6, 1),
        "open_files_max": max((row["open_files"] for row in inside if "open_files" in row), default=None),
        "oom_kills": int(b.get("oom_kill", 0) - a.get("oom_kill", 0)),
    }


def gc_summary(job):
    """The job's JVMs together: collections, total and max pause, GC share of the job's wall, the largest after-GC
    heap, and each long-lived JVM's flags. Precomputed (`gc_sum`) on a record rebuilt from VictoriaLogs."""
    if job.get("gc_sum"):
        return job["gc_sum"]
    jvms = job.get("jvms") or []
    if not jvms:
        return None
    wall = max(job["e"] - job["s"], 1e-9)
    pause = sum(jvm.get("gc_pause_ms") or 0 for jvm in jvms) / 1000.0
    overheads = [jvm["gc_overhead"] for jvm in jvms if jvm.get("gc_overhead") is not None and (jvm.get("uptime_s") or 0) >= 30]
    return {"jvms": len(jvms), "collections": sum(jvm.get("gc_count") or 0 for jvm in jvms), "pause_s": round(pause, 1),
            "max_pause_ms": max(jvm.get("gc_max_pause_ms") or 0 for jvm in jvms),
            "heap_after_max_mb": max(jvm.get("heap_after_max_mb") or 0 for jvm in jvms),
            "of_wall": round(pause / wall, 3), "worst_jvm_overhead": round(max(overheads), 3) if overheads else None}


def job_resources(job):
    """The job's resources over its own window, and per step (≥15 s) by the step's window. Precomputed (`res_sum`,
    `step_res`) on a record rebuilt from VictoriaLogs."""
    if job.get("res_sum") is not None:
        return job["res_sum"], job.get("step_res") or {}
    series = job.get("series")
    if not series:
        return None, {}
    whole = window_resources(series, job["s"], job["e"])
    steps = {}
    for name, offset, seconds in job_steps(job):
        if seconds < 15:
            continue
        figures = window_resources(series, job["s"] + offset, job["s"] + offset + seconds)
        if figures:
            steps[name] = figures
    return whole, steps


def top_processes(job, top=5):
    """The commands with the largest RSS over the job: peak RSS (MB), CPU seconds, peak threads, peak processes."""
    procs = job.get("procs") or {}
    columns, rows = procs.get("columns") or [], procs.get("rows") or []
    if not columns:
        return []
    index = {name: columns.index(name) for name in columns}
    by_comm, last_ticks = {}, {}
    for row in rows:
        comm = row[index["comm"]]
        entry = by_comm.setdefault(comm, {"comm": comm, "rss_mb": 0, "cpu_s": 0.0, "threads": 0, "procs": 0})
        entry["rss_mb"] = max(entry["rss_mb"], round((row[index["rss_kb"]] or 0) / 1024))
        entry["threads"] = max(entry["threads"], row[index["threads"]] or 0)
        entry["procs"] = max(entry["procs"], row[index["procs"]] or 0)
        ticks = row[index["ticks"]] or 0
        if comm in last_ticks and ticks > last_ticks[comm]:
            entry["cpu_s"] += (ticks - last_ticks[comm]) / 100.0
        last_ticks[comm] = ticks
    for entry in by_comm.values():
        entry["cpu_s"] = round(entry["cpu_s"], 1)
    return sorted(by_comm.values(), key=lambda entry: -entry["rss_mb"])[:top]


def bound_by(resources, gc):
    """What a job's wall time was bound by, from its resource figures and GC: the label `report` prints beside it."""
    notes, verdict = [], None
    if gc and gc.get("of_wall") is not None and gc["of_wall"] >= 0.05:
        notes.append("GC %.0f%% of wall" % (gc["of_wall"] * 100))
        verdict = "memory/GC"
    if resources:
        full, some = resources.get("psi_mem_full") or 0, resources.get("psi_mem_some") or 0
        if full >= 0.01 or some >= 0.10 or (resources.get("swap_used_max_mb") or 0) >= 256:
            notes.append("PSI mem full %.0f%% some %.0f%%" % (full * 100, some * 100))
            verdict = verdict or "memory"
        if (resources.get("oom_kills") or 0) > 0:
            notes.append("%d OOM kill(s)" % resources["oom_kills"])
            verdict = "memory"
        if (resources.get("psi_io_full") or 0) >= 0.05 or (resources.get("iowait") or 0) >= 0.10:
            notes.append("io full %.0f%% iowait %.0f%%" % ((resources.get("psi_io_full") or 0) * 100,
                                                          (resources.get("iowait") or 0) * 100))
            verdict = verdict or "io"
        if (resources.get("steal") or 0) >= 0.05:
            notes.append("steal %.0f%%" % (resources["steal"] * 100))
        cpu = resources.get("cpu") or 0
        notes.append("CPU %.0f%%" % (cpu * 100))
        if verdict is None:
            verdict = "cpu" if cpu >= 0.85 else ("waiting" if cpu < 0.35 else "mixed")
    return verdict, notes


def run_measures(record):
    """Everything one run contributes to an aggregate."""
    jobs = record["jobs"]
    if not jobs:
        return None
    start, end = run_window(record)
    path = critical_path(record)
    measures = {
        "id": record["id"], "num": record.get("num"), "sha": record.get("sha"), "branch": record.get("branch"),
        "concl": record.get("concl"), "start": start, "end": end, "wall": end - start,
        "runner_min": sum(job["e"] - job["s"] for job in jobs) / 60.0,
        "peak": peak_concurrency([(job["s"], job["e"]) for job in jobs]),
        "path": path, "path_queue": sum(hop[1] for hop in path), "path_work": sum(hop[2] for hop in path),
        "sat": record.get("sat"), "skewed_jobs": sum(1 for job in jobs if job.get("skew")),
        "jobs": {}, "categories": {}, "cache": {},
    }
    for job in jobs:
        categories = job_categories(job)
        resources, step_resources = job_resources(job)
        gc = gc_summary(job)
        caches = {name: (value.get("status") if isinstance(value, dict) else value)
                  for name, value in (job.get("cache") or {}).items()}
        measures["jobs"].setdefault(job["n"], []).append({
            "dur": job["e"] - job["s"], "queue": max(0.0, job["s"] - job["r"]), "concl": job["c"],
            "start": job["s"], "end": job["e"], "machine": job.get("machine") or {},
            "leg": job_leg(job["n"], job.get("label")), "group": job_group(job["n"]), "label": job.get("label"),
            "steps": {name: seconds for name, _, seconds in job_steps(job)}, "categories": categories,
            "res": resources, "step_res": step_resources, "gc": gc, "cache": caches,
            "tests": job.get("tests"), "procs": top_processes(job),
        })
        for category, seconds in categories.items():
            measures["categories"][category] = measures["categories"].get(category, 0.0) + seconds
        for name, status in caches.items():
            measures["cache"].setdefault(name, []).append(status)
    return measures


# ── statistics ──────────────────────────────────────────────────────────────────────────────────────────────────────

def percentile(values, fraction):
    values = sorted(v for v in values if v is not None)
    if not values:
        return None
    if len(values) == 1:
        return values[0]
    position = (len(values) - 1) * fraction
    low = int(math.floor(position))
    high = min(low + 1, len(values) - 1)
    return values[low] + (values[high] - values[low]) * (position - low)


def stats(values):
    values = [v for v in values if v is not None]
    if not values:
        return {"n": 0, "p50": None, "p90": None, "max": None}
    return {"n": len(values), "p50": percentile(values, 0.5), "p90": percentile(values, 0.9), "max": max(values)}


def pearson(xs, ys):
    pairs = [(x, y) for x, y in zip(xs, ys) if x is not None and y is not None]
    if len(pairs) < 5:
        return None
    mx = statistics.mean(x for x, _ in pairs)
    my = statistics.mean(y for _, y in pairs)
    sx = math.sqrt(sum((x - mx) ** 2 for x, _ in pairs))
    sy = math.sqrt(sum((y - my) ** 2 for _, y in pairs))
    if sx == 0 or sy == 0:
        return None
    return sum((x - mx) * (y - my) for x, y in pairs) / (sx * sy)


def hit_rate(statuses):
    return round(sum(1 for status in statuses if status == "hit") / len(statuses), 3) if statuses else None


def _counts(values):
    counted = {}
    for value in values:
        counted[value] = counted.get(value, 0) + 1
    return dict(sorted(counted.items(), key=lambda item: -item[1]))


def summarize(measures_list):
    """The aggregate `report` prints and `compare`/`trend` read: run-level, per job, per job's steps, per category."""
    runs = [m for m in measures_list if m]
    summary = {
        "runs": len(runs), "run_ids": [m["id"] for m in runs],
        "wall": stats([m["wall"] for m in runs]), "runner_min": stats([m["runner_min"] for m in runs]),
        "peak": stats([m["peak"] for m in runs]),
        "path_queue": stats([m["path_queue"] for m in runs]), "path_work": stats([m["path_work"] for m in runs]),
        "sat_mean": stats([(m["sat"] or {}).get("mean") for m in runs]),
        "sat_full": stats([(m["sat"] or {}).get("full") for m in runs]),
        "skewed_jobs": sum(m["skewed_jobs"] for m in runs),
        "categories": {}, "jobs": {}, "cache": {},
    }
    for category in CATEGORY_ORDER:
        values = [m["categories"].get(category, 0.0) for m in runs]
        if any(values):
            summary["categories"][category] = stats(values)
    names = sorted({name for m in runs for name in m["jobs"]})
    for name in names:
        rows = [row for m in runs for row in m["jobs"].get(name, [])]
        steps = {}
        for row in rows:
            for step, seconds in row["steps"].items():
                steps.setdefault(step, []).append(seconds)
        categories = {}
        for category in CATEGORY_ORDER:
            values = [row["categories"].get(category, 0.0) for row in rows]
            if any(values):
                categories[category] = stats(values)
        durations = [row["dur"] for row in rows]
        res = [row["res"] for row in rows if row["res"]]
        gcs = [row["gc"] for row in rows if row["gc"]]
        entry = {"dur": stats(durations), "queue": stats([row["queue"] for row in rows]),
                 "steps": {step: stats(values) for step, values in steps.items()}, "categories": categories,
                 "red": sum(1 for row in rows if row["concl"] not in ("success", "skipped")),
                 "cache": {}}
        if res:
            entry["res"] = {key: percentile([r.get(key) for r in res], 0.5) for key in res[0]}
        if gcs:
            entry["gc"] = {key: percentile([g.get(key) for g in gcs], 0.5)
                           for key in ("pause_s", "max_pause_ms", "heap_after_max_mb", "of_wall", "collections")}
        entry["bound"], entry["notes"] = bound_by(entry.get("res"), entry.get("gc"))
        correlations = {}
        for label, pick in (("gc", lambda row: (row["gc"] or {}).get("of_wall")),
                            ("psi_mem", lambda row: (row["res"] or {}).get("psi_mem_some")),
                            ("cpu", lambda row: (row["res"] or {}).get("cpu")),
                            ("queue", lambda row: row["queue"])):
            r = pearson([pick(row) for row in rows], durations)
            if r is not None:
                correlations[label] = round(r, 2)
        entry["corr"] = correlations
        models = [row["machine"].get("cpu_model") for row in rows if row["machine"].get("cpu_model")]
        images = [row["machine"].get("image_version") for row in rows if row["machine"].get("image_version")]
        entry["machines"] = {"cpu_model": _counts(models), "image_version": _counts(images)}
        tests = [row["tests"] for row in rows if row["tests"]]
        if tests:
            entry["tests"] = {key: percentile([t.get(key) for t in tests], 0.5) for key in ("tests", "failed", "skipped", "time_s")}
        for row in rows:
            for cache, status in row["cache"].items():
                entry["cache"].setdefault(cache, []).append(status)
        entry["cache"] = {cache: hit_rate(statuses) for cache, statuses in entry["cache"].items()}
        summary["jobs"][name] = entry
    for name in sorted({cache for m in runs for cache in m["cache"]}):
        summary["cache"][name] = hit_rate([status for m in runs for status in m["cache"].get(name, [])])
    paths = {}
    for m in runs:
        chain = " → ".join(hop[0] for hop in m["path"])
        paths.setdefault(chain, []).append(m)
    summary["paths"] = sorted(([chain, len(ms), percentile([x["path_queue"] for x in ms], 0.5),
                                percentile([x["path_work"] for x in ms], 0.5)] for chain, ms in paths.items()),
                              key=lambda row: -row[1])
    return summary


# ── buckets, trends, regressions ────────────────────────────────────────────────────────────────────────────────────

def bucket_start(seconds, by):
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


def bucket_label(seconds, by):
    moment = datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc)
    return moment.strftime("%m-%d %H:00" if by == "hour" else ("%Y-%m-%d" if by == "day" else "wk %Y-%m-%d"))


def buckets(measures_list, by, since=None, until=None):
    """[(bucket start, [measures])], every bucket from the first to the last, empty ones included."""
    runs = [m for m in measures_list if m]
    if not runs:
        return []
    first = bucket_start(since if since is not None else min(m["start"] for m in runs), by)
    last = bucket_start(until if until is not None else max(m["start"] for m in runs), by)
    keys, t = [], first
    step = {"hour": 3600, "day": 86400, "week": 604800}[by]
    while t <= last + 1:
        keys.append(t)
        t = bucket_start(t + step + 3600, by)  # +1h: a DST-free UTC clock needs none, but never loop on a bad step
    grouped = {key: [] for key in keys}
    for m in runs:
        key = bucket_start(m["start"], by)
        if key in grouped:
            grouped[key].append(m)
    return [(key, grouped[key]) for key in keys]


def metric_rows(summary):
    """The flat (row name, value) list one aggregate is charted by: what `trend` and `compare` line up."""
    rows = [("run: wall p50", summary["wall"]["p50"]), ("run: wall p90", summary["wall"]["p90"]),
            ("run: critical-path queue p50", summary["path_queue"]["p50"]),
            ("run: critical-path work p50", summary["path_work"]["p50"]),
            ("run: runner-min p50", summary["runner_min"]["p50"]), ("run: peak jobs p50", summary["peak"]["p50"]),
            ("run: slots busy mean", summary["sat_mean"]["p50"]),
            ("run: slots full share", summary["sat_full"]["p50"])]
    for category, figures in summary["categories"].items():
        rows.append(("category: %s p50" % category, figures["p50"]))
    for name, cache in summary["cache"].items():
        rows.append(("cache: %s hit rate" % name, cache))
    for name, entry in summary["jobs"].items():
        rows.append(("job: %s p50" % name, entry["dur"]["p50"]))
        rows.append(("job: %s p90" % name, entry["dur"]["p90"]))
        rows.append(("job: %s queue p50" % name, entry["queue"]["p50"]))
        for category, figures in entry["categories"].items():
            rows.append(("job: %s [%s] p50" % (name, category), figures["p50"]))
        for step, figures in entry["steps"].items():
            if (figures["p50"] or 0) >= 20:
                rows.append(("step: %s / %s p50" % (name, step), figures["p50"]))
        for key in ("cpu", "psi_mem_some", "psi_mem_full", "psi_cpu_some", "steal"):
            if entry.get("res", {}).get(key) is not None:
                rows.append(("res: %s %s" % (name, key), entry["res"][key]))
        if entry.get("gc"):
            rows.append(("gc: %s share of wall" % name, entry["gc"]["of_wall"]))
    return rows


def sparkline(values):
    present = [v for v in values if v is not None]
    if not present:
        return " " * len(values)
    low, high = min(present), max(present)
    span = high - low or 1.0
    return "".join(" " if v is None else SPARK[min(len(SPARK) - 1, int((v - low) / span * (len(SPARK) - 1) + 0.5))]
                   for v in values)


def slope(values):
    """Least-squares change per bucket over the buckets that have a value."""
    points = [(index, value) for index, value in enumerate(values) if value is not None]
    if len(points) < 2:
        return None
    mx = statistics.mean(i for i, _ in points)
    my = statistics.mean(v for _, v in points)
    denominator = sum((i - mx) ** 2 for i, _ in points)
    return sum((i - mx) * (v - my) for i, v in points) / denominator if denominator else None


def change(first, last):
    if first in (None, 0) or last is None:
        return None
    return (last - first) / abs(first)


def change_point(series, minimum=3):
    """Where a chronological [(measures, value)] series stepped: the split with the largest gap between the medians
    either side (each side at least `minimum` runs). (last run before, first run after, before median, after median)."""
    points = [(m, v) for m, v in series if v is not None]
    best = None
    for k in range(minimum, len(points) - minimum + 1):
        left = [v for _, v in points[:k]]
        right = [v for _, v in points[k:]]
        before, after = statistics.median(left), statistics.median(right)
        # Ties on the gap (a step inside a run of equal values) go to the split whose sides are most uniform.
        spread = sum(abs(v - before) for v in left) + sum(abs(v - after) for v in right)
        score = (abs(after - before), -spread)
        if best is None or score > best[0]:
            best = (score, points[k - 1][0], points[k][0], before, after)
    return best[1:] if best else None


def per_run_value(row_name, m):
    """One run's value of a metric row, for change_point: the same row computed over that run alone."""
    for name, value in metric_rows(summarize([m])):
        if name == row_name:
            return value
    return None


# Rows that can regress: time-like, where up is worse. Shares of slots and cache hit rates are context, not verdicts.
def regresses(name):
    return not name.startswith(("run: slots", "cache:", "run: peak"))


def regressions(current, previous, threshold, minimum_seconds=10.0):
    """The rows whose p50 rose more than `threshold` (0.2 = 20 %) and by at least `minimum_seconds` from the previous
    range to this one, worst first; a cache's hit rate falling by more than the threshold, too."""
    before = dict(metric_rows(summarize(previous)))
    flagged = []
    for name, value in metric_rows(summarize(current)):
        old = before.get(name)
        if value is None or old is None:
            continue
        if name.startswith("cache:"):
            if old - value > threshold:
                flagged.append((name, old, value, value - old))
            continue
        if not regresses(name) or old <= 0:
            continue
        absolute = name.startswith(("res:", "gc:")) or value - old >= minimum_seconds
        if (value - old) / old > threshold and absolute:
            flagged.append((name, old, value, (value - old) / old))
    return sorted(flagged, key=lambda row: -abs(row[3]))


def measures_machine_shift(before, after):
    """machine_shift over run measures: the commonest image version / CPU model before a change point and after."""
    def common(ms, key):
        counted = _counts([row["machine"].get(key) for m in ms for rows in m["jobs"].values() for row in rows
                           if row["machine"].get(key)])
        return next(iter(counted), None)
    shifts = []
    for key in ("image_version", "cpu_model"):
        a, b = common(before, key), common(after, key)
        if a and b and a != b:
            shifts.append("%s %s → %s" % (key, a, b))
    return shifts


def trend(measures_list, by, since, until, threshold, previous=None):
    """Every metric row per bucket: values, sparkline, slope per bucket, change first→last bucket; and the rows that
    regressed against `previous` (the range before), each pointed at the push window where it started."""
    grouped = buckets(measures_list, by, since, until)
    per_bucket = [dict(metric_rows(summarize(ms))) if ms else {} for _, ms in grouped]
    names = []
    for rows in per_bucket:
        for name in rows:
            if name not in names:
                names.append(name)
    table = []
    for name in names:
        values = [rows.get(name) for rows in per_bucket]
        present = [v for v in values if v is not None]
        table.append({"row": name, "values": values, "spark": sparkline(values), "slope": slope(values),
                      "change": change(present[0], present[-1]) if present else None})
    flagged = []
    if previous:
        chronological = sorted([m for m in list(previous) + list(measures_list) if m], key=lambda m: m["start"])
        for name, old, new, delta in regressions(measures_list, previous, threshold):
            point = change_point([(m, per_run_value(name, m)) for m in chronological])
            shift = []
            if point:
                shift = measures_machine_shift([m for m in chronological if m["start"] <= point[0]["start"]],
                                               [m for m in chronological if m["start"] >= point[1]["start"]])
            flagged.append({"row": name, "previous": old, "current": new, "change": delta, "machine_shift": shift,
                            "between": None if not point else {
                                "last_before": {"run": point[0]["id"], "sha": point[0]["sha"], "at": iso(point[0]["start"])},
                                "first_after": {"run": point[1]["id"], "sha": point[1]["sha"], "at": iso(point[1]["start"])},
                                "median_before": point[2], "median_after": point[3]}})
    return {"by": by, "buckets": [bucket_label(key, by) for key, _ in grouped],
            "runs": [len(ms) for _, ms in grouped], "rows": table, "regressions": flagged}


# ── GitHub ──────────────────────────────────────────────────────────────────────────────────────────────────────────

class _DropAuthOnRedirect(urllib.request.HTTPRedirectHandler):
    """An artifact download redirects to a signed blob-storage URL, which refuses a request that also carries the
    API's bearer token: the token stays on api.github.com."""

    def redirect_request(self, req, fp, code, msg, headers, newurl):
        new = super().redirect_request(req, fp, code, msg, headers, newurl)
        if new is not None and urllib.parse.urlsplit(newurl).netloc != urllib.parse.urlsplit(req.full_url).netloc:
            for name in list(new.headers):
                if name.lower() == "authorization":
                    del new.headers[name]
            new.unredirected_hdrs.pop("Authorization", None)
        return new


_OPENER = urllib.request.build_opener(_DropAuthOnRedirect)


class GitHub:
    """The API -- urllib with GH_TOKEN/GITHUB_TOKEN when set (the collector), else the `gh` CLI (a laptop) -- with a
    disk cache for what cannot change any more: a completed run's jobs and metrics artifacts."""

    def __init__(self, repo, cache_dir=CACHE_DIR, token=None):
        self.repo = repo
        self.cache_dir = os.path.join(cache_dir, repo.replace("/", "_"))
        self.token = token if token is not None else (os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN"))
        self.calls = 0

    def _url(self, path):
        path = path if path.startswith("repos/") else "repos/%s/%s" % (self.repo, path)
        return "https://api.github.com/" + path

    def _get(self, url, raw=False, attempts=3):
        """One GET, retried twice on a transient failure (a 5xx, a reset, a timeout) after 2 s and 6 s."""
        for attempt in range(attempts):
            try:
                return self._get_once(url, raw)
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired, OSError) as error:
                status = getattr(error, "code", None)
                if attempt == attempts - 1 or (status is not None and 400 <= status < 500 and status != 429):
                    raise
                time.sleep(2 * 3 ** attempt)

    def _get_once(self, url, raw=False):
        self.calls += 1
        if self.token:
            request = urllib.request.Request(url, headers={"Authorization": "Bearer " + self.token,
                                                           "Accept": "application/vnd.github+json",
                                                           "X-GitHub-Api-Version": "2022-11-28"})
            with _OPENER.open(request, timeout=GH_TIMEOUT_SECONDS) as response:
                body = response.read()
                link = response.headers.get("Link") or ""
        else:
            args = ["gh", "api", "-i", url.replace("https://api.github.com/", "")]
            done = subprocess.run(args, check=True, capture_output=True, timeout=GH_TIMEOUT_SECONDS)
            head, _, body = done.stdout.partition(b"\r\n\r\n")
            if not body and b"\n\n" in done.stdout:
                head, _, body = done.stdout.partition(b"\n\n")
            link = ""
            for line in head.decode(errors="replace").splitlines():
                if line.lower().startswith("link:"):
                    link = line.split(":", 1)[1]
        following = re.search(r'<([^>]+)>;\s*rel="next"', link)
        return body if raw else json.loads(body.decode() or "null"), (following.group(1) if following else None)

    def api(self, path, raw=False, pages=1):
        """One document; with pages > 1, a list of up to that many pages, following `Link: rel=next`."""
        url = self._url(path)
        if raw or pages == 1:
            return self._get(url, raw)[0]
        documents = []
        while url and len(documents) < pages:
            document, url = self._get(url)
            documents.append(document)
        return documents

    def _cached(self, name, compute):
        path = os.path.join(self.cache_dir, name)
        if os.path.exists(path):
            with open(path, encoding="utf-8") as handle:
                return json.load(handle)
        value = compute()
        try:
            os.makedirs(self.cache_dir, exist_ok=True)
            with open(path + ".tmp", "w", encoding="utf-8") as handle:
                json.dump(value, handle)
            os.replace(path + ".tmp", path)
        except OSError:
            pass
        return value

    def runs(self, workflow=None, branch=None, created=None, limit=None, status="completed"):
        """Runs newest first; `created` is GitHub's search syntax (`>=2026-11-01`, `a..b`)."""
        query = {"per_page": "100", "status": status}
        if branch:
            query["branch"] = branch
        if created:
            query["created"] = created
        base = ("actions/workflows/%s/runs" % workflow) if workflow else "actions/runs"
        found = []
        for page in self.api(base + "?" + urllib.parse.urlencode(query), pages=10):
            found += page["workflow_runs"]
            if limit and len(found) >= limit:
                break
        return found[:limit] if limit else found

    def run(self, run_id):
        return self.api("actions/runs/%s" % run_id)

    def jobs(self, run):
        def fetch():
            pages = self.api("actions/runs/%s/attempts/%s/jobs?per_page=100" % (run["id"], run.get("run_attempt") or 1),
                             pages=5)
            return [job for page in pages for job in page["jobs"]]
        if run.get("status") == "completed":
            return self._cached("jobs-%s-%s.json" % (run["id"], run.get("run_attempt") or 1), fetch)
        return fetch()

    def metrics(self, run):
        """Every job's metrics lines from the run's `ci-metrics-*` artifacts, by the runner name they name."""
        def fetch():
            found = {}
            pages = self.api("actions/runs/%s/artifacts?per_page=100" % run["id"], pages=3)
            for artifact in [a for page in pages for a in page.get("artifacts", [])]:
                if artifact.get("expired") or not artifact["name"].startswith(METRICS_ARTIFACT_PREFIX):
                    continue
                try:
                    blob = self.api("actions/artifacts/%s/zip" % artifact["id"], raw=True)
                    with zipfile.ZipFile(io.BytesIO(blob)) as archive:
                        text = archive.read("metrics.jsonl").decode(errors="replace")
                except Exception:  # noqa: BLE001 -- one unreadable artifact never loses the run
                    continue
                lines = parse_metrics(text)
                runner = next((l.get("runner_name") for l in lines if l.get("runner_name")), None)
                if runner:
                    found[runner] = lines
            return found
        if run.get("status") == "completed":
            return self._cached("metrics-%s-%s.json" % (run["id"], run.get("run_attempt") or 1), fetch)
        return fetch()


def parse_metrics(text):
    lines = []
    for raw in text.splitlines():
        raw = raw.strip()
        if raw.startswith("{"):
            try:
                lines.append(json.loads(raw))
            except ValueError:
                continue
    return lines


def record_of(github, run, with_metrics=True):
    """The run's record: the API's jobs for every timing, the jobs' metrics artifacts for the rest."""
    jobs = github.jobs(run)
    metrics = {}
    if with_metrics:
        try:
            metrics = github.metrics(run)
        except Exception:  # noqa: BLE001 -- the timings stand without them
            metrics = {}
    return run_record(run, jobs, metrics)


def latest_attempts(records):
    kept = {}
    for record in records:
        key = record["id"]
        if key not in kept or (record.get("attempt") or 1) > (kept[key].get("attempt") or 1):
            kept[key] = record
    return list(kept.values())


# ── VictoriaLogs events ─────────────────────────────────────────────────────────────────────────────────────────────

def _flat(prefix, figures):
    return {prefix + key: value for key, value in (figures or {}).items() if isinstance(value, (int, float, str))}


def events_of(record):
    """The run as flat JSON events (the doc's schema table): run, job, step, jvm, cache, suite/test, process."""
    if not record["jobs"]:
        return []
    m = run_measures(record)
    common = {"source": "ci", "repo": record.get("repo"), "workflow": record.get("wf"),
              "workflow_file": (record.get("path") or "").rsplit("/", 1)[-1], "run_id": record["id"],
              "run_number": record.get("num"), "run_attempt": record.get("attempt"), "sha": record.get("sha"),
              "branch": record.get("branch"), "event": record.get("event"), "run_conclusion": record.get("concl")}
    events = []
    sat = record.get("sat") or {}
    events.append(dict(common, _time=iso(m["start"]), kind="run",
                       _msg="%s run %s: wall %s, path queue %s + work %s" % (
                           record.get("wf"), record["id"], mins(m["wall"]), mins(m["path_queue"]), mins(m["path_work"])),
                       wall_s=round(m["wall"], 1), runner_min=round(m["runner_min"], 2), peak_jobs=m["peak"],
                       path_queue_s=round(m["path_queue"], 1), path_work_s=round(m["path_work"], 1),
                       critical_path=" → ".join(hop[0] for hop in m["path"]), sat_mean=sat.get("mean"),
                       sat_peak=sat.get("peak"), sat_full=sat.get("full"), jobs=len(record["jobs"]),
                       skewed_jobs=m["skewed_jobs"], skipped_jobs=len(record.get("skipped") or []),
                       skipped=", ".join(record.get("skipped") or [])[:2000], start=m["start"], end=m["end"],
                       **_flat("change_", record.get("change"))))
    for job in record["jobs"]:
        categories = job_categories(job)
        resources, step_resources = job_resources(job)
        gc = gc_summary(job)
        bound, notes = bound_by(resources, gc)
        setup = sum(v for k, v in categories.items() if k in SETUP_CATEGORIES)
        base = dict(common, job=job["n"], job_id=job.get("id"), group=job_group(job["n"]),
                    leg=job_leg(job["n"], job.get("label")), label=job.get("label"))
        event = dict(base, _time=iso(job["s"]), kind="job",
                     _msg="%s: %s (queue %s, setup %s)%s" % (job["n"], mins(job["e"] - job["s"]),
                                                             mins(max(0, job["s"] - job["r"])), mins(setup),
                                                             (" — " + ", ".join(notes)) if notes else ""),
                     conclusion=job["c"], duration_s=round(job["e"] - job["s"], 1),
                     queue_s=round(max(0.0, job["s"] - job["r"]), 1), skew_s=job.get("skew"),
                     runner_name=job.get("runner"), setup_s=round(setup, 1),
                     work_s=round(sum(categories.values()) - setup, 1), bound=bound,
                     start=job["s"], end=job["e"], runnable=job["r"],
                     rate_start=(job.get("rate") or {}).get("start"), rate_end=(job.get("rate") or {}).get("end"))
        event.update(_flat("machine_", job.get("machine")))
        event.update(_flat("res_", resources))
        if gc:
            event.update(_flat("gc_", {k: v for k, v in gc.items() if k != "flags"}))
        event.update(_flat("tests_", job.get("tests")))
        events.append(event)
        for index, (name, offset, seconds) in enumerate(job_steps(job)):
            step = dict(base, _time=iso(job["s"] + offset), kind="step", _msg="%s / %s: %s" % (job["n"], name, mins(seconds)),
                        step=name, step_index=index, category=categorize(name), offset_s=offset, duration_s=seconds)
            step.update(_flat("res_", step_resources.get(name)))
            events.append(step)
        for jvm in job.get("jvms") or []:
            events.append(dict(base, _time=iso(job["s"]), kind="jvm",
                               _msg="%s: JVM %s GC %.1f s (%s collections)" % (
                                   job["n"], jvm.get("main") or jvm.get("pid"), (jvm.get("gc_pause_ms") or 0) / 1000.0,
                                   jvm.get("gc_count")),
                               **{k: v for k, v in jvm.items() if isinstance(v, (int, float, str))}))
        for name, cache in (job.get("cache") or {}).items():
            cache = cache if isinstance(cache, dict) else {"status": cache}
            events.append(dict(base, _time=iso(job["s"]), kind="cache", _msg="%s: cache %s %s" % (job["n"], name, cache.get("status")),
                               cache=name, family=re.sub(r"-[0-9a-f]{16,}$", "", cache.get("key") or name), **cache))
        for line in job.get("slow") or []:
            events.append(dict(base, _time=iso(job["s"]), _msg="%s: %s %s %.1fs" % (
                job["n"], line.get("kind"), line.get("test") or line.get("suite"), line.get("time_s") or 0),
                **{k: v for k, v in line.items() if isinstance(v, (int, float, str))}))
        for proc in top_processes(job):
            events.append(dict(base, _time=iso(job["s"]), kind="process",
                               _msg="%s: %s peak %d MB, %.0f CPU-s" % (job["n"], proc["comm"], proc["rss_mb"], proc["cpu_s"]),
                               **proc))
    return events


def commit_change(github, sha):
    """The commit's size: files changed, lines added and deleted (one cached API call; None when unreadable)."""
    if not sha or not hasattr(github, "_cached"):
        return None
    try:
        commit = github._cached("commit-%s.json" % sha, lambda: github.api("commits/%s" % sha))
    except Exception:  # noqa: BLE001 -- a size is context, never a reason to drop the run
        return None
    stats = commit.get("stats") or {}
    return {"files": len(commit.get("files") or []), "additions": stats.get("additions"),
            "deletions": stats.get("deletions"), "parents": len(commit.get("parents") or [])}


def post_events(url, events, opener=urllib.request.urlopen):
    body = "".join(json.dumps(event, separators=(",", ":"), default=str) + "\n" for event in events).encode()
    query = urllib.parse.urlencode({"_stream_fields": VL_STREAM_FIELDS, "_time_field": "_time", "_msg_field": "_msg"})
    request = urllib.request.Request(url.rstrip("/") + "/insert/jsonline?" + query, data=body, method="POST",
                                     headers={"Content-Type": "application/stream+json"})
    with opener(request, timeout=60) as response:
        if response.status >= 300:
            raise RuntimeError("VictoriaLogs answered HTTP %s" % response.status)


def vl_query(url, query, start=None, end=None, limit=200000, opener=urllib.request.urlopen):
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


def _number(value):
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def records_from_events(events):
    """Rebuild run records from VictoriaLogs' events (strings everywhere: VictoriaLogs stores every field as text)."""
    runs, jobs = {}, {}
    for event in events:
        kind = event.get("kind")
        run_id = int(float(event.get("run_id") or 0))
        if kind == "run":
            runs[run_id] = {"v": RECORD_VERSION, "repo": event.get("repo"), "wf": event.get("workflow"),
                            "path": ".github/workflows/" + (event.get("workflow_file") or ""), "id": run_id,
                            "num": _number(event.get("run_number")), "attempt": _number(event.get("run_attempt")),
                            "branch": event.get("branch"), "sha": event.get("sha"), "event": event.get("event"),
                            "concl": event.get("run_conclusion"), "created": None, "jobs": [],
                            "sat": {"mean": _number(event.get("sat_mean")), "peak": _number(event.get("sat_peak")),
                                    "full": _number(event.get("sat_full"))} if event.get("sat_mean") else None}
        elif kind == "job":
            key = (run_id, event.get("job_id") or event.get("job"))
            job = jobs.setdefault(key, {"st": [], "run": run_id})
            job.update({"id": event.get("job_id"), "n": event.get("job"), "c": event.get("conclusion"),
                        "s": _number(event.get("start")), "e": _number(event.get("end")),
                        "r": _number(event.get("runnable")), "skew": _number(event.get("skew_s")) or 0,
                        "label": event.get("label") or None, "runner": event.get("runner_name"),
                        "machine": {k[8:]: v for k, v in event.items() if k.startswith("machine_")}})
            res = {k[4:]: _number(v) for k, v in event.items() if k.startswith("res_")}
            job["res_sum"] = res or None
            gc = {k[3:]: _number(v) for k, v in event.items() if k.startswith("gc_")}
            job["gc_sum"] = gc or None
            tests = {k[6:]: _number(v) for k, v in event.items() if k.startswith("tests_")}
            if tests:
                job["tests"] = tests
        elif kind == "step":
            key = (run_id, event.get("job_id") or event.get("job"))
            job = jobs.setdefault(key, {"st": [], "run": run_id})
            job["st"].append([event.get("step"), _number(event.get("offset_s")) or 0.0,
                              _number(event.get("duration_s")) or 0.0, _number(event.get("step_index")) or 0])
            res = {k[4:]: _number(v) for k, v in event.items() if k.startswith("res_")}
            if res:
                job.setdefault("step_res", {})[event.get("step")] = res
        elif kind == "cache":
            key = (run_id, event.get("job_id") or event.get("job"))
            jobs.setdefault(key, {"st": [], "run": run_id}).setdefault("cache", {})[event.get("cache")] = {
                "status": event.get("status"), "key": event.get("key")}
    for job in jobs.values():
        if job.get("s") is None or job["run"] not in runs:
            continue
        job["st"] = [step[:3] for step in sorted(job["st"], key=lambda step: step[3])]
        if job.get("res_sum") is None:
            job.pop("res_sum", None)
        runs[job.pop("run")]["jobs"].append(job)
    return list(runs.values())


# ── selection ───────────────────────────────────────────────────────────────────────────────────────────────────────

def workflow_matches(record, workflow):
    if not workflow:
        return True
    return (record.get("path") or "").endswith("/" + workflow) or record.get("wf") == workflow


def commits_in(spec):
    out = subprocess.run(["git", "rev-list", spec], check=True, capture_output=True, text=True, timeout=60).stdout
    return set(out.split())


class Source:
    """Records for a selection: VictoriaLogs' events first (with a URL), then the API's for what it lacks."""

    def __init__(self, github, vl_url=None, include_red=False, with_metrics=True):
        self.github = github
        self.vl_url = vl_url
        self.include_red = include_red
        self.with_metrics = with_metrics

    def _keep(self, run):
        if run.get("status") != "completed" or run.get("conclusion") in ("cancelled", "skipped"):
            return False
        return self.include_red or run.get("conclusion") == "success"

    def _keep_record(self, record):
        if record.get("concl") in ("cancelled", "skipped"):
            return False
        return self.include_red or record.get("concl") == "success"

    def from_vl(self, since, until, workflow=None, run_ids=None):
        if not self.vl_url:
            return []
        query = 'source:ci repo:"%s" kind:in(run,job,step,cache)' % self.github.repo
        if workflow:
            query += ' workflow_file:"%s"' % workflow
        if run_ids:
            query += " run_id:in(%s)" % ",".join(str(i) for i in run_ids)
        try:
            return [r for r in records_from_events(vl_query(self.vl_url, query, since, until)) if r["jobs"]]
        except Exception as error:  # noqa: BLE001 -- the API still answers
            print("ci-timings: VictoriaLogs unreadable (%s); reading the API" % error, file=sys.stderr)
            return []

    def fetch(self, runs):
        return [record_of(self.github, run, self.with_metrics) for run in runs]

    def select(self, spec, workflow):
        """A selection (the header's forms) as records, oldest first."""
        spec = spec.strip()
        if spec.startswith("runs:"):
            ids = [int(x) for x in spec[5:].split(",") if x.strip()]
            held = {r["id"]: r for r in self.from_vl(None, None, None, ids)}
            records = [held.get(i) or record_of(self.github, self.github.run(i), self.with_metrics) for i in ids]
        elif re.fullmatch(r"[0-9a-fA-F]{6,40}\.\.[0-9a-fA-F]{6,40}", spec):
            shas = commits_in(spec)
            candidates = [r for r in self.github.runs(workflow=workflow) if r.get("head_sha") in shas and self._keep(r)]
            records = self.fetch(candidates)
        elif ".." in spec:
            since, until = (parse_when(part) for part in spec.split("..", 1))
            records = self.window(workflow, since, until)
        else:
            branch, _, count = spec.partition(":")
            count = int(count or 10)
            runs = [r for r in self.github.runs(workflow=workflow, branch=branch, limit=count * 3) if self._keep(r)]
            held = {r["id"]: r for r in self.from_vl(None, None, workflow, [run["id"] for run in runs[:count]])}
            records = [held.get(run["id"]) or record_of(self.github, run, self.with_metrics) for run in runs[:count]]
        records = [r for r in records if r and r["jobs"] and workflow_matches(r, workflow)]
        return sorted(latest_attempts(records), key=lambda r: run_window(r)[0] or 0)

    def window(self, workflow, since, until):
        """Every kept run of the workflow whose first job started in [since, until]: VictoriaLogs', then the API's."""
        held = {r["id"]: r for r in latest_attempts(self.from_vl(since, until, workflow)) if self._keep_record(r)}
        created = "%s..%s" % (iso(since - 86400)[:10], iso(until + 86400)[:10])
        try:
            runs = self.github.runs(workflow=workflow, created=created)
        except Exception:  # noqa: BLE001 -- VictoriaLogs' runs still stand
            runs = []
        for run in runs:
            if run["id"] not in held and self._keep(run):
                held[run["id"]] = record_of(self.github, run, self.with_metrics)
        return [r for r in held.values() if r["jobs"] and since <= run_window(r)[0] <= until]


def add_saturation(records, github, everything=None):
    """Each record's slot saturation (see the header), from every job of the repository's runs in its window."""
    records = [r for r in records if r["jobs"]]
    if not records:
        return records
    if everything is None:
        start = min(run_window(r)[0] for r in records) - 4 * 3600
        end = max(run_window(r)[1] for r in records)
        everything = []
        for run in github.runs(created="%s..%s" % (iso(start)[:10], iso(end)[:10])):
            if run.get("status") != "completed":
                continue
            for job in github.jobs(run):
                record = job_record(job)
                if record:
                    everything.append((record["s"], record["e"]))
    for record in records:
        record["sat"] = saturation(run_window(record), everything)
    return records


# ── grouping ────────────────────────────────────────────────────────────────────────────────────────────────────────

ATTRIBUTES = {
    "cpu-model": lambda job: (job.get("machine") or {}).get("cpu_model"),
    "image": lambda job: (job.get("machine") or {}).get("image_version"),
    "machine": lambda job: "%s · %s cores · %s" % ((job.get("machine") or {}).get("cpu_model"),
                                                   (job.get("machine") or {}).get("cores"),
                                                   (job.get("machine") or {}).get("image_version")),
    "leg": lambda job: job_leg(job["n"], job.get("label")),
    "group": lambda job: job_group(job["n"]),
    "label": lambda job: job.get("label"),
    "runner-os": lambda job: (job.get("machine") or {}).get("runner_os"),
}


def partition(records, attribute):
    """{value: records holding only that value's jobs}, for `--by <attribute>`."""
    pick = ATTRIBUTES[attribute]
    parts = {}
    for record in records:
        for job in record["jobs"]:
            value = pick(job) or "(unknown)"
            part = parts.setdefault(value, {})
            part.setdefault(record["id"], dict(record, jobs=[]))["jobs"].append(job)
    return {value: list(by_run.values()) for value, by_run in sorted(parts.items())}


def machine_shift(before, after):
    """The image version or CPU model most jobs ran on before a change point and after it, when they differ."""
    def common(records, key):
        counted = _counts([(job.get("machine") or {}).get(key) for record in records for job in record["jobs"]
                           if (job.get("machine") or {}).get(key)])
        return next(iter(counted), None)
    shifts = []
    for key in ("image_version", "cpu_model"):
        a, b = common(before, key), common(after, key)
        if a and b and a != b:
            shifts.append("%s %s → %s" % (key, a, b))
    return shifts


# ── per-leg tables (the in-run summary) ─────────────────────────────────────────────────────────────────────────────

def leg_tables(record):
    """Markdown: one table per leg (convergence's countries; a workflow without legs gets one per job group of two or
    more rows): each row's queue, setup by category, work, total, machine, GC and pressure; the leg's wall-clock from
    its first row's start to its last row's end, and its critical (last-ending) row."""
    groups = {}
    for job in record["jobs"]:
        leg = job_leg(job["n"], job.get("label"))
        groups.setdefault(leg or ("group: " + job_group(job["n"])), []).append(job)
    has_legs = any(not key.startswith("group: ") for key in groups)
    lines = []
    for key, jobs in sorted(groups.items()):
        if has_legs and key.startswith("group: "):
            continue
        if not has_legs and len(jobs) < 2:
            continue
        start = min(job["s"] for job in jobs)
        end = max(job["e"] for job in jobs)
        critical = max(jobs, key=lambda job: job["e"])
        lines += ["", "#### %s — wall %s, critical row: %s" % (key.replace("group: ", ""), mins(end - start), critical["n"]),
                  "", "| row | queue | setup (by category) | work | total | machine · GC · pressure |",
                  "|---|---|---|---|---|---|"]
        for job in sorted(jobs, key=lambda job: -(job["e"] - job["s"])):
            categories = job_categories(job)
            setup = {k: v for k, v in categories.items() if k in SETUP_CATEGORIES and v >= 1}
            work = sum(v for k, v in categories.items() if k not in SETUP_CATEGORIES)
            resources, _ = job_resources(job)
            gc = gc_summary(job)
            bound, notes = bound_by(resources, gc)
            machine = job.get("machine") or {}
            note = " · ".join(x for x in [
                "%s %sc" % (machine.get("cpu_model") or "?", machine.get("cores") or "?") if machine else "",
                ", ".join(notes), bound or ""] if x)
            lines.append("| %s | %s | %s %s | %s | %s | %s |" % (
                CALLER_PREFIX.sub("", job["n"]), mins(max(0, job["s"] - job["r"])), mins(sum(setup.values())),
                "(" + ", ".join("%s %s" % (k, mins(v)) for k, v in sorted(setup.items(), key=lambda kv: -kv[1])) + ")"
                if setup else "", mins(work), mins(job["e"] - job["s"]), note or "-"))
    return "\n".join(lines) + ("\n" if lines else "")


# ── output ──────────────────────────────────────────────────────────────────────────────────────────────────────────

def mins(seconds):
    if seconds is None:
        return "-"
    return "%d:%02d" % (seconds // 60, round(seconds % 60)) if seconds >= 60 else "%ds" % round(seconds)


def pct(value):
    return "-" if value is None else "%.0f%%" % (value * 100)


def format_report(summary, workflow, critical=False, steps=True):
    lines = ["%s: %d runs (%s)" % (workflow or "all workflows", summary["runs"],
                                   ", ".join(str(i) for i in summary["run_ids"][-12:])),
             "  wall p50 %s p90 %s max %s · runner-min p50 %.0f · peak jobs p50 %s" % (
                 mins(summary["wall"]["p50"]), mins(summary["wall"]["p90"]), mins(summary["wall"]["max"]),
                 summary["runner_min"]["p50"] or 0, summary["peak"]["p50"]),
             "  critical path p50: queue %s + work %s" % (mins(summary["path_queue"]["p50"]),
                                                          mins(summary["path_work"]["p50"]))]
    if summary["sat_mean"]["n"]:
        lines.append("  account slots during the run: mean %.1f of %d busy, all %d busy %s of the time (p50)" % (
            summary["sat_mean"]["p50"], SLOTS, SLOTS, pct(summary["sat_full"]["p50"])))
    if summary["skewed_jobs"]:
        lines.append("  clock skew: %d jobs' GitHub timestamps were off; corrected from their steps" % summary["skewed_jobs"])
    if summary["cache"]:
        lines.append("  cache hit rates: " + ", ".join("%s %s" % (k, pct(v)) for k, v in summary["cache"].items()))
    lines.append("")
    setup = sum((summary["categories"].get(c) or {}).get("p50") or 0 for c in SETUP_CATEGORIES)
    work = sum((summary["categories"].get(c) or {}).get("p50") or 0 for c in summary["categories"]
               if c not in SETUP_CATEGORIES)
    lines.append("  setup vs work (runner-min per run, p50): setup %.1f, work %.1f" % (setup / 60, work / 60))
    lines.append("  %-18s %8s %8s" % ("category", "p50", "p90"))
    for category, figures in summary["categories"].items():
        lines.append("  %-18s %8s %8s" % (category, mins(figures["p50"]), mins(figures["p90"])))
    lines.append("")
    lines.append("  %-46s %3s %6s %6s %6s %6s %6s %6s  %s" % ("job", "n", "p50", "p90", "max", "queue", "q p90",
                                                               "setup", "bound"))
    ordered = sorted(summary["jobs"].items(), key=lambda item: -(item[1]["dur"]["p50"] or 0))
    for name, entry in ordered:
        setup_s = sum((entry["categories"].get(c) or {}).get("p50") or 0 for c in SETUP_CATEGORIES)
        red = " (%d red)" % entry["red"] if entry["red"] else ""
        bound = entry["bound"] or ""
        if entry["notes"]:
            bound += ": " + ", ".join(entry["notes"])
        if entry["corr"]:
            strong = ["r(dur,%s)=%.2f" % (k, v) for k, v in entry["corr"].items() if abs(v) >= 0.6]
            if strong:
                bound += " · " + ", ".join(strong)
        lines.append("  %-46s %3d %6s %6s %6s %6s %6s %6s  %s%s" % (
            name[:46], entry["dur"]["n"], mins(entry["dur"]["p50"]), mins(entry["dur"]["p90"]),
            mins(entry["dur"]["max"]), mins(entry["queue"]["p50"]), mins(entry["queue"]["p90"]), mins(setup_s),
            bound, red))
        if steps:
            for step, figures in sorted(entry["steps"].items(), key=lambda item: -(item[1]["p50"] or 0)):
                if (figures["p50"] or 0) >= 15:
                    lines.append("      %-62s %6s %6s  [%s]" % (step[:62], mins(figures["p50"]), mins(figures["p90"]),
                                                               categorize(step)))
    if critical:
        lines.append("")
        lines.append("  critical paths (runs · queue p50 · work p50):")
        for chain, count, queue, work in summary["paths"][:5]:
            lines.append("    %2d × %s + %s  %s" % (count, mins(queue), mins(work), chain))
    return "\n".join(lines)


def format_value(name, value):
    if value is None:
        return "-"
    if name.startswith(("cache:", "res:", "gc:")) or "share" in name:
        return pct(value)
    if "runner-min" in name or "peak" in name or "busy mean" in name:
        return "%.1f" % value
    return mins(value)


def format_trend(result, limit=None):
    lines = ["buckets (%s): %s" % (result["by"], " ".join("%s(%d)" % (b, n) for b, n in zip(result["buckets"], result["runs"])))]
    lines.append("%-60s %-*s %9s %9s %8s" % ("row", len(result["buckets"]), "trend", "first", "last", "change"))
    for row in result["rows"][:limit] if limit else result["rows"]:
        present = [v for v in row["values"] if v is not None]
        lines.append("%-60s %s %9s %9s %8s" % (
            row["row"][:60], row["spark"], format_value(row["row"], present[0] if present else None),
            format_value(row["row"], present[-1] if present else None),
            "-" if row["change"] is None else "%+.0f%%" % (row["change"] * 100)))
    if result["regressions"]:
        lines.append("")
        lines.append("REGRESSIONS against the previous range:")
        for flag in result["regressions"]:
            where = ""
            if flag["between"]:
                b = flag["between"]
                where = " -- started between run %s (%s, %s) and run %s (%s, %s)" % (
                    b["last_before"]["run"], (b["last_before"]["sha"] or "")[:9], b["last_before"]["at"],
                    b["first_after"]["run"], (b["first_after"]["sha"] or "")[:9], b["first_after"]["at"])
            if flag.get("machine_shift"):
                where += " -- lines up with a machine change: " + "; ".join(flag["machine_shift"])
            lines.append("  ! %s: %s → %s (%+.0f%%)%s" % (flag["row"], format_value(flag["row"], flag["previous"]),
                                                          format_value(flag["row"], flag["current"]),
                                                          flag["change"] * 100, where))
    elif result.get("compared"):
        lines.append("")
        lines.append("No regression beyond the threshold against the previous range.")
    return "\n".join(lines)


def compare_rows(before, after):
    left = dict(metric_rows(summarize(before)))
    right = dict(metric_rows(summarize(after)))
    names = [name for name, _ in metric_rows(summarize(before))]
    names += [name for name in right if name not in left]
    return [{"row": name, "before": left.get(name), "after": right.get(name),
             "change": change(left.get(name), right.get(name))} for name in names]


def format_compare(rows, before_n, after_n, everything=False):
    lines = ["%-64s %9s %9s %8s" % ("row (before %d runs, after %d)" % (before_n, after_n), "before", "after", "change")]
    for row in rows:
        if not everything and row["row"].startswith("step:"):
            continue
        lines.append("%-64s %9s %9s %8s" % (row["row"][:64], format_value(row["row"], row["before"]),
                                            format_value(row["row"], row["after"]),
                                            "-" if row["change"] is None else "%+.0f%%" % (row["change"] * 100)))
    return "\n".join(lines)


def emit(data, fmt, text, out):
    if fmt == "json":
        json.dump(data, out, indent=1, default=str)
        out.write("\n")
    elif fmt == "csv":
        writer = csv.writer(out)
        rows = data if isinstance(data, list) else data.get("rows") or []
        if rows and isinstance(rows[0], dict):
            keys = list(rows[0].keys())
            if "values" in keys:
                header = ["row"] + list(data["buckets"]) + ["slope", "change"]
                writer.writerow(header)
                for row in rows:
                    writer.writerow([row["row"]] + row["values"] + [row["slope"], row["change"]])
            else:
                writer.writerow(keys)
                for row in rows:
                    writer.writerow([row[key] for key in keys])
    else:
        out.write(text + "\n")


# ── commands ────────────────────────────────────────────────────────────────────────────────────────────────────────

def repo_of(args):
    if args.repo:
        return args.repo
    try:
        url = subprocess.run(["git", "remote", "get-url", "origin"], check=True, capture_output=True, text=True,
                             timeout=10).stdout.strip()
        match = re.search(r"github\.com[:/](.+?)(\.git)?$", url)
        if match:
            return match.group(1)
    except (subprocess.CalledProcessError, OSError):
        pass
    return "pawelkrupinski/movies"


TIME_UNITS = ("hour", "day", "week")


def report_text(records, args):
    if args.by and args.by not in TIME_UNITS:
        parts = partition(records, args.by)
        chunks = []
        for value, part in parts.items():
            summary = summarize([run_measures(r) for r in part])
            chunks.append("── %s = %s ──\n%s" % (args.by, value, format_report(summary, args.workflow,
                                                                                 critical=False, steps=False)))
        return {value: summarize([run_measures(r) for r in part]) for value, part in parts.items()}, "\n\n".join(chunks)
    summary = summarize([run_measures(r) for r in records])
    return summary, format_report(summary, args.workflow, critical=args.critical, steps=not args.no_steps)


def command_report(args, source, out):
    spec = args.select or ("runs:" + args.runs if args.runs else "%s:%d" % (args.branch, args.last))
    if args.since:
        spec = "%s..%s" % (args.since, args.until or "now")
    records = source.select(spec, args.workflow)
    if args.saturation:
        add_saturation([r for r in records if not r.get("sat")], source.github)
    data, text = report_text(records, args)
    emit(data, args.format, text, out)


def command_trend(args, source, out):
    until = parse_when(args.until or "now")
    since = parse_when(args.since or "14d")
    records = source.window(args.workflow, since, until)
    if args.saturation:
        add_saturation([r for r in records if not r.get("sat")], source.github)
    previous = [] if args.no_previous else source.window(args.workflow, since - (until - since), since)
    every = args.by if args.by in TIME_UNITS else args.every
    parts = {"": (records, previous)}
    if args.by and args.by not in TIME_UNITS:
        now_parts, before_parts = partition(records, args.by), partition(previous, args.by)
        parts = {value: (part, before_parts.get(value, [])) for value, part in now_parts.items()}
    results, texts = {}, []
    for value, (part, before) in parts.items():
        result = trend([run_measures(r) for r in part], every, since, until, args.threshold / 100.0,
                       [run_measures(r) for r in before])
        result["compared"] = bool(before)
        if args.match:
            result["rows"] = [row for row in result["rows"] if re.search(args.match, row["row"])]
        results[value] = result
        texts.append(("── %s = %s ──\n" % (args.by, value) if value else "") + format_trend(result))
    data = results[""] if "" in results else results
    if args.format == "csv" and "" not in results:
        rows = []
        for value, result in results.items():
            rows += [dict(row, row="%s=%s: %s" % (args.by, value, row["row"])) for row in result["rows"]]
        data = {"buckets": next(iter(results.values()))["buckets"] if results else [], "rows": rows}
    emit(data, args.format, "\n\n".join(texts), out)


def command_compare(args, source, out):
    before = [run_measures(r) for r in source.select(args.before, args.workflow)]
    after = [run_measures(r) for r in source.select(args.after, args.workflow)]
    rows = compare_rows(before, after)
    if args.match:
        rows = [row for row in rows if re.search(args.match, row["row"])]
    emit(rows, args.format, format_compare(rows, len([m for m in before if m]), len([m for m in after if m]),
                                           args.steps), out)


def load_state(path):
    try:
        with open(path, encoding="utf-8") as handle:
            return json.load(handle)
    except (OSError, ValueError):
        return {"done": {}}


def save_state(path, state):
    folder = os.path.dirname(os.path.abspath(path))
    os.makedirs(folder, exist_ok=True)
    with open(path + ".tmp", "w", encoding="utf-8") as handle:
        json.dump(state, handle)
    os.replace(path + ".tmp", path)


def command_export(args, source, out, post=post_events):
    """The collector: every completed run of the repository (every workflow) created since --since and not yet in the
    state file, as events, POSTed to VictoriaLogs (or printed with --dry-run). Idempotent and resumable: a run enters
    the state only once its events were accepted; one run's failure is logged and the rest go on."""
    until = parse_when(args.until or "now")
    since = parse_when(args.since or "2d")
    github = source.github
    state = load_state(args.state) if args.state else {"done": {}}
    runs = github.runs(created="%s..%s" % (iso(since)[:10], iso(until)[:10]))
    pending = [run for run in runs if run.get("status") == "completed"
               and "%s/%s" % (run["id"], run.get("run_attempt") or 1) not in state["done"]]
    def log(message):
        print("ci-timings export: " + message, file=sys.stderr, flush=True)

    log("%d runs in the window, %d to send, %d already in" % (len(runs), len(pending), len(runs) - len(pending)))
    # Every job's window first (one cached call per run), for each run's slot saturation; then each run is read, sent
    # and recorded in the state ON ITS OWN, so a pass cut short (a budget floor, a timeout) keeps what it sent and the
    # journal shows it moving (2026-11-01: the first pass read every run before sending any, ~15 min of silence).
    everything = []
    completed = [run for run in runs if run.get("status") == "completed"]
    for index, run in enumerate(completed, 1):
        try:
            for job in github.jobs(run):
                record = job_record(job)
                if record:
                    everything.append((record["s"], record["e"]))
        except Exception:  # noqa: BLE001 -- saturation is context; the run itself is read again below
            pass
        if index % 50 == 0 or index == len(completed):
            log("job windows: %d of %d runs, %d API calls" % (index, len(completed), github.calls))
    sent = failed = 0
    started = parse_when("now")
    for index, run in enumerate(sorted(pending, key=lambda run: run.get("created_at") or ""), 1):
        key = "%s/%s" % (run["id"], run.get("run_attempt") or 1)
        try:
            record = record_of(github, run)
            if record["jobs"]:
                record["sat"] = saturation(run_window(record), everything)
            record["change"] = commit_change(github, record.get("sha"))
            events = events_of(record)
            if args.dry_run:
                for event in events:
                    out.write(json.dumps(event, separators=(",", ":"), default=str) + "\n")
            elif events:
                post(args.vl_url, events)
        except Exception as error:  # noqa: BLE001 -- see the docstring
            failed += 1
            log("run %s not sent: %s" % (run["id"], error))
            continue
        sent += 1
        state["done"][key] = int(parse_when("now"))
        if args.state and not args.dry_run:
            save_state(args.state, state)
        if index % 10 == 0 or index == len(pending):
            log("sent %d of %d (%s %s, %d events), %d failed, %d API calls, %.0f s" % (
                index, len(pending), run.get("name"), run["id"], len(events), failed, github.calls,
                parse_when("now") - started))
    horizon = parse_when("100d")
    state["done"] = {k: v for k, v in state["done"].items() if v >= horizon}
    if args.state and not args.dry_run:
        save_state(args.state, state)
    log("%d runs sent, %d failed, %d already in, %d API calls" % (sent, failed, len(runs) - len(pending), github.calls))


def command_legs(args, source, out):
    """The in-run summary's per-leg tables for one run (ci_timings.py calls leg_tables directly)."""
    run = source.github.run(args.run_id)
    out.write(leg_tables(record_of(source.github, run)))


def main(argv=None, out=sys.stdout):
    parser = argparse.ArgumentParser(prog="ci-timings", description=__doc__.split("\n")[0],
                                     formatter_class=argparse.RawDescriptionHelpFormatter, epilog=__doc__)
    # THE COMMON OPTIONS GO EITHER SIDE OF THE COMMAND (2026-11-01: the collector on ci-runner ran
    # `export ... --vl-url URL`, which only `--vl-url URL export ...` parsed, and failed every pass). Each subcommand
    # takes them too, defaulting to SUPPRESS so a value given before the command is never reset by the subparser.
    def common(target, top):
        default = (lambda value: value) if top else (lambda value: argparse.SUPPRESS)
        target.add_argument("--repo", default=default(None), help="OWNER/REPO (default: origin's)")
        target.add_argument("--vl-url", default=default(os.environ.get("CI_TIMINGS_VL_URL")),
                            help="VictoriaLogs base URL (env CI_TIMINGS_VL_URL); none: the API alone")
        target.add_argument("--include-red", action="store_true", default=default(False),
                            help="count failed runs too (default: green only)")
        target.add_argument("--no-metrics", action="store_true", default=default(False),
                            help="skip the jobs' metrics artifacts (timings only)")
        target.add_argument("--format", choices=("text", "json", "csv"), default=default("text"))
        target.add_argument("--parse-only", action="store_true", default=default(False),
                            help="check the arguments and exit 0, touching nothing (infra's check of its unit's argv)")

    common(parser, top=True)
    sub = parser.add_subparsers(dest="command", required=True)
    _add_parser = sub.add_parser

    def add_parser(*args_, **kwargs):
        made = _add_parser(*args_, **kwargs)
        common(made, top=False)
        return made
    sub.add_parser = add_parser
    by_help = "hour|day|week (time buckets) or %s (groups)" % "|".join(sorted(ATTRIBUTES))

    report = sub.add_parser("report", help="one aggregate over a selection of runs")
    report.add_argument("--workflow", default="main.yml")
    report.add_argument("--branch", default="main")
    report.add_argument("--last", type=int, default=10)
    report.add_argument("--runs", help="run ids, comma-separated")
    report.add_argument("--select", help="a selection (see the header)")
    report.add_argument("--since")
    report.add_argument("--until")
    report.add_argument("--by", choices=sorted(ATTRIBUTES), help="one report per group")
    report.add_argument("--critical", action="store_true", help="list the critical paths")
    report.add_argument("--saturation", action="store_true", help="measure slot saturation for runs VictoriaLogs "
                                                                  "does not hold (more API calls)")
    report.add_argument("--no-steps", action="store_true")

    trend_ = sub.add_parser("trend", help="per time bucket, with trends and regressions against the previous range")
    trend_.add_argument("--workflow", default="main.yml")
    trend_.add_argument("--since", default="14d")
    trend_.add_argument("--until")
    trend_.add_argument("--by", default="day", choices=list(TIME_UNITS) + sorted(ATTRIBUTES), help=by_help)
    trend_.add_argument("--every", default="day", choices=TIME_UNITS, help="the time bucket under a group --by")
    trend_.add_argument("--threshold", type=float, default=20.0, help="regression: p50 up more than this %%")
    trend_.add_argument("--match", help="only rows matching this regex")
    trend_.add_argument("--no-previous", action="store_true", help="skip the previous range (no regression check)")
    trend_.add_argument("--saturation", action="store_true")

    compare = sub.add_parser("compare", help="before/after over two selections")
    compare.add_argument("--workflow", default="main.yml")
    compare.add_argument("--before", required=True)
    compare.add_argument("--after", required=True)
    compare.add_argument("--match")
    compare.add_argument("--steps", action="store_true", help="include every step row")

    export = sub.add_parser("export", help="the collector: send new runs' events to VictoriaLogs")
    export.add_argument("--since", default="2d", help="90d for the backfill")
    export.add_argument("--until")
    export.add_argument("--state", help="the file of runs already sent")
    export.add_argument("--dry-run", action="store_true", help="print the events instead of posting them")

    legs = sub.add_parser("legs", help="one run's per-leg tables (markdown)")
    legs.add_argument("run_id")

    args = parser.parse_args(argv)
    if args.parse_only:
        print("ci-timings: arguments ok (%s)" % args.command, file=out)
        return 0
    github = GitHub(repo_of(args))
    if args.command == "export":
        if not args.dry_run and not args.vl_url:
            parser.error("export needs --vl-url (or --dry-run)")
        command_export(args, Source(github, None, include_red=True), out)
        return 0
    source = Source(github, args.vl_url, include_red=args.include_red, with_metrics=not args.no_metrics)
    {"report": command_report, "trend": command_trend, "compare": command_compare,
     "legs": command_legs}[args.command](args, source, out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
