#!/usr/bin/env python3
"""A run's jobs' metrics (their ci-metrics-* artifacts, joined to the API's jobs by ci_trends.py) as Report run timings
(ci_timings.py) reports them: the summary's per-leg tables (leg_summary), and each job's CPU: the RUNNER's CPU over the job's window (its steps' first
start to last end), read from the job's `ci-metrics-*` artifact (.github/actions/job-telemetry: cgroup-sampler.sh's
series on Linux, darwin_sampler.py's on macOS; JobTelemetryCoverageSpec holds every job to it), pushed as OTLP gauges
beside the job's time, with ci_timings' labels and its run-start stamp:

    # TYPE kinowo_ci_job_cpu_busy_ratio gauge
    # TYPE kinowo_ci_job_cpu_seconds gauge
    # TYPE kinowo_ci_job_cores gauge
    # TYPE kinowo_ci_job_mem_pressure_ratio gauge
    # TYPE kinowo_ci_step_cpu_busy_ratio gauge

- `cpu_busy_ratio`: busy ticks over all ticks of all cores (near 1 CPU-bound; low, the job waits: network, a lock);
- `cpu_seconds`: the busy core-seconds (ticks are 1/100 s, /proc/stat's and Mach's alike);
- `cores`: the core count, labelled `cpu_model` and `runner_os` (machine-id.sh);
- `mem_pressure_ratio`: PSI memory `some` over the wall (Linux only);
- `kinowo_ci_step_cpu_busy_ratio`: the busy share of each step of 15 s or more.

WHOLE-MACHINE figures: on a ci-runner host serving several jobs at once, each job's reading counts its neighbours too.
A job with no artifact (none uploaded, expired, cancelled) has none of these series.

Test: python3 scripts/ci/ci_timings_test.py (CpuTest, SummaryTest)
"""
import importlib
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple, Union, cast

import ci_machine

TICKS_PER_SECOND = 100.0

Figures = Dict[str, Any]
Record = Dict[str, Any]
Point = Callable[..., Dict[str, Any]]


def busy_seconds(series: Sequence[Dict[str, float]], start: float, end: float) -> Optional[float]:
    """The busy core-seconds of the series' rows inside the window (ci_trends.window_resources' reach)."""
    inside = [row for row in series if start - 6 <= row.get("t", 0) <= end + 6 and "cpu_busy" in row]
    if len(inside) < 2:
        return None
    return round((inside[-1]["cpu_busy"] - inside[0]["cpu_busy"]) / TICKS_PER_SECOND, 1)


def trends() -> Any:
    """ci_trends.py, untyped: imported when first used (it is large, and only a report reads it)."""
    return importlib.import_module("ci_trends")


def metrics_record(run: Record) -> Union[Record, Exception]:
    """The run as ci_trends reads it (record_of); the error when it cannot be read. Never raises: a chart is no gate."""
    try:
        repo = run.get("repository", {}).get("full_name") or ci_machine.environment().get("GITHUB_REPOSITORY", "")
        return cast(Record, trends().record_of(trends().GitHub(repo), dict(run, status="in_progress")))
    except Exception as error:  # noqa: BLE001
        return error


def leg_summary(record: Union[Record, Exception]) -> str:
    """Per leg (convergence's countries; elsewhere per job group): each row's queue, setup by category, work, machine,
    GC and pressure (ci_trends.leg_tables). Never fails."""
    try:
        if isinstance(record, Exception):
            raise record
        tables = cast(str, trends().leg_tables(record))
        return ("\n### Per leg\n" + tables) if tables else ""
    except Exception as error:  # noqa: BLE001
        return "\n(per-leg tables not written: %s)\n" % error


def figures_of(job: Record) -> Optional[Figures]:
    """One job's figures, or None without a series in its window."""
    whole, steps = cast(Tuple[Optional[Figures], Figures], trends().job_resources(job))
    if not whole:
        return None
    machine: Record = job.get("machine") or {}
    return {"busy": whole["cpu"], "seconds": busy_seconds(job.get("series") or [], job["s"], job["e"]),
            "cores": machine.get("cores"), "cpu_model": machine.get("cpu_model") or "unknown",
            "runner_os": machine.get("runner_os") or "unknown", "mem_pressure": whole.get("psi_mem_some"),
            "steps": {name: step["cpu"] for name, step in sorted(steps.items())}}


def job_cpu(record: Union[Record, Exception]) -> Dict[Any, Figures]:
    """{job id: its figures} from the run's record; none when it could not be read."""
    if isinstance(record, Exception):
        return {}
    jobs = cast(List[Record], record.get("jobs") or [])
    found = {job.get("id"): figures_of(job) for job in jobs if job.get("id") is not None}
    return {job_id: figures for job_id, figures in found.items() if figures}


def attach(jobs: Sequence[Any], figures: Dict[Any, Figures]) -> Sequence[Any]:
    """Each ci_timings Job's `cpu`: its figures, or None."""
    for job in jobs:
        job.cpu = figures.get(job.id)
    return jobs


def note(job: Any) -> str:
    """The report line's CPU: ` CPU 75% of 4 cores, 2100 CPU-s`, or nothing."""
    cpu = job.cpu
    if not cpu:
        return ""
    return " CPU %.0f%% of %s cores, %.0f CPU-s" % (cpu["busy"] * 100, cpu.get("cores") or "?", cpu.get("seconds") or 0)


def gauges(jobs: Sequence[Any], point: Point) -> List[Dict[str, Any]]:
    """The OTLP gauges of the header, for the jobs that have figures; `point(value, **labels)` is ci_timings'."""
    measured = [job for job in jobs if job.cpu]

    def each(field: str) -> List[Any]:
        return [(job, job.cpu[field]) for job in measured if job.cpu.get(field) is not None]

    series = {
        "kinowo_ci_job_cpu_busy_ratio": [point(value, ci_job=job.name, conclusion=job.conclusion, runner=job.runner)
                                         for job, value in each("busy")],
        "kinowo_ci_job_cpu_seconds": [point(value, ci_job=job.name, conclusion=job.conclusion, runner=job.runner)
                                      for job, value in each("seconds")],
        "kinowo_ci_job_cores": [point(value, ci_job=job.name, runner=job.runner, cpu_model=job.cpu["cpu_model"],
                                      runner_os=job.cpu["runner_os"]) for job, value in each("cores")],
        "kinowo_ci_job_mem_pressure_ratio": [point(value, ci_job=job.name, runner=job.runner)
                                             for job, value in each("mem_pressure")],
        "kinowo_ci_step_cpu_busy_ratio": [point(value, ci_job=job.name, step=step, runner=job.runner)
                                          for job in measured for step, value in job.cpu["steps"].items()],
    }
    return [{"name": name, "gauge": {"dataPoints": points}} for name, points in series.items()]
