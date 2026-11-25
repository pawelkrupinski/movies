#!/usr/bin/env python3
"""Where a GitHub Actions run spent its time: every job and step, plus the `PhaseTimer` phases a
convergence suite prints -- printed to the run's log and summary, and pushed to Prometheus for the
"CI — build timings" Grafana dashboard (convergence per country first, then every build).

Report run timings (.github/workflows/report-timings.yml, .github/actions/report-timings,
RunTimingsWiringSpec) runs it once a run has ended -- on `workflow_run: completed`, or dispatched with
the run's id -- and, for the three workflows a GITHUB_TOKEN dispatch starts, as the run's own last job,
`timings`, which reports the run's OTHER jobs while it is still in progress: so the conclusion is read
off the jobs rather than the run.

Sources:

- GitHub's own job and step timestamps.
- The jobs' logs, for `PhaseTimer` lines (`[uk] bootCutover done in 486.8s, cpu ...`). Only
  TOP-LEVEL phases are kept: the indented sub-phases (`[uk]   identityTick done in ...`) of a boot
  and of the order replays running beside it share one scope, so which of them a line belongs to
  cannot be read off the log. A replay pass (`[ukp1]`) reports under its phase name, and a phase
  seen more than once in a job keeps its LONGEST time -- what bounded the wall clock when the
  copies ran side by side. Only logs of jobs over a minute are fetched; no shorter job has phases.

Every sample of a run is stamped at the RUN'S START (`run_started_at`), one point per run on a chart.
Prometheus's out-of-order window (90d, prometheus.yaml) is what admits a sample stamped at the start
of a run hours long. (The 90 days of history from before this reporter were pushed once, 2026-10-14,
by a backfill workflow and a `--history-days` mode since deleted.)

Pushed through grafana.kinowo.net's token-gated OTLP path (roles/public-proxy.nix
`tokenGatedRequests`) with the deploy marker's Grafana token. Pushing never fails the job: a
dashboard is a reading aid, not a gate.

    # TYPE kinowo_ci_run_seconds gauge
    # TYPE kinowo_ci_job_seconds gauge
    # TYPE kinowo_ci_step_seconds gauge
    # TYPE kinowo_ci_phase_seconds gauge
    # TYPE kinowo_ci_job_queue_seconds gauge
    # TYPE kinowo_ci_job_timeout_seconds gauge
    # TYPE kinowo_ci_run_number gauge

Every series carries `workflow`, `ref` (`main` | `branch`, see ref_of) and `runner`. The job, step, phase, queue and timeout
series carry the job's (`ci-runner` self-hosted, `hosted` GitHub's, else `unknown`); the run-level ones (run_seconds,
run_number) the whole run's: `hosted`, `ci-runner`, `mixed` or `unknown`. Read off the jobs API's `labels` / `runner_name`; `kinowo_ci_job_queue_seconds` is the job's `created_at` to
`started_at`, so the two kinds compare. A job's name is the `ci_job` label: Prometheus's own `job` is OTLP's `service.name`, kinowo-ci.
(The `# TYPE` lines are what GrafanaMetricCoverageSpec reads to know these families exist.)

For the build-time alerts (infra/nix/files/monitoring/rules/ci-build-times.rules):

- `kinowo_ci_job_timeout_seconds` is each job's own `timeout-minutes`, beside its duration, so an alert can say a job
  came within 80% of its ceiling BEFORE it hits it. A literal one is read off the workflow file (and the workflows it
  calls); an expression one -- a convergence row's, picked per phase from the caller's matrix -- off the
  `ci-job-ceiling <n> minutes` line the job echoes, since only the job knows what its expression came to. A job with
  none has github.com's 360. WorkflowCeilingSpec (in the test) holds every job to one of the three.
- each job's CPU (`kinowo_ci_job_cpu_*`, `kinowo_ci_step_cpu_busy_ratio`): run_metrics.py, from the jobs' metrics artifacts.
- `kinowo_ci_run_number` is the run's index (the number in its URL), so an alert can name the runs a regression
  started between -- a commit SHA would be a label of its own per run.
- `kinowo_ci_run_seconds{measure="runner_wait"}` is how long the run's first job waited for a runner once nothing
  else held it back: from the run's creation, or from the end of the same workflow's run on its branch it queued
  behind (Main runs one at a time), to its first job's start.

Run:  python3 scripts/ci/ci_timings.py --repo OWNER/REPO --run-id ID [--push]
      (by hand, too: re-pushing a finished run backfills it)
Test: python3 scripts/ci/ci_timings_test.py
"""
import argparse
import datetime
import functools
import json
import os
import re
import subprocess
import sys
import urllib.parse
from typing import Any, Callable, Dict, Iterable, List, Optional, Sequence, TextIO, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import grafana_otlp  # noqa: E402
import run_metrics  # noqa: E402
from report_fields import parse_instant  # noqa: E402
from workflow_ceilings import Ceiling, ceiling_of, workflow_ceilings  # noqa: E402
# Re-exported: ci_timings_test reads the workflow files' parsing through this module.
from workflow_ceilings import DEFAULT_TIMEOUT_MINUTES as DEFAULT_TIMEOUT_MINUTES  # noqa: E402
from workflow_ceilings import WORKFLOWS as WORKFLOWS  # noqa: E402
from workflow_ceilings import scalar as scalar, workflow_jobs as workflow_jobs  # noqa: E402

Record = Dict[str, Any]
LogFor = Callable[[Any], str]

# `[uk] bootCutover done in 486.8s`, `[ukp0] replayCutover done in 604.0s`; never the indented
# `[uk]   identityTick done in …` sub-phases (see the header).
PHASE_LINE = re.compile(r"\[([a-z]{2})(p\d+)?\] (\S[^\n]*?) done in ([0-9]+(?:\.[0-9]+)?)s")
# A count inside a phase name (`seed 817 venues into the archive`) moves run to run; `tick 1` does not.
COUNT = re.compile(r"\b\d{3,}\b")
ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
SERVICE_NAME = "kinowo-ci"
# No job this short prints a phase; skipping its log keeps a run's API calls bounded.
PHASE_LOG_MIN_SECONDS = 60
# What an expression `timeout-minutes` came to, echoed by its job (country-convergence-leg.yml); see the header.
CEILING_LINE = re.compile(r"ci-job-ceiling ([0-9]+) minutes")
# The order the gauges are pushed in.
GAUGE_ORDER = ("kinowo_ci_run_seconds", "kinowo_ci_job_seconds", "kinowo_ci_job_queue_seconds", "kinowo_ci_step_seconds",
               "kinowo_ci_phase_seconds", "kinowo_ci_job_timeout_seconds", "kinowo_ci_run_number")


def seconds_between(start: Optional[str], end: Optional[str]) -> Optional[float]:
    if not start or not end:
        return None
    return (parse_instant(end) - parse_instant(start)).total_seconds()


def phase_times(log_text: str) -> Dict[str, float]:
    """Top-level PhaseTimer phases of one job's log, longest copy of each."""
    phases: Dict[str, float] = {}
    for match in PHASE_LINE.finditer(ANSI.sub("", log_text)):
        label = COUNT.sub("N", match.group(3).strip())
        phases[label] = max(phases.get(label, 0.0), float(match.group(4)))
    return phases


def runner_kind(job: Record) -> str:
    """Where a job ran, from the jobs API's `labels` and `runner_name`: `ci-runner` (self-hosted: label `self-hosted`
    or a runner named `ci-runner-*`), `hosted` (a runner named `GitHub Actions N`, or an `ubuntu-*` label), else `unknown`
    (a job that never got a runner has neither)."""
    labels: List[str] = job.get("labels") or []
    name: str = job.get("runner_name") or ""
    if "self-hosted" in labels or name.startswith("ci-runner"):
        return "ci-runner"
    if name.startswith("GitHub Actions") or any(label.startswith("ubuntu-") for label in labels):
        return "hosted"
    return "unknown"


class Job:
    def __init__(self, job: Record, log_for: LogFor, ceilings: Iterable[Ceiling] = ()) -> None:
        self.id = job.get("id")
        self.name: str = job["name"]
        self.cpu: Optional[Any] = None  # run_metrics.attach: the runner's CPU over the job, when it uploaded its metrics
        self.runner = runner_kind(job)
        self.queued = seconds_between(job.get("created_at"), job.get("started_at"))
        self.conclusion = job.get("conclusion") or "unknown"
        self.started = job["started_at"]
        self.completed = job["completed_at"]
        self.seconds: float = seconds_between(self.started, self.completed) or 0.0
        self.steps: List[Tuple[str, float]] = []
        for step in job.get("steps", []):
            took = seconds_between(step.get("started_at"), step.get("completed_at"))
            if took is None or step.get("conclusion") == "skipped":
                continue
            self.steps.append((step["name"], took))
        log = log_for(job["id"]) if self.seconds >= PHASE_LOG_MIN_SECONDS else ""
        self.phases = phase_times(log)
        echoed = CEILING_LINE.search(log)
        minutes_ = int(echoed.group(1)) if echoed else ceiling_of(self.name, ceilings)
        self.ceiling: Optional[float] = minutes_ * 60.0 if minutes_ else None


def collect(jobs: Iterable[Record], log_for: LogFor, ceilings: Iterable[Ceiling] = ()) -> List[Job]:
    """The run's finished jobs, longest first: the in-run reporter, still in progress, is never one.
    `log_for(job_id)` returns that job's log text; `ceilings` is workflow_ceilings() of the run's workflow."""
    finished = [Job(job, log_for, tuple(ceilings)) for job in jobs
                if job.get("status") == "completed" and job.get("conclusion") != "skipped"
                and job.get("started_at") and job.get("completed_at")]
    return sorted(finished, key=lambda job: -job.seconds)


def run_start(run: Record) -> str:
    return run.get("run_started_at") or run["created_at"]


def run_measures(run: Record, jobs: Sequence[Job], ready: Optional[datetime.datetime] = None) -> Dict[str, float]:
    """Wall clock from the first job's start to the last job's end, and how long the run waited
    (its own queue, then for a runner) before any job started; with `ready` (ready_at), how long its
    first job waited for a runner."""
    if not jobs:
        return {}
    first = min(job.started for job in jobs)
    last = max(job.completed for job in jobs)
    measures = {"wall": seconds_between(first, last) or 0.0,
                "queued": max(0.0, seconds_between(run_start(run), first) or 0.0)}
    if ready is not None:
        measures["runner_wait"] = max(0.0, (parse_instant(first) - ready).total_seconds())
    return measures


def ready_at(run: Record, others: Iterable[Record]) -> datetime.datetime:
    """When nothing but a runner held the run back: its creation, or the end of an earlier run of the same workflow on
    its branch that was still going then -- the one it queued behind, for a workflow that runs one at a time (Main).
    For one that runs side by side it reads late, which only ever understates the wait."""
    created = parse_instant(run["created_at"])
    ready = created
    for other in others:
        if (other.get("id") == run.get("id") or other.get("path") != run.get("path")
                or other.get("head_branch") != run.get("head_branch") or other.get("status") != "completed"
                or not other.get("updated_at") or parse_instant(other["created_at"]) >= created):
            continue
        ready = max(ready, parse_instant(other["updated_at"]))
    return ready


def conclusion_of(run: Record, jobs: Sequence[Job]) -> str:
    """The run's conclusion once it has one (a backfill); while it runs, its jobs' worst."""
    if run.get("conclusion"):
        return run["conclusion"]
    seen = {job.conclusion for job in jobs}
    for worst in ("failure", "timed_out", "cancelled"):
        if worst in seen:
            return worst
    return "success" if jobs else "unknown"


def ref_of(run: Record) -> str:
    """`main` or `branch`, nothing else: a run of the branch literally named `main` (push, workflow_dispatch, schedule --
    GitHub runs a schedule on the default branch -- or a workflow_run of one of those); every other run -- a measure/* or
    land/* branch, a bot's pull_request (even one from a fork whose head branch is also called `main`) -- is `branch`."""
    return "main" if run.get("head_branch") == "main" and run.get("event") != "pull_request" else "branch"


def run_runner(jobs: Sequence[Job]) -> str:
    """Where the whole run ran: `hosted`, `ci-runner`, `mixed` (jobs on both), or `unknown` (no job got a runner)."""
    kinds = {job.runner for job in jobs} - {"unknown"}
    return "mixed" if len(kinds) > 1 else next(iter(kinds), "unknown")


def minutes(secs: float) -> str:
    return "%dm%02ds" % (secs // 60, secs % 60)


def report(run: Record, jobs: Sequence[Job], out: TextIO = sys.stdout) -> None:
    measures = run_measures(run, jobs)
    print("%s run %s (%s) -- wall %s, queued %s, %d jobs" % (
        run.get("name", "?"), run["id"], conclusion_of(run, jobs),
        minutes(measures.get("wall", 0)), minutes(measures.get("queued", 0)), len(jobs)), file=out)
    for job in jobs:
        print("", file=out)
        print("%-50s %8s  %s%s" % (job.name, minutes(job.seconds), job.conclusion, run_metrics.note(job)), file=out)
        for label, took in job.steps:
            if took >= 1:
                print("    step   %7.0fs  %s" % (took, label), file=out)
        for label, took in sorted(job.phases.items(), key=lambda item: -item[1]):
            print("    phase  %7.1fs  %s" % (took, label), file=out)


def summary_markdown(run: Record, jobs: Sequence[Job]) -> str:
    measures = run_measures(run, jobs)
    lines = ["### Timings",
             "",
             "Wall **%s** (first job start to last job end), queued %s." % (
                 minutes(measures.get("wall", 0)), minutes(measures.get("queued", 0))),
             "",
             "| job | total | longest step | longest phases |",
             "|---|---|---|---|"]
    for job in jobs:
        step = max(job.steps, key=lambda item: item[1], default=None)
        longest = ", ".join("%s %.0fs" % (label, took)
                            for label, took in sorted(job.phases.items(), key=lambda item: -item[1])[:4])
        lines.append("| %s | %s | %s | %s |" % (
            job.name, minutes(job.seconds), "%s %s" % (step[0], minutes(step[1])) if step else "-", longest or "-"))
    return "\n".join(lines) + "\n"


def gauge_point(stamp: str, shared: Dict[str, str], value: float, **labels: Any) -> Dict[str, Any]:
    """One OTLP data point at `stamp`: its own labels plus the run's (`workflow`, `ref`)."""
    return {"asDouble": float(value), "timeUnixNano": stamp,
            "attributes": [{"key": key, "value": {"stringValue": str(text)}}
                           for key, text in sorted(dict(labels, **shared).items())]}


def gauge(name: str, points: List[Dict[str, Any]]) -> Dict[str, Any]:
    return {"name": name, "gauge": {"dataPoints": points}}


def run_gauges(run: Record, jobs: Sequence[Job], ready: Optional[datetime.datetime],
               point: run_metrics.Point) -> List[Dict[str, Any]]:
    """The gauges of the run as a whole: its measures and its number."""
    run_wide = run_runner(jobs)
    return [
        gauge("kinowo_ci_run_seconds",
              [point(value, measure=measure, conclusion=conclusion_of(run, jobs), runner=run_wide)
               for measure, value in sorted(run_measures(run, jobs, ready).items())]),
        gauge("kinowo_ci_run_number", [point(run["run_number"], runner=run_wide)] if run.get("run_number") else []),
    ]


def job_gauges(jobs: Sequence[Job], point: run_metrics.Point) -> List[Dict[str, Any]]:
    return [
        gauge("kinowo_ci_job_seconds",
              [point(job.seconds, ci_job=job.name, conclusion=job.conclusion, runner=job.runner) for job in jobs]),
        gauge("kinowo_ci_job_queue_seconds",
              [point(max(0.0, job.queued), ci_job=job.name, conclusion=job.conclusion, runner=job.runner)
               for job in jobs if job.queued is not None]),
        gauge("kinowo_ci_step_seconds",
              [point(took, ci_job=job.name, step=label, runner=job.runner) for job in jobs for label, took in merged(job.steps)]),
        gauge("kinowo_ci_phase_seconds",
              [point(took, ci_job=job.name, phase=label, runner=job.runner) for job in jobs for label, took in sorted(job.phases.items())]),
        gauge("kinowo_ci_job_timeout_seconds",
              [point(job.ceiling, ci_job=job.name, runner=job.runner) for job in jobs if job.ceiling]),
    ]


def otlp_payload(run: Record, jobs: Sequence[Job], ready: Optional[datetime.datetime] = None) -> Optional[Dict[str, Any]]:
    """The run as OTLP/JSON gauges, every sample at the run's start (see the header)."""
    if not jobs:
        return None
    stamp = str(int(parse_instant(run_start(run)).timestamp()) * 1000000000)
    shared = {"workflow": run["name"], "ref": ref_of(run)}
    point = functools.partial(gauge_point, stamp, shared)
    by_name = {g["name"]: g for g in run_gauges(run, jobs, ready, point) + job_gauges(jobs, point)}
    ordered = [by_name[name] for name in GAUGE_ORDER]
    metrics = ordered + run_metrics.gauges(jobs, point)
    return {"resourceMetrics": [{
        "resource": {"attributes": [{"key": "service.name", "value": {"stringValue": SERVICE_NAME}}]},
        "scopeMetrics": [{"metrics": [m for m in metrics if m["gauge"]["dataPoints"]]}],
    }]}


def merged(steps: Iterable[Tuple[str, float]]) -> List[Tuple[str, float]]:
    """A step name that occurs twice in one job (two `Run actions/upload-artifact@v6`) would be two
    samples of one series at one timestamp; sum them instead."""
    totals: Dict[str, float] = {}
    for label, took in steps:
        totals[label] = totals.get(label, 0.0) + took
    return sorted(totals.items())


def push(payload: Dict[str, Any], url: Optional[str], token: Optional[str], opener: Callable[..., Any] = grafana_otlp.OPEN) -> str:
    """POST the payload; a message on any failure, never an exception."""
    if not url or not token:
        return "Grafana secrets unset -- timings not pushed."
    failed = grafana_otlp.post(payload, url, token, opener)
    return "::warning::timings not pushed: %s" % failed if failed else "Pushed timings."


# Every network call is bounded (scripts/tool-conventions.sh `bounded`): a hung API never holds the run's last job.
GH_TIMEOUT_SECONDS = 120


class GitHub:
    def __init__(self, repo: str) -> None:
        self.repo = repo

    def json(self, path: str, paginate: bool = True) -> List[Any]:
        out = subprocess.run(["gh", "api"] + (["--paginate"] if paginate else []) + ["repos/%s/%s" % (self.repo, path)],
                             check=True, capture_output=True, text=True, timeout=GH_TIMEOUT_SECONDS).stdout
        # --paginate concatenates one JSON document per page.
        decoder, index = json.JSONDecoder(), 0
        documents: List[Any] = []
        while index < len(out):
            if out[index].isspace():
                index += 1
                continue
            document, index = decoder.raw_decode(out, index)
            documents.append(document)
        return documents

    def run(self, run_id: str) -> Record:
        return self.json("actions/runs/%s" % run_id)[0]

    @functools.lru_cache(maxsize=None)
    def workflow_name(self, workflow_id: int) -> str:
        return self.json("actions/workflows/%s" % workflow_id)[0]["name"]

    def recent_runs(self, branch: str) -> List[Record]:
        """The newest runs on the branch, one page: the one a run queued behind is among them."""
        page = self.json("actions/runs?branch=%s&per_page=50" % urllib.parse.quote(branch), paginate=False)[0]
        return page["workflow_runs"]

    def jobs(self, run_id: Any) -> List[Record]:
        return [job for page in self.json("actions/runs/%s/jobs?per_page=100" % run_id) for job in page["jobs"]]

    def log(self, job_id: Any) -> str:
        # sbt colours its output, and `gh` refuses to print escape sequences unless told to.
        try:
            done = subprocess.run(["gh", "api", "--allow-escape-sequences", "repos/%s/actions/jobs/%s/logs" % (self.repo, job_id)],
                                  capture_output=True, text=True, timeout=GH_TIMEOUT_SECONDS)
        except subprocess.TimeoutExpired:
            return ""
        return done.stdout if done.returncode == 0 else ""



def named(github: GitHub, run: Record) -> Record:
    """The run, named by its workflow: a workflow with a `run-name:` titles each run its own way, and GitHub names the run
    by that title -- charted by it, every identity-measure dispatch would be a workflow of its own."""
    return dict(run, name=github.workflow_name(run["workflow_id"]))


def handle(github: GitHub, run: Record, args: argparse.Namespace, out: TextIO = sys.stdout) -> None:
    run = named(github, run)
    jobs = collect(github.jobs(run["id"]), github.log, workflow_ceilings(run.get("path") or ""))
    record = run_metrics.metrics_record(run)
    run_metrics.attach(jobs, run_metrics.job_cpu(record))
    report(run, jobs, out)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as handle_:
            handle_.write(summary_markdown(run, jobs))
            handle_.write(run_metrics.leg_summary(record))
    try:
        ready = ready_at(run, github.recent_runs(run["head_branch"])) if run.get("head_branch") else None
    except Exception as error:  # noqa: BLE001 -- see the header: never fail the job
        print("::warning::runner wait not measured: %s" % error, file=out)
        ready = None
    payload = otlp_payload(run, jobs, ready)
    if args.push and payload:
        print(push(payload, os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN")), file=out)


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=(__doc__ or "").split("\n")[0])
    parser.add_argument("--repo")
    parser.add_argument("--run-id")
    parser.add_argument("--push", action="store_true", help="push to Prometheus (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    args = parser.parse_args(argv)

    if not args.repo:
        parser.error("--repo is required")
    github = GitHub(args.repo)
    if not args.run_id:
        parser.error("--run-id is required")
    handle(github, github.run(args.run_id), args)
    return 0


if __name__ == "__main__":
    sys.exit(main())
