#!/usr/bin/env python3
"""Where a GitHub Actions run spent its time: every job and step, plus the `PhaseTimer` phases a
convergence suite prints -- printed to the run's log and summary, and pushed to Prometheus for the
"CI — build timings" Grafana dashboard (convergence per country first, then every build).

Every workflow runs it as its last job, `timings` (.github/actions/report-timings,
RunTimingsWiringSpec), so it reports the run's OTHER jobs: the run is still in progress, and its
conclusion is read off those jobs rather than the run.

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
of a run hours long -- and the HISTORY: `--history-days` reads every finished run on main of the last
N days off GitHub (steps from its job API; no phases, which only logs hold), and `--push-file` pushes
what it wrote, so the dashboard's trends reach back as far as Prometheus keeps (90 days). A run whose
own `timings` job reported it is left out: it is in Prometheus already, with its phases.

Pushed through grafana.kinowo.net's token-gated OTLP path (roles/public-proxy.nix
`tokenGatedRequests`) with the deploy marker's Grafana token. Pushing never fails the job: a
dashboard is a reading aid, not a gate.

    # TYPE kinowo_ci_run_seconds gauge
    # TYPE kinowo_ci_job_seconds gauge
    # TYPE kinowo_ci_step_seconds gauge
    # TYPE kinowo_ci_phase_seconds gauge

A job's name is the `ci_job` label: Prometheus's own `job` is OTLP's `service.name`, kinowo-ci.
(The `# TYPE` lines are what GrafanaMetricCoverageSpec reads to know these families exist.)

Run:  python3 scripts/ci/ci_timings.py --repo OWNER/REPO --run-id ID [--push] [--exclude-job NAME]
      (by hand, too: re-pushing a finished run backfills it)
      python3 scripts/ci/ci_timings.py --repo OWNER/REPO --history-days 90 --out history.jsonl   # a person's gh
      python3 scripts/ci/ci_timings.py --push-file history.jsonl   # CI: `CI timings backfill` holds the token
Test: python3 scripts/ci/ci_timings_test.py
"""
import argparse
import concurrent.futures
import datetime
import functools
import json
import os
import re
import subprocess
import sys
import urllib.request
from pathlib import Path

# `[uk] bootCutover done in 486.8s`, `[ukp0] replayCutover done in 604.0s`; never the indented
# `[uk]   identityTick done in …` sub-phases (see the header).
PHASE_LINE = re.compile(r"\[([a-z]{2})(p\d+)?\] (\S[^\n]*?) done in ([0-9]+(?:\.[0-9]+)?)s")
# A count inside a phase name (`seed 817 venues into the archive`) moves run to run; `tick 1` does not.
COUNT = re.compile(r"\b\d{3,}\b")
ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
SERVICE_NAME = "kinowo-ci"
# No job this short prints a phase; skipping its log keeps a run's API calls bounded.
PHASE_LOG_MIN_SECONDS = 60
# The job .github/actions/report-timings runs in (`github.job`): a run holding one reported itself.
REPORTER = "timings"
# GitHub answers a filtered run listing with at most 1000 runs; Main runs ~150 a week.
HISTORY_WINDOW_DAYS = 7
# A run that ran no job, or was cancelled part-way (a newer push superseding it, 38% of Main's), says nothing of how long
# the build takes -- and its jobs would cost an API call each.
HISTORY_SKIPPED = {"cancelled", "skipped"}
# A run's jobs asked this many times before the run is skipped (with a warning), and how many runs' jobs are read at once.
HISTORY_ATTEMPTS = 3
# Prometheus takes no sample older than its out-of-order window (prometheus.yaml, 90d) behind now; the margin keeps one
# a few minutes from the edge from crossing it in flight.
PUSH_MAX_AGE = datetime.timedelta(days=90) - datetime.timedelta(hours=1)
HISTORY_READERS = 6


def seconds_between(start, end):
    if not start or not end:
        return None
    return (parse_time(end) - parse_time(start)).total_seconds()


def parse_time(text):
    return datetime.datetime.fromisoformat(text.replace("Z", "+00:00"))


def phase_times(log_text):
    """Top-level PhaseTimer phases of one job's log, longest copy of each."""
    phases = {}
    for match in PHASE_LINE.finditer(ANSI.sub("", log_text)):
        label = COUNT.sub("N", match.group(3).strip())
        phases[label] = max(phases.get(label, 0.0), float(match.group(4)))
    return phases


class Job:
    def __init__(self, job, log_for):
        self.name = job["name"]
        self.conclusion = job.get("conclusion") or "unknown"
        self.started = job["started_at"]
        self.completed = job["completed_at"]
        self.seconds = seconds_between(self.started, self.completed)
        self.steps = []
        for step in job.get("steps", []):
            took = seconds_between(step.get("started_at"), step.get("completed_at"))
            if took is None or step.get("conclusion") == "skipped":
                continue
            self.steps.append((step["name"], took))
        self.phases = phase_times(log_for(job["id"])) if self.seconds >= PHASE_LOG_MIN_SECONDS else {}


def collect(jobs, log_for, exclude=()):
    """The run's finished jobs, longest first. `log_for(job_id)` returns that job's log text;
    `exclude` names jobs to leave out (the in-run reporter itself)."""
    finished = [Job(job, log_for) for job in jobs
                if job.get("status") == "completed" and job.get("conclusion") != "skipped"
                and job["name"] not in exclude and job.get("started_at") and job.get("completed_at")]
    return sorted(finished, key=lambda job: -job.seconds)


def run_start(run):
    return run.get("run_started_at") or run["created_at"]


def run_measures(run, jobs):
    """Wall clock from the first job's start to the last job's end, and how long the run waited
    (its own queue, then for a runner) before any job started."""
    if not jobs:
        return {}
    first = min(job.started for job in jobs)
    last = max(job.completed for job in jobs)
    return {"wall": seconds_between(first, last), "queued": max(0.0, seconds_between(run_start(run), first))}


def conclusion_of(run, jobs):
    """The run's conclusion once it has one (a backfill); while it runs, its jobs' worst."""
    if run.get("conclusion"):
        return run["conclusion"]
    seen = {job.conclusion for job in jobs}
    for worst in ("failure", "timed_out", "cancelled"):
        if worst in seen:
            return worst
    return "success" if jobs else "unknown"


def ref_of(run):
    return "main" if run.get("head_branch") == "main" else "branch"


def minutes(secs):
    return "%dm%02ds" % (secs // 60, secs % 60)


def report(run, jobs, out=sys.stdout):
    measures = run_measures(run, jobs)
    print("%s run %s (%s) -- wall %s, queued %s, %d jobs" % (
        run.get("name", "?"), run["id"], conclusion_of(run, jobs),
        minutes(measures.get("wall", 0)), minutes(measures.get("queued", 0)), len(jobs)), file=out)
    for job in jobs:
        print("", file=out)
        print("%-50s %8s  %s" % (job.name, minutes(job.seconds), job.conclusion), file=out)
        for label, took in job.steps:
            if took >= 1:
                print("    step   %7.0fs  %s" % (took, label), file=out)
        for label, took in sorted(job.phases.items(), key=lambda item: -item[1]):
            print("    phase  %7.1fs  %s" % (took, label), file=out)


def summary_markdown(run, jobs):
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


def otlp_payload(run, jobs):
    """The run as OTLP/JSON gauges, every sample at the run's start (see the header)."""
    if not jobs:
        return None
    stamp = str(int(parse_time(run_start(run)).timestamp()) * 1000000000)
    workflow, ref = run["name"], ref_of(run)

    def point(value, **labels):
        return {"asDouble": float(value), "timeUnixNano": stamp,
                "attributes": [{"key": key, "value": {"stringValue": str(text)}}
                               for key, text in sorted(dict(labels, workflow=workflow, ref=ref).items())]}

    def gauge(name, points):
        return {"name": name, "gauge": {"dataPoints": points}}

    metrics = [
        gauge("kinowo_ci_run_seconds",
              [point(value, measure=measure, conclusion=conclusion_of(run, jobs))
               for measure, value in sorted(run_measures(run, jobs).items())]),
        gauge("kinowo_ci_job_seconds",
              [point(job.seconds, ci_job=job.name, conclusion=job.conclusion) for job in jobs]),
        gauge("kinowo_ci_step_seconds",
              [point(took, ci_job=job.name, step=label) for job in jobs for label, took in merged(job.steps)]),
        gauge("kinowo_ci_phase_seconds",
              [point(took, ci_job=job.name, phase=label) for job in jobs for label, took in sorted(job.phases.items())]),
    ]
    return {"resourceMetrics": [{
        "resource": {"attributes": [{"key": "service.name", "value": {"stringValue": SERVICE_NAME}}]},
        "scopeMetrics": [{"metrics": [m for m in metrics if m["gauge"]["dataPoints"]]}],
    }]}


def merged(steps):
    """A step name that occurs twice in one job (two `Run actions/upload-artifact@v6`) would be two
    samples of one series at one timestamp; sum them instead."""
    totals = {}
    for label, took in steps:
        totals[label] = totals.get(label, 0.0) + took
    return sorted(totals.items())


class NoRedirect(urllib.request.HTTPRedirectHandler):
    """A redirect here is the Google sign-in answering instead of the token gate; following it
    would read the login page's 200 as a successful push."""

    def redirect_request(self, *args, **kwargs):
        return None


def push(payload, url, token, opener=urllib.request.build_opener(NoRedirect).open):
    """POST the payload; a message on any failure, never an exception."""
    if not url or not token:
        return "Grafana secrets unset -- timings not pushed."
    request = urllib.request.Request(
        url.rstrip("/") + "/ci/otlp/v1/metrics", data=json.dumps(payload).encode(), method="POST",
        headers={"Content-Type": "application/json", "Authorization": "Bearer " + token})
    try:
        with opener(request, timeout=20) as response:
            status = response.status
    except Exception as error:  # noqa: BLE001 -- see the header: never fail the job
        return "::warning::timings not pushed: %s" % error
    if status != 200:
        return "::warning::timings not pushed: HTTP %s" % status
    return "Pushed timings."


class GitHub:
    def __init__(self, repo):
        self.repo = repo

    def json(self, path):
        out = subprocess.run(["gh", "api", "--paginate", "repos/%s/%s" % (self.repo, path)],
                             check=True, capture_output=True, text=True).stdout
        # --paginate concatenates one JSON document per page.
        decoder, documents, index = json.JSONDecoder(), [], 0
        while index < len(out):
            if out[index].isspace():
                index += 1
                continue
            document, index = decoder.raw_decode(out, index)
            documents.append(document)
        return documents

    def run(self, run_id):
        return self.json("actions/runs/%s" % run_id)[0]

    def workflows(self):
        return [w["id"] for page in self.json("actions/workflows?per_page=100") for w in page["workflows"]]

    @functools.lru_cache(maxsize=None)
    def workflow_name(self, workflow_id):
        return self.json("actions/workflows/%s" % workflow_id)[0]["name"]

    def workflow_runs(self, workflow_id, since, until):
        """Finished runs on main created in [since, until), dates as YYYY-MM-DD."""
        last = (datetime.date.fromisoformat(until) - datetime.timedelta(days=1)).isoformat()
        return [run for page in self.json("actions/workflows/%s/runs?branch=main&status=completed&per_page=100&created=%s..%s"
                                          % (workflow_id, since, last)) for run in page["workflow_runs"]]

    def jobs(self, run_id):
        return [job for page in self.json("actions/runs/%s/jobs?per_page=100" % run_id) for job in page["jobs"]]

    def log(self, job_id):
        # sbt colours its output, and `gh` refuses to print escape sequences unless told to.
        done = subprocess.run(["gh", "api", "--allow-escape-sequences", "repos/%s/actions/jobs/%s/logs" % (self.repo, job_id)],
                              capture_output=True, text=True)
        return done.stdout if done.returncode == 0 else ""



def named(github, run):
    """The run, named by its workflow: a workflow with a `run-name:` titles each run its own way, and GitHub names the run
    by that title -- charted by it, every identity-measure dispatch would be a workflow of its own."""
    return dict(run, name=github.workflow_name(run["workflow_id"]))


def history(github, days, today=None):
    """One payload per finished run on main of the last `days` days, every workflow's -- but none a
    `timings` job already reported (see the header)."""
    today = today or datetime.datetime.now(datetime.timezone.utc).date()
    start, end = today - datetime.timedelta(days=days), today + datetime.timedelta(days=1)
    for workflow_id in github.workflows():
        since = start
        while since < end:
            until = min(end, since + datetime.timedelta(days=HISTORY_WINDOW_DAYS))
            runs = [run for run in github.workflow_runs(workflow_id, since.isoformat(), until.isoformat())
                    if run.get("conclusion") not in HISTORY_SKIPPED]
            with concurrent.futures.ThreadPoolExecutor(HISTORY_READERS) as readers:
                for run, raw in zip(runs, readers.map(lambda run: jobs_of(github, run), runs)):
                    if raw is None or any(job["name"] == REPORTER for job in raw):
                        continue
                    payload = otlp_payload(named(github, run), collect(raw, lambda _job_id: ""))
                    if payload:
                        yield payload
            since = until


def jobs_of(github, run):
    """The run's jobs, asked up to HISTORY_ATTEMPTS times -- one `gh` call failing is not a run without jobs -- else None,
    and the run is left out of the history with a warning rather than ending it."""
    for attempt in range(1, HISTORY_ATTEMPTS + 1):
        try:
            return github.jobs(run["id"])
        except subprocess.CalledProcessError as error:
            if attempt == HISTORY_ATTEMPTS:
                print("::warning::run %s left out of the history: its jobs could not be read (%s)" % (run["id"], error), file=sys.stderr)
    return None


def started_at(payload):
    """The stamp every sample of a payload carries (see otlp_payload), as a datetime."""
    nanos = payload["resourceMetrics"][0]["scopeMetrics"][0]["metrics"][0]["gauge"]["dataPoints"][0]["timeUnixNano"]
    return datetime.datetime.fromtimestamp(int(nanos) / 1e9, datetime.timezone.utc)


def push_file(path, url, token, batch=25, opener=urllib.request.build_opener(NoRedirect).open, now=None):
    """Push a `--history-days` file, `batch` runs to a request: (runs pushed, runs not). Prometheus refuses a request
    whole for one sample it will not take, so a refused batch is pushed again one run at a time, and only the runs
    refused alone count as not pushed. A run older than PUSH_MAX_AGE is past the out-of-order window -- a file read at
    `--history-days 90` ages into it before it is pushed, and Prometheus answers 400 -- so it is skipped, said so, and
    is neither pushed nor failed: it can never be taken, and failing the push for it would keep the file for ever."""
    now = now or datetime.datetime.now(datetime.timezone.utc)
    payloads = []
    for line in Path(path).read_text().splitlines():
        if not line.strip():
            continue
        payload = json.loads(line)
        stamp = started_at(payload)
        if now - stamp > PUSH_MAX_AGE:
            print("skipped a run started %s: older than Prometheus's 90d window, it cannot be taken" % stamp.isoformat(),
                  file=sys.stderr)
        else:
            payloads.append(payload)

    def pushed_(chunk):
        return push({"resourceMetrics": [rm for payload in chunk for rm in payload["resourceMetrics"]]}, url, token, opener)

    pushed = failed = 0
    for at in range(0, len(payloads), batch):
        chunk = payloads[at:at + batch]
        if pushed_(chunk) == "Pushed timings.":
            pushed += len(chunk)
            continue
        for payload in chunk:
            message = pushed_([payload])
            if message == "Pushed timings.":
                pushed += 1
            else:
                failed += 1
                print("%s (run started %s)" % (message, started_at(payload).isoformat()), file=sys.stderr)
    return pushed, failed


def handle(github, run, args, out=sys.stdout):
    run = named(github, run)
    jobs = collect(github.jobs(run["id"]), github.log, exclude=set(args.exclude_job))
    report(run, jobs, out)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as handle_:
            handle_.write(summary_markdown(run, jobs))
    payload = otlp_payload(run, jobs)
    if args.push and payload:
        print(push(payload, os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN")), file=out)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--repo")
    parser.add_argument("--run-id")
    parser.add_argument("--history-days", type=int, help="write the last N days' runs on main to --out, as payloads")
    parser.add_argument("--out", help="where --history-days writes, one payload a line")
    parser.add_argument("--push-file", help="push what --history-days wrote (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    parser.add_argument("--push", action="store_true", help="push to Prometheus (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    parser.add_argument("--exclude-job", action="append", default=[],
                        help="leave this job out (the reporting job, still running, names itself)")
    args = parser.parse_args(argv)

    if args.push_file:
        pushed, failed = push_file(args.push_file, os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN"))
        print("pushed %d runs, %d not" % (pushed, failed))
        return 1 if failed else 0
    if not args.repo:
        parser.error("--repo is required")
    github = GitHub(args.repo)
    if args.history_days:
        if not args.out:
            parser.error("--history-days needs --out")
        with open(args.out, "w", encoding="utf-8") as out:
            count = 0
            for payload in history(github, args.history_days):
                out.write(json.dumps(payload) + "\n")
                count += 1
        print("wrote %d runs to %s" % (count, args.out))
        return 0
    if not args.run_id:
        parser.error("--run-id is required")
    handle(github, github.run(args.run_id), args)
    return 0


if __name__ == "__main__":
    sys.exit(main())
