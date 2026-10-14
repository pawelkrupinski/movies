#!/usr/bin/env python3
"""Where a GitHub Actions run spent its time: every job and step, plus the `PhaseTimer` phases a
convergence suite prints -- printed to the log (and the run summary), and pushed to Prometheus for
the "CI — build timings" and "CI — identity model convergence timings" Grafana dashboards.

Sources, read once the run (or, in-run, the jobs before this one) have finished:

- GitHub's own job and step timestamps, for every workflow.
- The jobs' logs, for `PhaseTimer` lines (`[uk] bootCutover done in 486.8s, cpu ...`). Only
  TOP-LEVEL phases are kept: the indented sub-phases (`[uk]   identityTick done in ...`) of a boot
  and of the order replays running beside it share one scope, so which of them a line belongs to
  cannot be read off the log. A replay pass (`[ukp1]`) reports under its phase name, and a phase
  seen more than once in a job keeps its LONGEST time -- what bounded the wall clock when the
  copies ran side by side. Only logs of jobs over a minute are fetched; no shorter job has phases.

EVERY SAMPLE OF A RUN IS STAMPED AT THE RUN'S START (`run_started_at`), and every value is a pure
function of the finished run. So pushing a run twice -- `workflow_run` and the sweep both seeing it
-- writes identical samples, which Prometheus accepts as duplicates; a different value at one
timestamp would be rejected. Prometheus's out-of-order window (12h, prometheus.yaml) is what admits
a sample stamped at the start of a run hours long.

Pushed through grafana.kinowo.net's token-gated OTLP path (roles/public-proxy.nix
`tokenGatedRequests`) with the deploy marker's Grafana token. Pushing never fails the job: a
dashboard is a reading aid, not a gate.

    # TYPE kinowo_ci_run_seconds gauge
    # TYPE kinowo_ci_job_seconds gauge
    # TYPE kinowo_ci_step_seconds gauge
    # TYPE kinowo_ci_phase_seconds gauge

A job's name is the `ci_job` label: Prometheus's own `job` is OTLP's `service.name`, kinowo-ci.

(The `# TYPE` lines are what GrafanaMetricCoverageSpec reads to know these families exist.)

Run:  python3 scripts/ci/ci_timings.py --repo OWNER/REPO --run-id ID [--push]
      python3 scripts/ci/ci_timings.py --repo OWNER/REPO --since-minutes 180 [--push]   (the sweep)
Test: python3 scripts/ci/ci_timings_test.py
"""
import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import urllib.request

# `[uk] bootCutover done in 486.8s`, `[ukp0] replayCutover done in 604.0s`; never the indented
# `[uk]   identityTick done in …` sub-phases (see the header).
PHASE_LINE = re.compile(r"\[([a-z]{2})(p\d+)?\] (\S[^\n]*?) done in ([0-9]+(?:\.[0-9]+)?)s")
# A count inside a phase name (`seed 817 venues into the archive`) moves run to run; `tick 1` does not.
COUNT = re.compile(r"\b\d{3,}\b")
ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
SERVICE_NAME = "kinowo-ci"
# The collector's own runs are not builds.
OWN_WORKFLOW = "CI timings"
# No job this short prints a phase; skipping its log keeps a sweep's API calls bounded.
PHASE_LOG_MIN_SECONDS = 60


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


def ref_of(run):
    return "main" if run.get("head_branch") == "main" else "branch"


def minutes(secs):
    return "%dm%02ds" % (secs // 60, secs % 60)


def report(run, jobs, out=sys.stdout):
    measures = run_measures(run, jobs)
    print("%s run %s (%s) -- wall %s, queued %s, %d jobs" % (
        run.get("name", "?"), run["id"], run.get("conclusion") or "in progress",
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
              [point(value, measure=measure, conclusion=run.get("conclusion") or "unknown")
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


# Runs another workflow started with the GITHUB_TOKEN raise no `workflow_run` -- Main's convergence
# dispatches, the bisect -- so the sweep is the only collector that ever sees them.
TOKEN_ACTOR = "github-actions[bot]"


def workflow_run_names(workflow_yaml):
    """The workflows ci-timings.yml's `workflow_run` lists: the ones reported as they finish."""
    names, inside = [], False
    for line in workflow_yaml.splitlines():
        if line.strip() == "workflows:":
            inside = True
        elif inside and line.strip().startswith("- "):
            names.append(line.strip()[2:].strip())
        elif inside:
            break
    return set(names)


def due_for_sweep(runs, finished_after, reported_on_completion):
    """Completed runs that finished inside the window (a run's `updated_at` is when it finished)
    and that no `workflow_run` reported: an unlisted workflow's, or one the GITHUB_TOKEN started.
    The collector's own excluded."""
    return [run for run in runs
            if run.get("name") != OWN_WORKFLOW and run.get("status") == "completed"
            and (run.get("name") not in reported_on_completion
                 or (run.get("triggering_actor") or {}).get("login") == TOKEN_ACTOR)
            and parse_time(run["updated_at"]) >= finished_after]


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

    def jobs(self, run_id):
        return [job for page in self.json("actions/runs/%s/jobs?per_page=100" % run_id) for job in page["jobs"]]

    def log(self, job_id):
        # sbt colours its output, and `gh` refuses to print escape sequences unless told to.
        done = subprocess.run(["gh", "api", "--allow-escape-sequences", "repos/%s/actions/jobs/%s/logs" % (self.repo, job_id)],
                              capture_output=True, text=True)
        return done.stdout if done.returncode == 0 else ""

    def completed_created_since(self, since):
        stamp = since.strftime("%Y-%m-%dT%H:%M:%SZ")
        return [run for page in self.json("actions/runs?status=completed&per_page=100&created=%%3E%%3D%s" % stamp)
                for run in page["workflow_runs"]]


def handle(github, run, args, out=sys.stdout):
    jobs = collect(github.jobs(run["id"]), github.log, exclude=set(args.exclude_job))
    report(run, jobs, out)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary and args.run_id:
        with open(summary, "a", encoding="utf-8") as handle_:
            handle_.write(summary_markdown(run, jobs))
    payload = otlp_payload(run, jobs)
    if args.push and payload:
        print(push(payload, os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN")), file=out)


def main(argv=None, now=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--repo", required=True)
    which = parser.add_mutually_exclusive_group(required=True)
    which.add_argument("--run-id", help="report this run")
    which.add_argument("--since-minutes", type=int,
                       help="the sweep: every run no workflow_run reported that finished in the last N minutes")
    parser.add_argument("--workflow-file", default=".github/workflows/ci-timings.yml",
                        help="the collector's workflow, whose workflow_run list the sweep leaves alone")
    parser.add_argument("--push", action="store_true", help="push to Prometheus (GRAFANA_URL, GRAFANA_ANNOTATION_TOKEN)")
    parser.add_argument("--exclude-job", action="append", default=[],
                        help="leave this job out (an in-run reporter, still running, names itself)")
    args = parser.parse_args(argv)

    github = GitHub(args.repo)
    if args.run_id:
        runs = [github.run(args.run_id)]
    else:
        now = now or datetime.datetime.now(datetime.timezone.utc)
        window = datetime.timedelta(minutes=args.since_minutes)
        # Created up to a day before the window: a six-hour convergence run created yesterday
        # evening finished inside it.
        with open(args.workflow_file, encoding="utf-8") as handle_:
            listed = workflow_run_names(handle_.read())
        runs = due_for_sweep(github.completed_created_since(now - window - datetime.timedelta(hours=24)), now - window, listed)
    for run in runs:
        handle(github, run, args)
        print("")
    return 0


if __name__ == "__main__":
    sys.exit(main())
