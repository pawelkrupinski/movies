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
of a run hours long. (The 90 days of history from before this reporter were pushed once, 2026-10-14,
by a backfill workflow and a `--history-days` mode since deleted.)

Pushed through grafana.kinowo.net's token-gated OTLP path (roles/public-proxy.nix
`tokenGatedRequests`) with the deploy marker's Grafana token. Pushing never fails the job: a
dashboard is a reading aid, not a gate.

    # TYPE kinowo_ci_run_seconds gauge
    # TYPE kinowo_ci_job_seconds gauge
    # TYPE kinowo_ci_step_seconds gauge
    # TYPE kinowo_ci_phase_seconds gauge
    # TYPE kinowo_ci_job_timeout_seconds gauge
    # TYPE kinowo_ci_run_number gauge

A job's name is the `ci_job` label: Prometheus's own `job` is OTLP's `service.name`, kinowo-ci.
(The `# TYPE` lines are what GrafanaMetricCoverageSpec reads to know these families exist.)

For the build-time alerts (infra/nix/files/monitoring/rules/ci-build-times.rules):

- `kinowo_ci_job_timeout_seconds` is each job's own `timeout-minutes`, beside its duration, so an alert can say a job
  came within 80% of its ceiling BEFORE it hits it. A literal one is read off the workflow file (and the workflows it
  calls); an expression one -- a convergence row's, picked per phase from the caller's matrix -- off the
  `ci-job-ceiling <n> minutes` line the job echoes, since only the job knows what its expression came to. A job with
  none has github.com's 360. WorkflowCeilingSpec (in the test) holds every job to one of the three.
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
import functools
import json
import os
import re
import subprocess
import sys
import urllib.parse
import urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from report_fields import parse_instant  # noqa: E402

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
# github.com's ceiling for a job that names none.
DEFAULT_TIMEOUT_MINUTES = 360
EXPRESSION = re.compile(r"\$\{\{.*?\}\}")
WORKFLOWS = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", ".github", "workflows")


def seconds_between(start, end):
    if not start or not end:
        return None
    return (parse_instant(end) - parse_instant(start)).total_seconds()


def phase_times(log_text):
    """Top-level PhaseTimer phases of one job's log, longest copy of each."""
    phases = {}
    for match in PHASE_LINE.finditer(ANSI.sub("", log_text)):
        label = COUNT.sub("N", match.group(3).strip())
        phases[label] = max(phases.get(label, 0.0), float(match.group(4)))
    return phases


def indent(line):
    return len(line) - len(line.lstrip(" "))


def scalar(value):
    """A plain or quoted YAML scalar without its trailing comment: `40 # longest green row 31 min` is 40, as YAML reads
    it; a quoted one keeps a ` #` inside its quotes."""
    value = value.strip()
    if value[:1] in ("'", '"') and value.find(value[0], 1) > 0:
        return value[1:value.find(value[0], 1)]
    return re.sub(r"(^|\s)#.*$", "", value).strip()


def workflow_jobs(text):
    """Every job of a workflow file: {key: (its top-level attributes, its whole block)}. Read by indentation, not a
    YAML parser: the runners' python has no PyYAML, and a job's keys are the lines one level under its own."""
    lines = text.splitlines()
    try:
        start = lines.index("jobs:") + 1
    except ValueError:
        return {}
    body = []
    for line in lines[start:]:
        if line.strip() and not line.startswith((" ", "#")):
            break
        body.append(line)
    content = [line for line in body if line.strip() and not line.lstrip().startswith("#")]
    if not content:
        return {}
    job_indent = indent(content[0])
    jobs, key, block = {}, None, []
    for line in body + ["end:"]:
        if line.strip() and not line.lstrip().startswith("#") and indent(line) <= job_indent:
            if key:
                inner = [l for l in block if l.strip() and not l.lstrip().startswith("#")]
                level = indent(inner[0]) if inner else 0
                attrs = {}
                for l in inner:
                    if indent(l) == level and ":" in l:
                        name, _, value = l.strip().partition(":")
                        attrs[name] = scalar(value)
                jobs[key] = (attrs, "\n".join(block))
            key, block = line.strip().rstrip(":"), []
        else:
            block.append(line)
    return jobs


def workflow_ceilings(path, root=WORKFLOWS, seen=()):
    """(name pattern, its literal length, ceiling) for each job of a workflow and of the workflows it calls. The
    ceiling is minutes, or None for a call (it runs no steps itself) or an expression (its job echoes it)."""
    file = os.path.join(root, os.path.basename(path))
    if file in seen or not os.path.isfile(file):
        return []
    with open(file, encoding="utf-8") as handle:
        jobs = workflow_jobs(handle.read())
    found = []
    for key, (attrs, _) in jobs.items():
        name = attrs.get("name") or key
        literal = EXPRESSION.split(name)
        pattern = re.compile(".+".join(re.escape(part) for part in literal))
        uses = attrs.get("uses", "")
        timeout = attrs.get("timeout-minutes")
        if uses.startswith("./.github/workflows/"):
            ceiling = None
            found += workflow_ceilings(uses, root, seen + (file,))
        elif timeout is None:
            ceiling = DEFAULT_TIMEOUT_MINUTES
        else:
            ceiling = int(timeout) if timeout.isdigit() else None
        found.append((pattern, sum(map(len, literal)), ceiling))
    return found


def ceiling_of(name, ceilings):
    """A job's ceiling in minutes, from the most specific pattern its name matches: `leg (poland)` is the caller row
    `leg (${{ matrix.country }})`, not the callee's `${{ matrix.phase }} (${{ inputs.country }})`. GitHub prefixes a
    called job with its caller's name (`poland / takes (poland)`); Forgejo does not."""
    matching = [(length, ceiling) for pattern, length, ceiling in ceilings
                if pattern.fullmatch(name) or pattern.fullmatch(name.split(" / ")[-1])]
    return max(matching, key=lambda match: match[0])[1] if matching else None


class Job:
    def __init__(self, job, log_for, ceilings=()):
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
        log = log_for(job["id"]) if self.seconds >= PHASE_LOG_MIN_SECONDS else ""
        self.phases = phase_times(log)
        echoed = CEILING_LINE.search(log)
        minutes_ = int(echoed.group(1)) if echoed else ceiling_of(self.name, ceilings)
        self.ceiling = minutes_ * 60.0 if minutes_ else None


def collect(jobs, log_for, ceilings=()):
    """The run's finished jobs, longest first: the in-run reporter, still in progress, is never one.
    `log_for(job_id)` returns that job's log text; `ceilings` is workflow_ceilings() of the run's workflow."""
    finished = [Job(job, log_for, ceilings) for job in jobs
                if job.get("status") == "completed" and job.get("conclusion") != "skipped"
                and job.get("started_at") and job.get("completed_at")]
    return sorted(finished, key=lambda job: -job.seconds)


def run_start(run):
    return run.get("run_started_at") or run["created_at"]


def run_measures(run, jobs, ready=None):
    """Wall clock from the first job's start to the last job's end, and how long the run waited
    (its own queue, then for a runner) before any job started; with `ready` (ready_at), how long its
    first job waited for a runner."""
    if not jobs:
        return {}
    first = min(job.started for job in jobs)
    last = max(job.completed for job in jobs)
    measures = {"wall": seconds_between(first, last), "queued": max(0.0, seconds_between(run_start(run), first))}
    if ready is not None:
        measures["runner_wait"] = max(0.0, (parse_instant(first) - ready).total_seconds())
    return measures


def ready_at(run, others):
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


def otlp_payload(run, jobs, ready=None):
    """The run as OTLP/JSON gauges, every sample at the run's start (see the header)."""
    if not jobs:
        return None
    stamp = str(int(parse_instant(run_start(run)).timestamp()) * 1000000000)
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
               for measure, value in sorted(run_measures(run, jobs, ready).items())]),
        gauge("kinowo_ci_job_seconds",
              [point(job.seconds, ci_job=job.name, conclusion=job.conclusion) for job in jobs]),
        gauge("kinowo_ci_step_seconds",
              [point(took, ci_job=job.name, step=label) for job in jobs for label, took in merged(job.steps)]),
        gauge("kinowo_ci_phase_seconds",
              [point(took, ci_job=job.name, phase=label) for job in jobs for label, took in sorted(job.phases.items())]),
        gauge("kinowo_ci_job_timeout_seconds",
              [point(job.ceiling, ci_job=job.name) for job in jobs if job.ceiling]),
        gauge("kinowo_ci_run_number", [point(run["run_number"])] if run.get("run_number") else []),
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


# Every network call is bounded (scripts/tool-conventions.sh `bounded`): a hung API never holds the run's last job.
GH_TIMEOUT_SECONDS = 120


class GitHub:
    def __init__(self, repo):
        self.repo = repo

    def json(self, path, paginate=True):
        out = subprocess.run(["gh", "api"] + (["--paginate"] if paginate else []) + ["repos/%s/%s" % (self.repo, path)],
                             check=True, capture_output=True, text=True, timeout=GH_TIMEOUT_SECONDS).stdout
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

    @functools.lru_cache(maxsize=None)
    def workflow_name(self, workflow_id):
        return self.json("actions/workflows/%s" % workflow_id)[0]["name"]

    def recent_runs(self, branch):
        """The newest runs on the branch, one page: the one a run queued behind is among them."""
        page = self.json("actions/runs?branch=%s&per_page=50" % urllib.parse.quote(branch), paginate=False)[0]
        return page["workflow_runs"]

    def jobs(self, run_id):
        return [job for page in self.json("actions/runs/%s/jobs?per_page=100" % run_id) for job in page["jobs"]]

    def log(self, job_id):
        # sbt colours its output, and `gh` refuses to print escape sequences unless told to.
        try:
            done = subprocess.run(["gh", "api", "--allow-escape-sequences", "repos/%s/actions/jobs/%s/logs" % (self.repo, job_id)],
                                  capture_output=True, text=True, timeout=GH_TIMEOUT_SECONDS)
        except subprocess.TimeoutExpired:
            return ""
        return done.stdout if done.returncode == 0 else ""



def named(github, run):
    """The run, named by its workflow: a workflow with a `run-name:` titles each run its own way, and GitHub names the run
    by that title -- charted by it, every identity-measure dispatch would be a workflow of its own."""
    return dict(run, name=github.workflow_name(run["workflow_id"]))


def handle(github, run, args, out=sys.stdout):
    run = named(github, run)
    jobs = collect(github.jobs(run["id"]), github.log, workflow_ceilings(run.get("path") or ""))
    report(run, jobs, out)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as handle_:
            handle_.write(summary_markdown(run, jobs))
    try:
        ready = ready_at(run, github.recent_runs(run["head_branch"])) if run.get("head_branch") else None
    except Exception as error:  # noqa: BLE001 -- see the header: never fail the job
        print("::warning::runner wait not measured: %s" % error, file=out)
        ready = None
    payload = otlp_payload(run, jobs, ready)
    if args.push and payload:
        print(push(payload, os.environ.get("GRAFANA_URL"), os.environ.get("GRAFANA_ANNOTATION_TOKEN")), file=out)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
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
