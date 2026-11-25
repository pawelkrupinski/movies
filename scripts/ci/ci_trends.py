#!/usr/bin/env python3
"""Where CI's time goes, across many runs: per job and step, setup against work, the critical path, queueing, the
account's runner slots (20, or --slots), caches, machines, CPU/memory/GC, tests -- over a selection of runs, per time bucket with
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
the run's window, sampled every 10 s: the mean busy slots, the peak, and the share of the window all slots were busy (--slots, default 20).

Run:  scripts/ci/ci-timings --help
Test: python3 scripts/ci/ci_trends_test.py
"""
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple, TextIO, cast

# The modules this one was split into; the names below are what the tests and scripts/ci/run_metrics.py import from it.
from trend_events import commit_change as commit_change, events_of as events_of, post_events as post_events
from trend_events import records_from_events as records_from_events, vl_query as vl_query
from trend_export import command_export, load_state as load_state, save_state as save_state
from trend_format import compare_rows, emit, format_compare, format_report, format_trend, leg_tables as leg_tables
from trend_format import format_runs as format_runs, format_value as format_value, mins as mins, pct as pct
from trend_github import DropAuthOnRedirect, GitHub, job_windows, latest_attempts as latest_attempts
from trend_github import parse_metrics as parse_metrics, record_of
from trend_measures import SLOTS, bound_by as bound_by, categorize as categorize, critical_path as critical_path
from trend_measures import gc_summary as gc_summary, job_categories as job_categories, job_resources as job_resources
from trend_measures import run_change, run_measures, run_window, saturation, top_processes as top_processes
from trend_records import Record, epoch as epoch, iso, job_group, job_leg, job_record as job_record
from trend_records import parse_when, run_record as run_record, skew_of as skew_of
from trend_series import bucket_start as bucket_start, buckets as buckets, change as change
from trend_series import regressions as regressions, slope as slope, sparkline as sparkline, trend
from trend_source import FETCH_THREADS as FETCH_THREADS, Source
from trend_summary import metric_rows as metric_rows, summarize

_DropAuthOnRedirect = DropAuthOnRedirect
TIME_UNITS = ("hour", "day", "week")


def _repository_windows(github: GitHub, records: Sequence[Record]) -> List[Tuple[float, float]]:
    """The (start, end) of every job of every completed run of the repository around `records`."""
    spans = [cast(Tuple[float, float], run_window(r)) for r in records]
    start = min(span[0] for span in spans) - 4 * 3600
    end = max(span[1] for span in spans)
    windows: List[Tuple[float, float]] = []
    for run in github.runs(created="%s..%s" % (iso(start)[:10], iso(end)[:10])):
        if run.get("status") == "completed":
            windows += job_windows(github, run)
    return windows


def add_saturation(records: Sequence[Record], github: Optional[GitHub], everything: Optional[List[Tuple[float, float]]] = None,
                   slots: int = SLOTS) -> List[Record]:
    """Each record's slot saturation (see the header), from every job of the repository's runs in its window."""
    kept = [r for r in records if r["jobs"]]
    if not kept:
        return kept
    if everything is None:
        everything = _repository_windows(cast(GitHub, github), kept)
    for record in kept:
        record["sat"] = saturation(run_window(record), everything, slots)
    return kept


# ── grouping ────────────────────────────────────────────────────────────────────────────────────────────────────────

def _machine(job: Record) -> Record:
    return job.get("machine") or {}


ATTRIBUTES: Dict[str, Callable[[Record], Any]] = {
    "cpu-model": lambda job: _machine(job).get("cpu_model"),
    "image": lambda job: _machine(job).get("image_version"),
    "machine": lambda job: "%s · %s cores · %s" % (_machine(job).get("cpu_model"), _machine(job).get("cores"),
                                                   _machine(job).get("image_version")),
    "leg": lambda job: job_leg(job["n"], job.get("label")),
    "group": lambda job: job_group(job["n"]),
    "label": lambda job: job.get("label"),
    "runner-os": lambda job: _machine(job).get("runner_os"),
}

# Attributes of a whole run: `--by` on one keeps each run whole in its group.
RUN_ATTRIBUTES: Dict[str, Callable[[Record], Any]] = {"change": run_change}


def partition(records: Sequence[Record], attribute: str) -> Dict[str, List[Record]]:
    """{value: records holding only that value's jobs}, for `--by <attribute>`."""
    if attribute in RUN_ATTRIBUTES:
        whole: Dict[str, List[Record]] = {}
        for record in records:
            whole.setdefault(RUN_ATTRIBUTES[attribute](record) or "(unknown)", []).append(record)
        return dict(sorted(whole.items()))
    pick = ATTRIBUTES[attribute]
    parts: Dict[str, Dict[Any, Record]] = {}
    for record in records:
        for job in record["jobs"]:
            part = parts.setdefault(pick(job) or "(unknown)", {})
            part.setdefault(record["id"], dict(record, jobs=[]))["jobs"].append(job)
    return {value: list(by_run.values()) for value, by_run in sorted(parts.items())}


# ── commands ────────────────────────────────────────────────────────────────────────────────────────────────────────

def repo_of(args: argparse.Namespace) -> str:
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


def report_text(records: Sequence[Record], args: argparse.Namespace) -> Tuple[Any, str]:
    if args.by and args.by not in TIME_UNITS:
        parts = partition(records, args.by)
        chunks: List[str] = []
        for value, part in parts.items():
            summary = summarize([run_measures(r) for r in part])
            chunks.append("── %s = %s ──\n%s" % (args.by, value, format_report(summary, args.workflow,
                                                                                 critical=args.critical, steps=False,
                                                                                 runs=args.per_run, slots=args.slots)))
        return {value: summarize([run_measures(r) for r in part]) for value, part in parts.items()}, "\n\n".join(chunks)
    summary = summarize([run_measures(r) for r in records])
    return summary, format_report(summary, args.workflow, critical=args.critical, steps=not args.no_steps,
                                  runs=args.per_run, slots=args.slots)


def command_report(args: argparse.Namespace, source: Source, out: TextIO) -> None:
    spec = args.select or ("runs:" + args.runs if args.runs else "%s:%d" % (args.branch, args.last))
    if args.since:
        spec = "%s..%s" % (args.since, args.until or "now")
    records = source.select(spec, args.workflow)
    if args.saturation:
        add_saturation([r for r in records if not r.get("sat")], source.github, slots=args.slots)
    data, text = report_text(records, args)
    emit(data, args.format, text, out)


def _trend_parts(args: argparse.Namespace, records: List[Record], previous: List[Record]
                 ) -> Dict[str, Tuple[List[Record], List[Record]]]:
    """The (records, previous records) each trend is taken over: the whole selection, or one pair per `--by` group."""
    if not args.by or args.by in TIME_UNITS:
        return {"": (records, previous)}
    now_parts, before_parts = partition(records, args.by), partition(previous, args.by)
    return {value: (part, before_parts.get(value, [])) for value, part in now_parts.items()}


def _trend_csv(args: argparse.Namespace, results: Dict[str, Record]) -> Record:
    """One CSV table for a trend per group: every row named by its group."""
    rows: List[Record] = []
    for value, result in results.items():
        rows += [dict(row, row="%s=%s: %s" % (args.by, value, row["row"])) for row in result["rows"]]
    return {"buckets": next(iter(results.values()))["buckets"] if results else [], "rows": rows}


def _trend_of(args: argparse.Namespace, part: List[Record], before: List[Record], since: float, until: float) -> Record:
    """One trend of `part` against the range before it (the `--match` rows only)."""
    every = args.by if args.by in TIME_UNITS else args.every
    result = trend([run_measures(r) for r in part], every, since, until, args.threshold / 100.0,
                   [run_measures(r) for r in before])
    result["compared"] = bool(before)
    if args.match:
        result["rows"] = [row for row in result["rows"] if re.search(args.match, row["row"])]
    return result


def _trend_data(args: argparse.Namespace, results: Dict[str, Record]) -> Record:
    """What `--format json|csv` writes: the one trend, or one per group (a single CSV table for the groups)."""
    if "" in results:
        return results[""]
    return _trend_csv(args, results) if args.format == "csv" else results


def command_trend(args: argparse.Namespace, source: Source, out: TextIO) -> None:
    until = parse_when(args.until or "now")
    since = parse_when(args.since or "14d")
    records = source.window(args.workflow, since, until)
    if args.saturation:
        add_saturation([r for r in records if not r.get("sat")], source.github, slots=args.slots)
    previous = [] if args.no_previous else source.window(args.workflow, since - (until - since), since)
    results = {value: _trend_of(args, part, before, since, until)
               for value, (part, before) in _trend_parts(args, records, previous).items()}
    texts = [("── %s = %s ──\n" % (args.by, value) if value else "") + format_trend(result)
             for value, result in results.items()]
    emit(_trend_data(args, results), args.format, "\n\n".join(texts), out)


def command_compare(args: argparse.Namespace, source: Source, out: TextIO) -> None:
    before = [run_measures(r) for r in source.select(args.before, args.workflow)]
    after = [run_measures(r) for r in source.select(args.after, args.workflow)]
    rows = compare_rows(before, after)
    if args.match:
        rows = [row for row in rows if re.search(args.match, row["row"])]
    emit(rows, args.format, format_compare(rows, len([m for m in before if m]), len([m for m in after if m]),
                                           args.steps), out)


def command_legs(args: argparse.Namespace, source: Source, out: TextIO) -> None:
    """The in-run summary's per-leg tables for one run (ci_timings.py calls leg_tables directly)."""
    run = source.github.run(args.run_id)
    out.write(leg_tables(record_of(source.github, run)))


# ── the command line ────────────────────────────────────────────────────────────────────────────────────────────────

def _common_options(target: argparse.ArgumentParser, top: bool) -> None:
    """THE COMMON OPTIONS GO EITHER SIDE OF THE COMMAND (2026-11-01: the collector on ci-runner ran
    `export ... --vl-url URL`, which only `--vl-url URL export ...` parsed, and failed every pass). Each subcommand
    takes them too, defaulting to SUPPRESS so a value given before the command is never reset by the subparser."""
    def default(value: Any) -> Any:
        return value if top else argparse.SUPPRESS
    target.add_argument("--repo", default=default(None), help="OWNER/REPO (default: origin's)")
    target.add_argument("--vl-url", default=default(os.environ.get("CI_TIMINGS_VL_URL")),
                        help="VictoriaLogs base URL (env CI_TIMINGS_VL_URL); none: the API alone")
    target.add_argument("--slots", type=int, default=default(int(os.environ.get("CI_TIMINGS_SLOTS") or SLOTS)),
                        help="the account's runner slots, for the saturation figures (env CI_TIMINGS_SLOTS; "
                             "default %d, movies')" % SLOTS)
    target.add_argument("--include-red", action="store_true", default=default(False),
                        help="count failed runs too (default: green only)")
    target.add_argument("--no-metrics", action="store_true", default=default(False),
                        help="skip the jobs' metrics artifacts (timings only)")
    target.add_argument("--format", choices=("text", "json", "csv"), default=default("text"))
    target.add_argument("--parse-only", action="store_true", default=default(False),
                        help="check the arguments and exit 0, touching nothing (infra's check of its unit's argv)")


def _subcommand(sub: Any, name: str, **kwargs: Any) -> argparse.ArgumentParser:
    made = sub.add_parser(name, **kwargs)
    _common_options(made, top=False)
    return made


def _add_report(sub: Any, groups: List[str]) -> None:
    report = _subcommand(sub, "report", help="one aggregate over a selection of runs")
    report.add_argument("--workflow", default="main.yml")
    report.add_argument("--branch", default="main")
    report.add_argument("--last", type=int, default=10)
    report.add_argument("--runs", help="run ids, comma-separated")
    report.add_argument("--select", help="a selection (see the header)")
    report.add_argument("--since")
    report.add_argument("--until")
    report.add_argument("--by", choices=groups, help="one report per group")
    report.add_argument("--critical", action="store_true", help="list the critical paths")
    report.add_argument("--per-run", action="store_true", help="one row per run: wall, queue and execution")
    report.add_argument("--saturation", action="store_true", help="measure slot saturation for runs VictoriaLogs "
                                                                  "does not hold (more API calls)")
    report.add_argument("--no-steps", action="store_true")


def _add_trend(sub: Any, groups: List[str]) -> None:
    trend_ = _subcommand(sub, "trend", help="per time bucket, with trends and regressions against the previous range")
    trend_.add_argument("--workflow", default="main.yml")
    trend_.add_argument("--since", default="14d")
    trend_.add_argument("--until")
    trend_.add_argument("--by", default="day", choices=list(TIME_UNITS) + groups,
                        help="hour|day|week (time buckets) or %s (groups)" % "|".join(groups))
    trend_.add_argument("--every", default="day", choices=TIME_UNITS, help="the time bucket under a group --by")
    trend_.add_argument("--threshold", type=float, default=20.0, help="regression: p50 up more than this %%")
    trend_.add_argument("--match", help="only rows matching this regex")
    trend_.add_argument("--no-previous", action="store_true", help="skip the previous range (no regression check)")
    trend_.add_argument("--saturation", action="store_true")


def _add_other_commands(sub: Any) -> None:
    compare = _subcommand(sub, "compare", help="before/after over two selections")
    compare.add_argument("--workflow", default="main.yml")
    compare.add_argument("--before", required=True)
    compare.add_argument("--after", required=True)
    compare.add_argument("--match")
    compare.add_argument("--steps", action="store_true", help="include every step row")

    export = _subcommand(sub, "export", help="the collector: send new runs' events to VictoriaLogs")
    export.add_argument("--since", default="2d", help="90d for the backfill")
    export.add_argument("--until")
    export.add_argument("--state", help="the file of runs already sent")
    export.add_argument("--dry-run", action="store_true", help="print the events instead of posting them")

    legs = _subcommand(sub, "legs", help="one run's per-leg tables (markdown)")
    legs.add_argument("run_id")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="ci-timings", description=(__doc__ or "").split("\n")[0],
                                     formatter_class=argparse.RawDescriptionHelpFormatter, epilog=__doc__)
    _common_options(parser, top=True)
    sub = parser.add_subparsers(dest="command", required=True)
    groups = sorted(set(ATTRIBUTES) | set(RUN_ATTRIBUTES))
    _add_report(sub, groups)
    _add_trend(sub, groups)
    _add_other_commands(sub)
    return parser


_COMMANDS: Dict[str, Callable[[argparse.Namespace, Source, TextIO], None]] = {
    "report": command_report, "trend": command_trend, "compare": command_compare, "legs": command_legs}


def main(argv: Optional[Sequence[str]] = None, out: TextIO = sys.stdout) -> int:
    parser = build_parser()
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
    _COMMANDS[args.command](args, source, out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
