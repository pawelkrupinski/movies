#!/usr/bin/env python3
"""ONE RESOLVER FOR UPSTREAM RUNS: which run of the producing workflow a workflow_run-triggered workflow reads.

Every workflow started by another workflow's run picks its input here, through .github/actions/upstream-run in its plan
job (UpstreamRunSpec holds them to it). Before, each chose for itself, and each choice went wrong once:

- the live drift check ran only after a SUCCESSFUL recording and skipped its one job otherwise -- and a run whose only
  job is skipped shows green: run index 1493 compared nothing after recording 1476 failed;
- the findings report took the triggering run whatever its end: run 3139 went red with "run 3136 left no country's
  findings" after recording 3136 failed before any convergence row wrote them -- red for lack of input, not a finding.

The rule, one for all:

- a run dispatched by hand reads the run it names (`run-id`), whatever its age: someone chose it;
- a producing run on main that succeeded is read;
- one that failed, did not end in success, or ran off main is passed over for the NEWEST successful run on main, said as
  a warning -- and when none is newer than --max-age-hours (default 48; 0 lifts the limit), the step fails: a stale input
  checks nothing. With --on-fallback skip it is passed over for nothing instead (an empty run, said as a notice): for a
  consumer whose result on an older input would only repeat what it already said;
- no run named and no trigger (a scheduled backstop) takes the newest successful run on main, as a fallback.

Prints the run id on stdout (empty when skipped); every reason on stderr, as a GitHub annotation. Lists runs through
scripts/forge-gh (gh on GitHub, Forgejo's API on a Forgejo checkout), or $KINOWO_GH.

Run:  python3 scripts/ci/upstream_run.py --workflow NAME [--trigger RUN] [--conclusion C --branch B] [--repo O/R]
                                         [--max-age-hours 48] [--on-fallback newest|skip]
Test: python3 scripts/ci/upstream_run_test.py
"""
import argparse
import datetime
import json
import os
import subprocess
import sys
from pathlib import Path

MAX_AGE_HOURS = 48


def successful_runs(workflow, repo, gh):
    """The workflow's successful runs on main, newest first by creation time (a status-filtered listing is not in
    creation order: identity_newest_recording in scripts/identity-tools.sh)."""
    out = subprocess.run([gh, "run", "list", "-R", repo, "--workflow", workflow, "--status", "success", "--branch", "main",
                          "-L", "50", "--json", "databaseId,createdAt"], check=True, capture_output=True, text=True).stdout
    runs = [(r["databaseId"], parsed(r["createdAt"])) for r in json.loads(out or "[]")]
    return sorted(runs, key=lambda r: r[1], reverse=True)


def parsed(stamp):
    return datetime.datetime.fromisoformat(stamp.replace("Z", "+00:00"))


def taken_as_is(trigger, conclusion, branch):
    """The trigger itself is read: dispatched by hand, or a success on main."""
    return bool(trigger) and (not conclusion or (conclusion == "success" and branch == "main"))


def choose(trigger, conclusion, branch, runs, now, max_age_hours=MAX_AGE_HOURS, workflow="the producing workflow",
           on_fallback="newest"):
    """(run id, message) for the run read; ("", message) when skipped; (None, message) when none may be.
    `max_age_hours` 0 is no limit."""
    if trigger and not conclusion:
        return trigger, "reading %s run %s, as dispatched" % (workflow, trigger)
    if taken_as_is(trigger, conclusion, branch):
        return trigger, "reading %s run %s, which just succeeded" % (workflow, trigger)
    why = ("no %s run named" % workflow if not trigger else
           "%s run %s %s" % (workflow, trigger, "ran off main (%s)" % branch if branch != "main" else "ended %s" % conclusion))
    if trigger and on_fallback == "skip":
        return "", "%s: nothing to read, and this workflow reads no older run in its place" % why
    fresh = [(run, at) for run, at in runs if not max_age_hours or now - at <= datetime.timedelta(hours=max_age_hours)]
    if not fresh:
        newest = ("; the newest successful one is %s, created %s" % (runs[0][0], runs[0][1].isoformat())
                  if runs else "; there is no successful one")
        limit = " is newer than %d h" % max_age_hours if max_age_hours else ""
        return None, "%s, and no successful run on main%s%s" % (why, limit, newest)
    run, at = fresh[0]
    return run, "%s: reading the newest successful run on main instead, %s (created %s)" % (why, run, at.isoformat())


def main(argv=None, now=None, listing=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--workflow", required=True, help="the producing workflow's name")
    parser.add_argument("--trigger", default="", help="the run named or triggering; empty: none")
    parser.add_argument("--conclusion", default="", help="the triggering run's; empty: dispatched")
    parser.add_argument("--branch", default="main")
    parser.add_argument("--repo", default=os.environ.get("KINOWO_REPO", "pawelkrupinski/movies"))
    parser.add_argument("--max-age-hours", type=int, default=MAX_AGE_HOURS)
    parser.add_argument("--on-fallback", choices=("newest", "skip"), default="newest")
    args = parser.parse_args(argv)
    if args.trigger and not args.trigger.isdigit():
        print("::error::the upstream run is a run id, not %r" % args.trigger, file=sys.stderr)
        return 1
    now = now or datetime.datetime.now(datetime.timezone.utc)
    gh = os.environ.get("KINOWO_GH") or str(Path(__file__).resolve().parent.parent / "forge-gh")
    as_is = taken_as_is(args.trigger, args.conclusion, args.branch)
    needs_listing = not as_is and not (args.trigger and args.on_fallback == "skip")
    runs = (listing or (lambda: successful_runs(args.workflow, args.repo, gh)))() if needs_listing else []
    run, message = choose(args.trigger, args.conclusion, args.branch, runs, now, args.max_age_hours, args.workflow,
                          args.on_fallback)
    if run is None:
        print("::error::%s" % message, file=sys.stderr)
        return 1
    print(("%s" if as_is else "::notice::%s" if run == "" else "::warning::%s") % message, file=sys.stderr)
    print(run)
    return 0


if __name__ == "__main__":
    sys.exit(main())
