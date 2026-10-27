#!/usr/bin/env python3
"""Which "Record scrape fixtures" run the live drift check compares (.github/workflows/identity-live-drift.yml) -- and
whose findings the findings report reports (.github/workflows/identity-findings-report.yml).

WHY. The check ran only after a SUCCESSFUL recording, and skipped its one job otherwise -- and a run whose only job is
skipped shows green. Run index 1493 did exactly that after recording 1476 failed on Germany's convergence leg: no
comparison, no counts pushed, a green run. Now:

- a run dispatched by hand compares the recording it names (`run-id`), whatever its age: someone chose it;
- a recording on main that succeeded is compared;
- one that failed, or did not end in success, is passed over for the NEWEST successful recording on main, said as a
  warning -- and when none is newer than MAX_AGE_HOURS (48), the step fails: a stale moment checks nothing;
- no recording named (the findings report's daily backstop) takes the newest successful recording on main.

`--max-age-hours 0` lifts the age limit. The findings report passes it: an older recording's findings are still the
newest there are to triage, where the drift check's comparison is of prod's moment. Run index 3139 was the report red
with "run 3136 left no country's findings" after recording 3136 failed before any convergence row wrote them.

Prints the run id on stdout; every reason on stderr, as a GitHub annotation. Lists runs through scripts/forge-gh (gh on
GitHub, Forgejo's API on a Forgejo checkout), or $KINOWO_GH.

Run:  python3 scripts/ci/live_drift_recording.py [--trigger RUN] [--conclusion C --branch B] [--repo O/R] [--max-age-hours 48]
Test: python3 scripts/ci/live_drift_recording_test.py
"""
import argparse
import datetime
import json
import os
import subprocess
import sys
from pathlib import Path

WORKFLOW = "Record scrape fixtures"
MAX_AGE_HOURS = 48


def successful_recordings(repo, gh):
    """The successful recordings on main, newest first by creation time (a status-filtered listing is not in creation
    order: identity_newest_recording in scripts/identity-tools.sh)."""
    out = subprocess.run([gh, "run", "list", "-R", repo, "--workflow", WORKFLOW, "--status", "success", "--branch", "main",
                          "-L", "50", "--json", "databaseId,createdAt"], check=True, capture_output=True, text=True).stdout
    runs = [(r["databaseId"], parsed(r["createdAt"])) for r in json.loads(out or "[]")]
    return sorted(runs, key=lambda r: r[1], reverse=True)


def parsed(stamp):
    return datetime.datetime.fromisoformat(stamp.replace("Z", "+00:00"))


def choose(trigger, conclusion, branch, recordings, now, max_age_hours=MAX_AGE_HOURS):
    """(run id, message) for the run compared; (None, message) when none may be. `max_age_hours` 0 is no limit."""
    if trigger and not conclusion:
        return trigger, "comparing recording %s, as dispatched" % trigger
    if trigger and conclusion == "success" and branch == "main":
        return trigger, "comparing recording %s, which just succeeded" % trigger
    why = ("no recording named" if not trigger else
           "recording %s %s" % (trigger, "ran off main (%s)" % branch if branch != "main" else "ended %s" % conclusion))
    fresh = [(run, at) for run, at in recordings
             if not max_age_hours or now - at <= datetime.timedelta(hours=max_age_hours)]
    if not fresh:
        newest = ("; the newest successful one is %s, recorded %s" % (recordings[0][0], recordings[0][1].isoformat())
                  if recordings else "; there is no successful one")
        limit = " is newer than %d h" % max_age_hours if max_age_hours else ""
        return None, "%s, and no successful recording on main%s%s" % (why, limit, newest)
    run, at = fresh[0]
    return run, "%s: comparing the newest successful recording on main instead, %s (recorded %s)" % (why, run, at.isoformat())


def main(argv=None, now=None, listing=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--trigger", default="", help="the recording that triggered the run; empty: none was named")
    parser.add_argument("--conclusion", default="")
    parser.add_argument("--branch", default="main")
    parser.add_argument("--repo", default=os.environ.get("KINOWO_REPO", "pawelkrupinski/movies"))
    parser.add_argument("--max-age-hours", type=int, default=MAX_AGE_HOURS)
    args = parser.parse_args(argv)
    if args.trigger and not args.trigger.isdigit():
        print("::error::the recording is a run id, not %r" % args.trigger, file=sys.stderr)
        return 1
    now = now or datetime.datetime.now(datetime.timezone.utc)
    gh = os.environ.get("KINOWO_GH") or str(Path(__file__).resolve().parent.parent / "forge-gh")
    needs_listing = not args.trigger or (args.conclusion and not (args.conclusion == "success" and args.branch == "main"))
    recordings = (listing or (lambda: successful_recordings(args.repo, gh)))() if needs_listing else []
    run, message = choose(args.trigger, args.conclusion, args.branch, recordings, now, args.max_age_hours)
    if run is None:
        print("::error::%s" % message, file=sys.stderr)
        return 1
    print(("::warning::%s" if needs_listing else "%s") % message, file=sys.stderr)
    print(run)
    return 0


if __name__ == "__main__":
    sys.exit(main())
