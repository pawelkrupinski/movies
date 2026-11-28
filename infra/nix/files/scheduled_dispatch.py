#!/usr/bin/env python3
"""The external scheduler for the movies repo's daily and weekly GitHub Actions workflows.

WHY. GitHub's `schedule:` events are best effort, and on this repository they are worse than late:
identity-family-explorer.yml (cron '7 2 * * 0') was not run at all on Sunday 2026-11-08, with the
workflow active, and every scheduled run of 2026-10-31..11-08 started 3 to 10 hours after its slot
(nightly-stress 01:30 -> ~07:40, convergence-daily 03:23 -> ~10:30). The owner, that day: "write an
external cron trigger for the overnight jobs so we don't depend on github".

ONE SOURCE OF TRUTH: the workflows' own `on.schedule` crons. `generate` reads every
.github/workflows/*.yml and writes infra/nix/files/scheduled-workflows.json, which
modules/roles/scheduled-dispatch.nix turns into one systemd timer per workflow (OnCalendar in UTC).
`check` fails when that file, or a workflow's wiring, has drifted from the crons
(scheduled_dispatch_test.py runs it over the repository, so a cron edited without regenerating is
red in the script tests).

COVERED: a workflow whose every cron fires at most once a day (one minute, one hour). A sub-daily
one (decodo-probe every 30 min, security-headers every 6 h) is left to GitHub: its next slot is the
recovery from a dropped one, and a grace period as long as its interval would double every run.

THE DISPATCH. At each slot the timer starts `run <stem>`, which waits out a short grace period, then
asks GitHub for the workflow's runs created since the slot. A run started by the `schedule` event
(GitHub on time) or by an earlier dispatch of ours means the slot ran. Otherwise it sends the
repository_dispatch event `scheduled-<stem>`, which each covered workflow listens to and treats
exactly as its schedule (its scheduled-run conditions read `github.event_name == 'schedule' ||
github.event_name == 'repository_dispatch'`). Not a workflow_dispatch: a dispatched run gets its
inputs' DEFAULTS, a scheduled one gets them EMPTY, and every workflow here was written for the
second; a repository_dispatch run has no inputs either, so it takes the scheduled path unchanged.

THE GUARD. GitHub's scheduler stays on, as the backstop for this host being down -- so when it then
starts the slot's run hours late, that run is a duplicate: for --guard-hours after the slot (never
past the next one) the service polls every two minutes and cancels a not-yet-completed `schedule`
run of the slot, only while a run of ours exists that was not cancelled (GitHub's late run, on
arriving in the workflow's concurrency group, cancels ours if ours is still pending there). A cancelled run says nothing to the red gate and
skips report-scheduled-run (`!cancelled()`). The cost: a late run may have done a minute or two of
its plan job before it is cancelled. The guard ends as soon as it has judged the slot's schedule run
(GitHub starts one at most): each poll is a request of the App installation's 5,000 an hour.

Bounded retries (an API error is retried, re-checking the runs first, since a dispatch whose
answer was lost may still have landed). A RATE-LIMITED refusal (403/429 with x-ratelimit-remaining 0,
retry-after, or a rate-limit message) costs no attempt: it waits for the reset GitHub names, up to the
guard's end (2026-11-09: the four attempts spent in six minutes of an exhausted quota lost the nightly
recording), logged as `rate-limited until=<iso> waiting <n> s`. Every decision is one journal line, shipped to
VictoriaLogs:
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=ran run=<id> event=schedule late_minutes=<n>
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=dispatched-earlier run=<id>
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=missed-dispatched
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=late-duplicate-cancelled run=<id> late_minutes=<n>
    scheduled-dispatch: workflow=<file> slot=<iso> outcome=dispatch-failed error=<...>
`dispatch-failed` exits 1, so the unit is failed and host-health.rules' SystemdUnitFailed pages.

CREDENTIAL: the kinowo-lander GitHub App (installed on movies; Contents RW, which a
repository_dispatch needs), the identity agentctl already pushes and dispatches as on ci-runner
(bitcashier infra/nix/hosts/ci-runner/github-apps.nix). Its installation token is minted the way
agentctl's lib/agentctl/ghapp.py does: a JWT signed RS256 by `openssl dgst`, no Python crypto.
"""
import argparse
import os
import sys
from typing import Optional, Sequence

# The pieces beside it. Run as a script, this file's own directory leads sys.path and `scheduled_dispatch_parts/` is found
# from it; roles/scheduled-dispatch.nix swaps this one line for the store copy of that directory, because the built
# script lives in the store and its siblings do not.
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), "scheduled_dispatch_parts"))
from dispatch_workflows import JSON_PATH, generate, problems, read_workflows, render  # noqa: E402
from dispatch_slot import run  # noqa: E402
# What scheduled_dispatch_test.py reads of this file (`sd.<name>`): the pieces' own names, re-exported.
from dispatch_cron import latest_slot as latest_slot, matches as matches, next_slot as next_slot  # noqa: E402,F401
from dispatch_cron import on_calendar as on_calendar, parse_cron as parse_cron, staggering as staggering  # noqa: E402,F401
from dispatch_github import ApiError as ApiError, rate_limit_reset as rate_limit_reset  # noqa: E402,F401
from dispatch_slot import created_at, iso as iso, settle as settle  # noqa: E402,F401
from dispatch_workflows import DISPATCH_EQ as DISPATCH_EQ, dispatch_types as dispatch_types  # noqa: E402,F401
from dispatch_workflows import workflow_crons as workflow_crons  # noqa: E402,F401

_created = created_at


DOCSTRING = __doc__ or ""  # None under python -OO


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)
    gen = sub.add_parser("generate", help=f"write {JSON_PATH} from the workflows' crons")
    gen.add_argument("--root", default=".")
    chk = sub.add_parser("check", help="fail when the generated file or a workflow's wiring has drifted")
    chk.add_argument("--root", default=".")
    rn = sub.add_parser("run", help="settle a workflow's latest slot: wait the grace, dispatch if GitHub did not run it")
    rn.add_argument("workflow", help="the workflow's file stem, a key of the schedule file")
    rn.add_argument("--schedule", required=True, help="scheduled-workflows.json")
    rn.add_argument("--repository", required=True)
    rn.add_argument("--app-id", required=True)
    rn.add_argument("--key-file", help="the App's private key (default: the systemd credential github-app-key)")
    rn.add_argument("--grace-minutes", type=int, default=15)
    rn.add_argument("--guard-hours", type=int, default=14,
                    help="how long after the slot GitHub's late schedule run is cancelled (never past the next slot)")
    args = parser.parse_args(argv)
    if args.command == "generate":
        with open(os.path.join(args.root, JSON_PATH), "w") as f:
            f.write(render(generate(read_workflows(args.root))))
        return 0
    if args.command == "check":
        found = problems(args.root)
        for p in found:
            print(f"scheduled-dispatch check: {p}", file=sys.stderr)
        return 1 if found else 0
    return run(args)


if __name__ == "__main__":
    sys.exit(main())
