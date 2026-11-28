"""One slot, settled: did GitHub run it, must it be dispatched, and the late duplicate cancelled.

A part of nix/files/scheduled_dispatch.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import argparse
import datetime
import json
import os
from typing import Callable, List, Optional, Set
import dispatch_clock
from dispatch_cron import latest_slot, next_slot
from dispatch_github import ApiError, GitHub, Json, RATE_LIMIT_MARGIN_SECONDS, RATE_LIMIT_MIN_WAIT_SECONDS, WorkflowRun
from dispatch_workflows import EVENT_TYPE_PREFIX, stem


SCHEDULED_EVENTS = ("schedule", "repository_dispatch")

Clock = Callable[[], datetime.datetime]
Sleep = Callable[[float], None]
Log = Callable[[str], None]


# ---------------------------------------------------------------- the decision

def iso(at: datetime.datetime) -> str:
    return at.strftime("%Y-%m-%dT%H:%M:%SZ")


def created_at(run: WorkflowRun) -> datetime.datetime:
    return datetime.datetime.strptime(run["created_at"], "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc)


class Slot:
    """One slot of one workflow, settled against GitHub: run it if GitHub did not, then cancel GitHub's late copy."""

    def __init__(self, github: GitHub, workflow_file: str, slot: datetime.datetime, log: Log, clock: Clock,
                 sleep: Sleep, attempts: int = 4, backoff: int = 60, poll: int = 120) -> None:
        self.github, self.file, self.slot, self.log = github, workflow_file, slot, log
        self.clock, self.sleep, self.attempts, self.backoff, self.poll = clock, sleep, attempts, backoff, poll
        self.head = f"scheduled-dispatch: workflow={workflow_file} slot={iso(slot)}"

    def _runs(self) -> List[WorkflowRun]:
        return [r for r in self.github.runs_since(self.file, self.slot) if r.get("event") in SCHEDULED_EVENTS]

    def _late_minutes(self, run: WorkflowRun) -> int:
        return int((created_at(run) - self.slot).total_seconds() // 60)

    def _settle_once(self) -> str:
        """One look at the slot's runs: 'dispatched-earlier', 'ran', or the dispatch sent now ('missed-dispatched')."""
        runs = sorted(self._runs(), key=lambda r: r.get("created_at") or "")
        ours = [r for r in runs if r["event"] == "repository_dispatch"]
        if ours:
            self.log(f"{self.head} outcome=dispatched-earlier run={ours[0].get('id')}")
            return "dispatched-earlier"
        if runs:
            self.log(f"{self.head} outcome=ran run={runs[0].get('id')} event=schedule "
                     f"late_minutes={self._late_minutes(runs[0])}")
            return "ran"
        self.github.dispatch(EVENT_TYPE_PREFIX + stem(self.file),
                             {"slot": iso(self.slot), "source": "scheduled-dispatch"})
        self.log(f"{self.head} outcome=missed-dispatched")
        return "missed-dispatched"

    def _wait_out_rate_limit(self, retry_at: datetime.datetime, error: str, deadline: datetime.datetime) -> bool:
        """Sleep until GitHub's reset, or say (False) that it comes past the guard's end."""
        wait = max((retry_at - self.clock()).total_seconds() + RATE_LIMIT_MARGIN_SECONDS, RATE_LIMIT_MIN_WAIT_SECONDS)
        if self.clock() + datetime.timedelta(seconds=wait) >= deadline:
            self.log(f"{self.head} rate-limited until={iso(retry_at)} past the guard's end "
                     f"{iso(deadline)} api-error={error}")
            return False
        self.log(f"{self.head} rate-limited until={iso(retry_at)} waiting {int(wait)} s api-error={error}")
        self.sleep(wait)
        return True

    def _back_off(self, attempt: int, error: str) -> None:
        self.log(f"{self.head} attempt={attempt}/{self.attempts} api-error={error}")
        if attempt < self.attempts:
            self.sleep(self.backoff * attempt)

    def ensure(self, deadline: datetime.datetime) -> str:
        """'ran' (GitHub's schedule started it), 'dispatched-earlier' (an earlier dispatch of ours did),
        'missed-dispatched' (neither: dispatched now) or 'dispatch-failed' (the API would not say or take it).
        A rate-limited refusal costs no attempt: it waits for the reset GitHub names, while that is before
        `deadline` (the guard's end: a dispatch later than that would collide with GitHub's late run or the next
        slot). 2026-11-09: four fixed attempts spent in 6 minutes of an exhausted quota lost the nightly recording."""
        error = ""
        attempt = 0
        while attempt < self.attempts:
            try:
                return self._settle_once()
            except ApiError as e:
                error = str(e).replace("\n", " ")
                if e.retry_at is None:
                    attempt += 1
                    self._back_off(attempt, error)
                elif not self._wait_out_rate_limit(e.retry_at, error, deadline):
                    break
        self.log(f"{self.head} outcome=dispatch-failed error={error}")
        return "dispatch-failed"

    def _cancel_late(self, cancelled: Set[int]) -> bool:
        """One poll of the guard: cancel the slot's not-yet-completed schedule runs while a live run of ours exists.
        True once the schedule run has been judged (ours is listed beside it)."""
        runs = self._runs()
        if any(r["event"] == "repository_dispatch" and r.get("conclusion") != "cancelled" for r in runs):
            for r in runs:
                if r["event"] == "schedule" and r.get("status") != "completed" and r["id"] not in cancelled:
                    self.github.cancel(r["id"])
                    cancelled.add(r["id"])
                    self.log(f"{self.head} outcome=late-duplicate-cancelled run={r['id']} "
                             f"late_minutes={self._late_minutes(r)}")
        return {"schedule", "repository_dispatch"} <= {r["event"] for r in runs}

    def guard(self, until: datetime.datetime) -> List[int]:
        """Until `until`, cancel every schedule run of the slot that GitHub starts after ours (it starts them hours
        late: 3-10 h in 2026-11), so the slot runs once. Only while a run of ours exists that was not cancelled: never
        the slot's only run. Ours waiting in its concurrency group is cancelled by GitHub's late run of the same group
        arriving (a pending run is superseded; prune-actions-caches cancels in progress too), and GitHub's is then the
        slot's run. GitHub starts a slot's schedule run at most once, so the guard ends once it has judged it against
        ours (cancelled, or kept because ours was superseded): every poll is a request of the App installation's hourly quota. A
        rate-limited poll waits for the reset rather than polling into it."""
        cancelled: Set[int] = set()
        while self.clock() < until:
            wait: float = self.poll
            try:
                if self._cancel_late(cancelled):  # ours listed too: the schedule run was judged
                    break
            except ApiError as e:
                self.log(f"{self.head} guard api-error={str(e).replace(chr(10), ' ')}")
                if e.retry_at is not None:
                    wait = max(wait, (e.retry_at - self.clock()).total_seconds() + RATE_LIMIT_MARGIN_SECONDS)
            self.sleep(wait)
        return sorted(cancelled)


def settle(github: GitHub, workflow_file: str, crons: List[str], now: datetime.datetime, grace_minutes: int,
           guard_hours: int, log: Log, clock: Clock, sleep: Sleep) -> int:
    """Wait out the grace after the latest slot, make sure it ran, and guard it. The exit status."""
    slot = latest_slot(crons, now)
    wait = (slot + datetime.timedelta(minutes=grace_minutes) - now).total_seconds()
    if wait > 0:
        log(f"scheduled-dispatch: workflow={workflow_file} slot={iso(slot)} waiting {int(wait)} s of grace")
        sleep(wait)
    guard_end = min(slot + datetime.timedelta(hours=guard_hours), next_slot(crons, slot))
    one = Slot(github, workflow_file, slot, log, clock, sleep)
    outcome = one.ensure(guard_end)
    if outcome in ("missed-dispatched", "dispatched-earlier"):
        one.guard(guard_end)
    return 1 if outcome == "dispatch-failed" else 0


def _schedule_entry(schedule_file: str, workflow: str) -> Json:
    with open(schedule_file) as f:
        return json.load(f)["workflows"][workflow]


def _key_file(given: Optional[str]) -> str:
    """The App's private key: the one named, or the systemd credential `github-app-key`."""
    return given or os.path.join(os.environ.get("CREDENTIALS_DIRECTORY", ""), "github-app-key")


def _print_line(line: str) -> None:
    print(line, flush=True)


def run(args: argparse.Namespace, clock: Optional[Clock] = None, sleep: Sleep = dispatch_clock.sleep,
        log: Optional[Log] = None) -> int:
    entry = _schedule_entry(args.schedule, args.workflow)
    clock = clock or dispatch_clock.now_utc
    github = GitHub(args.repository, args.app_id, _key_file(args.key_file))
    return settle(github, entry["file"], entry["crons"], clock(), args.grace_minutes, args.guard_hours,
                  log or _print_line, clock, sleep)
