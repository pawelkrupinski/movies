#!/usr/bin/env python3
"""Waiting on GitHub Actions runs, bounded: the logic behind scripts/ci-wait (its header has the usage); tests:
scripts/ci_wait_test.py.

A TARGET is one thing waited for -- a run (`--run`), a workflow's run for a commit (`--workflow --sha`), or a
workflow's newest verdict (`--workflow --any-completed`) -- and every poll reads each unresolved target once. A read
is trusted only when it is whole: a run without an id, a sha or a status, a `completed` without a conclusion, a
listing that is not a list -- what `jq` printed as "null null null" the day a hand-written loop took it for a verdict
-- is an UNREADABLE read, asked again, and never an answer. An empty listing is an answer ("no run yet"), never a
verdict. Every gh call is bounded (CI_WAIT_GH_TIMEOUT, 60 s); CI_WAIT_MAX_MISSES (10) unreadable polls in a row end
the wait (exit 2).

CANCELLED. main's runs cancel each other (`cancel-in-progress`): a cancelled run says nothing, and the run that
cancelled it tests a superset. So by default a cancelled run is FOLLOWED to the runs of the same workflow on the same
branch at commits that descend from its own (git, else GitHub's compare API), and the first of them to complete
uncancelled is the verdict, named as superseding the one waited for. A cancelled run nothing superseded is exit 3,
as is any cancelled run under --no-follow.
"""
from __future__ import annotations

import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import time

EXIT_SUCCESS, EXIT_FAILURE, EXIT_NO_VERDICT, EXIT_CANCELLED = 0, 1, 2, 3
RUN_FIELDS = "databaseId,headSha,headBranch,status,conclusion,workflowName,createdAt,updatedAt"
LIST_LIMIT = 50


class Clock:
    """Monotonic seconds for deadlines, the wall time for the log lines, and the sleep between polls."""

    def now(self) -> float:
        return time.monotonic()

    def wall(self) -> str:
        return datetime.datetime.now(datetime.timezone.utc).strftime("%H:%M:%SZ")

    def sleep(self, seconds: float) -> None:
        time.sleep(seconds)


def gh_runner(gh_timeout: float):
    """(argv) -> (exit status, stdout): gh, ended once it outlasts gh_timeout (124, as `bounded` says it)."""

    def run(argv):
        try:
            done = subprocess.run(["gh"] + argv, capture_output=True, text=True, timeout=gh_timeout,
                                  stdin=subprocess.DEVNULL)
        except subprocess.TimeoutExpired:
            return 124, ""
        except OSError as e:
            return 127, str(e)
        if done.returncode != 0 and done.stderr.strip():
            say(f"gh {' '.join(argv[:2])}: {done.stderr.strip().splitlines()[-1]}")
        return done.returncode, done.stdout

    return run


def say(message: str) -> None:
    print(f"ci-wait: {message}", file=sys.stderr, flush=True)


class Run:
    def __init__(self, raw: dict):
        self.id = raw["databaseId"]
        self.sha = raw["headSha"]
        self.branch = raw.get("headBranch") or ""
        self.status = raw["status"]
        self.conclusion = raw.get("conclusion") or ""
        self.workflow = raw.get("workflowName") or ""
        self.created = raw.get("createdAt") or ""
        self.updated = raw.get("updatedAt") or ""

    @property
    def completed(self) -> bool:
        return self.status == "completed"

    @property
    def cancelled(self) -> bool:
        return self.completed and self.conclusion == "cancelled"

    def state(self) -> str:
        return f"{self.status}/{self.conclusion}" if self.completed else self.status


def whole_run(raw) -> Run | None:
    """The run, or None when the read is not whole enough to act on."""
    if not isinstance(raw, dict):
        return None
    if not isinstance(raw.get("databaseId"), int) or not raw.get("headSha") or not raw.get("status"):
        return None
    if raw["status"] == "completed" and not raw.get("conclusion"):
        return None  # GitHub's run objects lag their jobs by a moment: completed, no conclusion yet
    return Run(raw)


class GitHub:
    """The reads, each None when unreadable. `ancestry(a, b)`: is b a or a descendant of it (None: could not tell)."""

    def __init__(self, gh, repo: str | None = None, git=None):
        self.gh = gh
        self.repo = ["--repo", repo] if repo else []
        self.git = git or default_git
        self.known = {}

    def _json(self, argv):
        status, out = self.gh(argv)
        if status != 0:
            return None
        try:
            return json.loads(out) if out.strip() else None
        except ValueError:
            return None

    def view(self, run_id: int) -> Run | None:
        return whole_run(self._json(["run", "view", str(run_id), "--json", RUN_FIELDS] + self.repo))

    def runs(self, workflow: str, branch: str, commit: str | None = None) -> list | None:
        argv = ["run", "list", "--workflow", workflow, "--branch", branch, "--limit", str(LIST_LIMIT),
                "--json", RUN_FIELDS] + self.repo
        if commit:
            argv += ["--commit", commit]
        raw = self._json(argv)
        if not isinstance(raw, list):
            return None
        runs = [whole_run(r) for r in raw]
        # one run unreadable makes the listing unreadable: dropping it could hide the very run waited for
        return None if any(r is None for r in runs) else runs

    def ancestry(self, base: str, head: str) -> bool | None:
        if base == head:
            return True
        key = (base, head)
        if key not in self.known:
            answer = self.git(base, head)
            if answer is None:
                answer = self._compare(base, head)
            if answer is None:
                return None
            self.known[key] = answer
        return self.known[key]

    def _compare(self, base: str, head: str) -> bool | None:
        repo = self.repo[1] if self.repo else "{owner}/{repo}"
        status, out = self.gh(["api", f"repos/{repo}/compare/{base}...{head}", "--jq", ".status"])
        out = out.strip()
        if status != 0 or out not in ("ahead", "identical", "behind", "diverged"):
            return None
        return out in ("ahead", "identical")


def default_git(base: str, head: str) -> bool | None:
    try:
        done = subprocess.run(["git", "merge-base", "--is-ancestor", base, head], capture_output=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        return None
    return {0: True, 1: False}.get(done.returncode)  # 128: a commit this checkout does not have


class Target:
    """One thing waited for. `verdict` is set once it is answered: (run, note) -- the run whose conclusion answers it."""

    def __init__(self, workflow: str = "", branch: str = "main", sha: str = "", run_id: int | None = None,
                 any_completed: bool = False, follow: bool = True, or_later: bool = False, until_green: bool = False):
        self.workflow, self.branch, self.sha, self.run_id = workflow, branch, sha, run_id
        self.any_completed, self.follow = any_completed, follow
        self.or_later = or_later or until_green
        self.until_green = until_green
        self.own: Run | None = None
        self.verdict: tuple | None = None
        self.cancelled_alone = False
        self.state = ""

    def label(self) -> str:
        if self.run_id and not self.workflow:
            return f"run {self.run_id}"
        return self.workflow + (f" @ {self.sha[:9]}" if self.sha else f" on {self.branch}")


def poll(target: Target, github: GitHub) -> str | None:
    """Reads the target once; sets its verdict when answered. Returns its state line, or None when unreadable."""
    if target.any_completed:
        runs = github.runs(target.workflow, target.branch)
        if runs is None:
            return None
        done = sorted((r for r in runs if r.completed and not r.cancelled), key=lambda r: r.created, reverse=True)
        if done:
            target.verdict = (done[0], "")
            return f"newest completed run {done[0].id} at {done[0].sha[:9]}: {done[0].conclusion}"
        return "no completed run yet"

    if target.run_id is not None:
        own = github.view(target.run_id)
        if own is None:
            return None
        # a run's workflow, branch and commit are its own: followed, it is to its workflow's later runs on its branch
        target.workflow, target.branch, target.sha, target.own = own.workflow, own.branch, own.sha, own
        runs = None
    else:
        runs = github.runs(target.workflow, target.branch)
        if runs is None:
            return None
        at_sha = [r for r in runs if r.sha == target.sha]
        if not at_sha and len(runs) >= LIST_LIMIT:
            at_sha = github.runs(target.workflow, target.branch, commit=target.sha)
            if at_sha is None:
                return None
        target.own = max(at_sha, key=lambda r: r.created) if at_sha else None
    own = target.own

    if own is not None and own.completed and not own.cancelled and not (target.until_green and own.conclusion != "success"):
        target.verdict = (own, "")
        return f"run {own.id} at {own.sha[:9]}: {own.state()}"
    if own is not None and not own.completed and not target.or_later:
        return f"run {own.id} at {own.sha[:9]}: {own.state()}"
    if own is None and not target.or_later:
        return f"no run at {target.sha[:9]} yet"
    if own is not None and own.cancelled and not target.follow:
        target.verdict = (own, "")
        return f"run {own.id} at {own.sha[:9]}: cancelled"

    # Followed: the runs at the commit's descendants -- every time with --or-later, else because the own run was cancelled
    if runs is None:
        runs = github.runs(target.workflow, target.branch)
        if runs is None:
            return None
    later = []
    for r in runs:
        if r.sha == target.sha:
            continue
        descends = github.ancestry(target.sha, r.sha)
        if descends is None:
            return None
        if descends:
            later.append(r)
    candidates = ([own] if own is not None else []) + later
    if target.until_green:
        answered = [r for r in candidates if r.completed and r.conclusion == "success"]
    else:
        answered = [r for r in candidates if r.completed and not r.cancelled]
    if answered:
        first = min(answered, key=lambda r: (r.updated, r.created))
        note = ""
        if own is not None and first.id != own.id:
            note = f"superseded run {own.id} at {own.sha[:9]} ({own.state()})"
        elif own is None:
            note = f"no run at {target.sha[:9]}; first at or after it"
        target.verdict = (first, note)
        return f"run {first.id} at {first.sha[:9]}: {first.state()}" + (f" -- {note}" if note else "")
    running = [r for r in candidates if not r.completed]
    if running:
        newest = max(running, key=lambda r: r.created)
        head = f"run {own.id} at {own.sha[:9]} {own.state()}; " if own is not None else f"no run at {target.sha[:9]}; "
        return head + f"waiting on run {newest.id} at {newest.sha[:9]} ({newest.state()})"
    if target.or_later:
        return f"no {'green ' if target.until_green else ''}run at or after {target.sha[:9]} yet"
    # cancelled, and nothing running or done after it: cancelled by hand, or every successor cancelled too
    target.verdict = (own, "cancelled; nothing superseded it" if not later else "cancelled; every later run cancelled too")
    target.cancelled_alone = True
    return f"run {own.id} at {own.sha[:9]}: cancelled, nothing superseded it"


def exit_status(targets: list) -> int:
    conclusions = [t.verdict[0].conclusion if t.verdict else None for t in targets]
    if any(c not in (None, "success", "cancelled") for c in conclusions):
        return EXIT_FAILURE
    if any(c == "cancelled" for c in conclusions):
        return EXIT_CANCELLED
    if any(c is None for c in conclusions):
        return EXIT_NO_VERDICT
    return EXIT_SUCCESS


def parent_alive():
    """Whether the process that started this one still runs: a wait left behind by a killed caller ends itself."""
    parent = os.getppid()
    return lambda: os.getppid() == parent


def wait(targets: list, github: GitHub, clock: Clock, timeout_s: float, interval_s: float, max_misses: int,
         caller_alive=lambda: True) -> int:
    deadline = clock.now() + timeout_s
    misses = 0
    ended = "pending"
    while True:
        unreadable = False
        for t in targets:
            if t.verdict:
                continue
            state = poll(t, github)
            if state is None:
                unreadable = True
                continue
            if state != t.state:
                t.state = state
                print(f"{clock.wall()} {t.label()}: {state}", file=sys.stderr, flush=True)
        misses = misses + 1 if unreadable else 0
        failed = any(t.verdict and t.verdict[0].conclusion not in ("success", "cancelled") for t in targets)
        if all(t.verdict for t in targets) or failed:
            break
        if misses >= max_misses:
            say(f"{misses} unreadable reads in a row: no verdict")
            ended = "unreadable"
            break
        if clock.now() >= deadline:
            say(f"timed out after {timeout_s / 60:g} min")
            ended = "timeout"
            break
        if not caller_alive():
            say("the caller is gone: stopping")
            ended = "orphaned"
            break
        clock.sleep(interval_s)
    for t in targets:
        if t.verdict:
            run, note = t.verdict
            print("\t".join([str(run.id), run.sha, run.conclusion, t.workflow or t.label(), note]).rstrip("\t"))
        else:
            own = t.own
            print("\t".join([str(own.id) if own else "-", own.sha if own else (t.sha or "-"),
                             ended, t.workflow or t.label(), t.state]).rstrip("\t"))
    return exit_status(targets)


def duration(text: str, unit: str) -> float:
    """'90s', '20m', '2h', or a bare number in <unit> ('s' or 'm') -> seconds."""
    m = re.fullmatch(r"(\d+(?:\.\d+)?)([smh]?)", text.strip())
    if not m:
        raise argparse.ArgumentTypeError(f"not a duration: {text}")
    return float(m.group(1)) * {"s": 1, "m": 60, "h": 3600}[m.group(2) or unit]


def resolve_sha(ref: str) -> str:
    if re.fullmatch(r"[0-9a-f]{40}", ref):
        return ref
    try:
        done = subprocess.run(["git", "rev-parse", "--verify", "--quiet", ref + "^{commit}"], capture_output=True,
                              text=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        done = None
    if done is None or done.returncode != 0:
        raise SystemExit(f"ci-wait: --sha {ref}: not a commit this checkout knows (give the full 40-character sha)")
    return done.stdout.strip()


def parse(argv: list) -> argparse.Namespace:
    p = argparse.ArgumentParser(prog="ci-wait", add_help=False)
    p.add_argument("--run", type=int, action="append", default=[])
    p.add_argument("--workflow", action="append", default=[])
    p.add_argument("--sha")
    p.add_argument("--branch", default="main")
    p.add_argument("--any-completed", action="store_true")
    p.add_argument("--or-later", action="store_true")
    p.add_argument("--until-green", action="store_true")
    p.add_argument("--no-follow", action="store_true")
    p.add_argument("--timeout", type=lambda s: duration(s, "m"), default=60 * 60)
    p.add_argument("--interval", type=lambda s: duration(s, "s"),
                   default=float(os.environ.get("CI_WAIT_POLL_SECONDS", "30")))
    p.add_argument("--repo")
    args = p.parse_args(argv)
    if not args.run and not args.workflow:
        p.error("name a --run or a --workflow")
    if args.workflow and not args.sha and not args.any_completed:
        p.error("--workflow needs --sha <commit> or --any-completed")
    if args.sha and args.any_completed:
        p.error("--sha and --any-completed are two different questions: pick one")
    if args.no_follow and (args.or_later or args.until_green):
        p.error("--no-follow waits on one run only: it does not combine with --or-later / --until-green")
    return args


def targets_of(args) -> list:
    common = dict(follow=not args.no_follow, or_later=args.or_later, until_green=args.until_green)
    targets = [Target(run_id=r, **common) for r in args.run]
    sha = resolve_sha(args.sha) if args.sha else ""
    for w in args.workflow:
        targets.append(Target(workflow=w, branch=args.branch, sha=sha, any_completed=args.any_completed, **common))
    return targets


def main(argv=None, gh=None, git=None, clock=None) -> int:
    try:
        args = parse(sys.argv[1:] if argv is None else argv)
    except SystemExit as e:
        return 2 if e.code else 0
    github = GitHub(gh or gh_runner(float(os.environ.get("CI_WAIT_GH_TIMEOUT", "60"))), args.repo, git)
    return wait(targets_of(args), github, clock or Clock(), args.timeout, args.interval,
                int(os.environ.get("CI_WAIT_MAX_MISSES", "10")), parent_alive())


if __name__ == "__main__":
    sys.exit(main())
