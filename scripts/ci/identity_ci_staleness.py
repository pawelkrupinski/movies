#!/usr/bin/env python3
"""IS THE IDENTITY CI STILL TELLING US ANYTHING? Names each check that has gone stale, as OVERDUE, not as a red run
(.github/workflows/identity-ci-staleness.yml runs it as scripts/ci/identity-ci-staleness.sh).

WHY. The identity resolver's code-caused regressions are caught by CI, never by Prometheus: the identity takes check and
the suspect-take ratchet in every leg of `Identity model convergence`, replaying the pairs the nightly `Record scrape
fixtures` pinned. Both can go quiet without anyone being told: convergence is DISPATCHED by Main, so its `report` job
(`schedule` only) never fires and a red leg sat red for ~10h of pushes with no issue; a pipeline commit whose dispatch
was refused, or whose run was cancelled and never replaced, simply has no verdict; and the nightly recording's own
report opens an issue for a FAILED scheduled run, not for one cancelled, timed out or never started.

The checks, each against `gh run list` and, for coverage, this checkout's history:

  convergence-red   the newest decided (success|failure) convergence run on main failed, and the red streak began (its
                    first failure completed) over CONVERGENCE_RED_HOURS (6) ago;
  convergence-unrun the newest first-parent commit touching `.github/convergence-paths.txt` (CONVERGENCE_PATHS_FILE)
                    landed over CONVERGENCE_UNRUN_HOURS (3) ago and no convergence run that is queued, running or
                    decided (not cancelled) is at it or a descendant of it;
  takes-red, takes-unrun   the same two of `Identity takes`;
  recording-stale   no `Record scrape fixtures` run on main has succeeded in RECORDING_STALE_HOURS (36).

A stale check is work OVERDUE, not a regression: it prints `::warning::`, writes `overdue=true` to $GITHUB_OUTPUT and
exits 0 (the workflow's `report` job files the issue). Exit 1 only when the checker cannot judge (gh, git or the run
list failed): a broken check is. Needs GH_TOKEN and GH_REPO and a checkout of main with its history; NOW (epoch
seconds) is the clock. Tested by identity_ci_staleness_test.py."""
from __future__ import annotations

import datetime
import json
import os
import re
import sys
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Mapping, Optional, Sequence, TextIO, Tuple, cast

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import ci_machine  # noqa: E402
import tool_actions  # noqa: E402
import tool_git  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

RUN_FIELDS = "headSha,status,conclusion,createdAt,updatedAt,url"
HOUR = 3600


@dataclass(frozen=True)
class Run:
    sha: str
    status: str
    conclusion: str
    created: str
    updated: Any
    url: str

    @property
    def decided(self) -> bool:
        return self.status == "completed" and self.conclusion in ("success", "failure")

    @property
    def finished(self) -> float:
        """When it completed, in epoch seconds; ValueError for a time `fromdateiso8601` would refuse."""
        return epoch(self.updated)


def epoch(stamp: object) -> int:
    if not isinstance(stamp, str):
        raise ValueError(f"not a timestamp: {stamp!r}")
    when = datetime.datetime.strptime(stamp, "%Y-%m-%dT%H:%M:%SZ")
    return int(when.replace(tzinfo=datetime.timezone.utc).timestamp())


def parse_runs(text: str) -> List[Run]:
    """The runs of a `gh run list` answer; ValueError for one that is no list of runs."""
    parsed: Any = json.loads(text)
    if not isinstance(parsed, list):
        raise ValueError("not a list")
    entries = cast("List[Dict[str, Any]]", parsed)
    return [Run(str(e.get("headSha")), str(e.get("status")), str(e.get("conclusion")), str(e.get("createdAt")),
                e.get("updatedAt"), str(e.get("url"))) for e in entries]


class Verdicts:
    """What was said, and so whether the CI is overdue (`stale`) and whether the checker itself failed (`broken`)."""

    def __init__(self, actions: tool_actions.Actions) -> None:
        self.actions = actions
        self.stale = False
        self.broken = False

    def judge(self, check: str, state: str, message: str) -> None:
        if state == "stale":
            self.actions.say(f"::warning::{check}: {message}")
            self.stale = True
        else:
            self.actions.say(f"ok  {check}: {message}")

    def unjudged(self, check: str, message: str) -> None:
        """The checker could not read its input: stale AND broken."""
        self.actions.say(f"::error::{check}: {message}")
        self.stale = self.broken = True


@dataclass(frozen=True)
class Limits:
    now: int
    red_hours: float
    unrun_hours: float
    recording_hours: float


Judge = Callable[[Sequence[Run], Limits], Tuple[str, str]]


def red_streak(runs: Sequence[Run], now: int) -> List[Run]:
    """The decided runs, newest first, up to the newest success."""
    settled = [run for run in sorted(runs, key=lambda r: r.created)[::-1] if run.finished <= now and run.decided]
    streak: List[Run] = []
    for run in settled:
        if run.conclusion != "failure":
            break
        streak.append(run)
    return streak


def red_verdict(runs: Sequence[Run], limits: Limits) -> Tuple[str, str]:
    """(stale|fresh, why) of a workflow's failing streak."""
    streak = red_streak(runs, limits.now)
    if not streak:
        return "fresh", "newest decided run is green (or there is none)"
    since = str(streak[-1].updated)
    age = (limits.now - epoch(since)) // HOUR
    state = "stale" if age >= limits.red_hours else "fresh"
    return state, f"red since {since} ({age}h, {len(streak)} failed run(s)); newest {streak[0].url}"


def recording_verdict(runs: Sequence[Run], limits: Limits) -> Tuple[str, str]:
    successes = [run for run in runs if run.status == "completed" and run.conclusion == "success"
                 and run.finished <= limits.now]
    if not successes:
        return "stale", "no successful run among the last 100"
    last = sorted(successes, key=lambda r: epoch(r.updated))[-1]
    age = (limits.now - epoch(last.updated)) // HOUR
    state = "stale" if age >= limits.recording_hours else "fresh"
    return state, f"last success {last.updated} ({age}h ago): {last.url}"


class Pipeline:
    """The newest first-parent commit touching the convergence paths (None when none does)."""

    def __init__(self, git: tool_git.Git, paths_file: str) -> None:
        self.git = git
        self.sha = ""
        self.time = 0
        self.read = True
        self._find(self._pathspecs(paths_file))

    @staticmethod
    def _pathspecs(paths_file: str) -> List[str]:
        """`dir/**` is git's `dir/`; any other entry is an exact path."""
        try:
            with open(paths_file) as f:
                lines = f.read().splitlines()
        except OSError:
            return []
        entries = (re.sub(r"\s*#.*", "", line).strip() for line in lines)
        return [entry.removesuffix("**") for entry in entries if entry and not entry.startswith("!")]

    def _find(self, pathspecs: Sequence[str]) -> None:
        newest = self.git.out("log", "-1", "--first-parent", "--format=%H %ct", "HEAD", "--", *pathspecs)
        if newest is None:
            self.read = False
        elif newest.split():
            self.sha, time = newest.split()
            self.time = int(time)

    def covered_by(self, runs: Sequence[Run]) -> str:
        """The sha of the first queued, running or decided run at the commit or a descendant of it; "" if none."""
        for run in runs:
            if run.conclusion not in ("cancelled", "skipped") \
                    and self.git.ok("merge-base", "--is-ancestor", self.sha, run.sha, quiet=True):
                return run.sha
        return ""


class Staleness:
    def __init__(self, gh: tool_github.GitHub, git: tool_git.Git, verdicts: Verdicts, limits: Limits,
                 pipeline: Pipeline) -> None:
        self.gh = gh
        self.git = git
        self.verdicts = verdicts
        self.limits = limits
        self.pipeline = pipeline

    def runs(self, workflow: str) -> Optional[str]:
        return self.gh.read(["run", "list", "--workflow", workflow, "--branch", "main", "--limit", "100",
                             "--json", RUN_FIELDS], timeout=60)

    def listed(self, check: str, workflow: str) -> Optional[str]:
        """A workflow's runs as the JSON gh printed; a failed read is no `nothing is wrong`: it is reported stale."""
        text = self.runs(workflow)
        if text is None:
            self.verdicts.unjudged(check, f"could not list {workflow} runs")
        return text

    def judged(self, check: str, workflow: str, text: Optional[str], judge: Judge) -> None:
        try:
            state, why = judge(parse_runs(text or "[]"), self.limits)
        except (ValueError, TypeError, AttributeError):
            self.verdicts.unjudged(check, f"could not judge the {workflow} runs (jq failed on them)")
            return
        self.verdicts.judge(check, state, why)

    def unrun(self, label: str, workflow: str, text: Optional[str]) -> None:
        check, pipeline = f"{label}-unrun", self.pipeline
        if not pipeline.read:
            self.verdicts.unjudged(check, "could not read this checkout's history")
            return
        if not pipeline.sha:
            self.verdicts.judge(check, "fresh", "no commit touches the convergence paths")
            return
        try:
            runs = parse_runs(text or "[]")
        except (ValueError, TypeError, AttributeError):
            runs = []
        covered, short = pipeline.covered_by(runs), pipeline.sha[:9]
        age = (self.limits.now - pipeline.time) // HOUR
        if covered:
            self.verdicts.judge(check, "fresh", f"pipeline commit {short} is covered by a {workflow} run at {covered[:9]}")
        elif age >= self.limits.unrun_hours:
            self.verdicts.judge(check, "stale", f"pipeline commit {short} landed {age}h ago and no {workflow} run covers it")
        else:
            self.verdicts.judge(check, "fresh", f"pipeline commit {short} is {age}h old, not yet due a run")

    def gating(self, label: str, workflow: str) -> None:
        text = self.listed(label, workflow)
        self.judged(f"{label}-red", workflow, text, red_verdict)
        self.unrun(label, workflow, text)


def main(env: Mapping[str, str], out: TextIO) -> int:
    runner = tool_process.SubprocessRunner()
    verdicts = Verdicts(tool_actions.Actions(env, out))
    limits = Limits(int(env.get("NOW") or datetime.datetime.now(datetime.timezone.utc).timestamp()),
                    float(env.get("CONVERGENCE_RED_HOURS", "6")), float(env.get("CONVERGENCE_UNRUN_HOURS", "3")),
                    float(env.get("RECORDING_STALE_HOURS", "36")))
    git = tool_git.Git(runner, env, os.getcwd())
    paths = env.get("CONVERGENCE_PATHS_FILE") or os.path.join(os.path.dirname(HERE), "..", ".github", "convergence-paths.txt")
    check = Staleness(tool_github.GitHub(runner, env), git, verdicts, limits, Pipeline(git, paths))
    recording = env.get("RECORDING_WORKFLOW", "record-scrape-fixtures.yml")
    recording_text = check.listed("recording-stale", recording)
    check.gating("convergence", env.get("CONVERGENCE_WORKFLOW", "identity-model-convergence.yml"))
    check.gating("takes", env.get("TAKES_WORKFLOW", "identity-takes.yml"))
    check.judged("recording-stale", recording, recording_text, recording_verdict)
    tool_actions.Actions(env, out).output("overdue", "true" if verdicts.stale else "false")
    return 1 if verdicts.broken else 0


if __name__ == "__main__":
    sys.exit(main(ci_machine.environment(), sys.stdout))
