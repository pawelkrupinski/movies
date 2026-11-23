"""DISPATCHING A WORKFLOW and finding the run it made, the one way the tools do it (scripts/lib; it took over the
shell's scripts/ci/dispatch.sh and its rules): every gh call bounded, and a failed READ never taken for "nothing there"
-- each answer is None (said on stderr, `dispatch: ...`) when gh could not be read.

  Dispatcher(gh, settings, clock, sleep)
    .dispatch(workflow, ref, args)        `gh workflow run`; a 5xx (GitHub's own blip) asked again twice after
                                          DISPATCH_RETRY_SECONDS (15); a refusal for the token's spent API rate limit
                                          waited out to its reset when that is at most DISPATCH_RATE_LIMIT_MAX_WAIT
                                          (900 s) away (DISPATCH_RATE_LIMIT_SECONDARY_SECONDS, 60, for a secondary
                                          limit); any other refusal fails at once
    .find_run(workflow, since, predicate, args) the newest workflow_dispatch run created at or after `since` (ISO-8601)
                                          matching the jq `predicate` (fields databaseId, headSha, createdAt,
                                          displayTitle), polled for DISPATCH_FIND_SECONDS (120): a dispatch's run is
                                          listed seconds after it
    .find_titled(workflow, title, seconds, poll) the newest workflow_dispatch run titled so (a run another run's job
                                          dispatched: an Identity takes run its convergence run's preflight started),
                                          polled every `poll` for `seconds`
    .in_flight(workflow, ref, sha)        the run of `workflow` on `ref` for `sha` still queued or running ("" none): the
                                          run a second dispatch of the same commit would only wait behind (run 3656,
                                          2026-10-29: 54 min queued behind 3653, the same commit, then run in full)
    .eta(workflow, branch_prefix, args)   "~N min (median of K recent runs)" over the newest 30 runs that ended success
                                          or failure on a branch starting so: an estimate to say, never a deadline

The selections are gh's `--jq`: what gh answers is the answer, a run id or the figures.
  backoff_next(seconds, cap)              the next poll's delay: x1.5, at least 1, at most the cap (DISPATCH_BACKOFF_CAP)

A workflow step asks from its shell (convergence-daily.yml):
  python3 scripts/lib/tool_dispatch.py in-flight <workflow> <ref> <sha>   the run's id on stdout (nothing: none);
                                          exit 1 when the list cannot be read (the caller dispatches), 64 usage

DISPATCH_GH (gh) is the gh a CLI call runs, DISPATCH_GH_TIMEOUT (60 s) bounds each gh call. Tested by
tool_dispatch_test.py against a fake gh."""
from __future__ import annotations

import json
import os
import re
import sys
import time
from dataclasses import dataclass
from typing import Callable, List, Mapping, Optional, Protocol, Sequence

import tool_cli
import tool_github
import tool_process

class GhCalls(Protocol):
    def call(self, args: Sequence[str], timeout: float, quiet: bool = False) -> tool_process.Captured: ...


@dataclass(frozen=True)
class Settings:
    gh_timeout: float = 60
    retry: float = 15
    rate_limit_max_wait: float = 900
    rate_limit_secondary: float = 60
    rate_limit_pad: float = 5
    find_seconds: float = 120
    backoff_cap: int = 300

    @staticmethod
    def of(env: Mapping[str, str], gh_timeout: Optional[float] = None) -> "Settings":
        """From DISPATCH_* (a caller's own gh bound, given, over DISPATCH_GH_TIMEOUT)."""
        def number(name: str, default: float) -> float:
            return float(env.get(name) or default)
        return Settings(gh_timeout or number("DISPATCH_GH_TIMEOUT", 60), number("DISPATCH_RETRY_SECONDS", 15),
                        number("DISPATCH_RATE_LIMIT_MAX_WAIT", 900), number("DISPATCH_RATE_LIMIT_SECONDARY_SECONDS", 60),
                        number("DISPATCH_RATE_LIMIT_PAD", 5), number("DISPATCH_FIND_SECONDS", 120),
                        int(number("DISPATCH_BACKOFF_CAP", 300)))


def say(line: str) -> None:
    print(f"dispatch: {line}", file=sys.stderr, flush=True)


def backoff_next(seconds: int, cap: int = 300) -> int:
    return min(max(seconds * 3 // 2, 1), cap)


class Dispatcher:
    def __init__(self, gh: GhCalls, settings: Settings, clock: Callable[[], float] = time.monotonic,
                 sleep: Callable[[float], None] = time.sleep) -> None:
        self.gh = gh
        self.settings = settings
        self.clock = clock
        self.sleep = sleep

    def _read(self, args: Sequence[str]) -> Optional[str]:
        done = self.gh.call(args, self.settings.gh_timeout, quiet=True)
        return done.stdout if done.code == 0 else None

    def dispatch(self, workflow: str, ref: str, args: Sequence[str] = ()) -> bool:
        for attempt in (1, 2, 3):
            done = self.gh.call(["workflow", "run", workflow, "--ref", ref, *args], self.settings.gh_timeout, quiet=True)
            if done.code == 0:
                return True
            sys.stderr.write(done.stderr)
            wait = self._retry_wait(workflow, done.stderr, attempt) if attempt < 3 else None
            if wait is None:
                break
            self.sleep(wait)
        say(f"could not dispatch {workflow} on {ref}")
        return False

    def _retry_wait(self, workflow: str, err: str, attempt: int) -> Optional[float]:
        """The pause before asking again, said; None when this refusal is not one to ask again."""
        if re.search(r"HTTP 5[0-9][0-9]", err):
            say(f"{workflow}: GitHub answered 5xx, retrying in {self.settings.retry:g}s (attempt {attempt} of 3)")
            return self.settings.retry
        wait = self._rate_limit_wait() if re.search(r"rate limit", err, re.IGNORECASE) else None
        if wait is not None:
            say(f"{workflow}: the API rate limit is spent, retrying in {wait:g}s, at its reset (attempt {attempt} of 3)")
        return wait

    def _rate_limit_wait(self) -> Optional[float]:
        """The seconds until the core limit resets (+ the pad), or the secondary limit's pause when the core one is not
        spent; None past the longest wait or when the limit is unreadable. Reading /rate_limit costs no request."""
        out = self._read(["api", "rate_limit", "--jq", '.resources.core | "\\(.remaining) \\(.reset)"'])
        parts = (out or "").split()
        if len(parts) != 2 or not all(part.isdigit() for part in parts):
            return None
        remaining, reset = int(parts[0]), int(parts[1])
        wait = self.settings.rate_limit_secondary if remaining > 0 else \
            max(reset - int(time.time()) + self.settings.rate_limit_pad, 0)
        return wait if wait <= self.settings.rate_limit_max_wait else None

    def find_run(self, workflow: str, since: str, predicate: str, args: Sequence[str] = ()) -> Optional[str]:
        select = f'[.[] | select(.createdAt >= "{since}") | select({predicate})] | sort_by(.createdAt) | last' \
                 ' | .databaseId // empty'
        return self._poll(["run", "list", "--workflow", workflow, "--event", "workflow_dispatch", "--limit", "20", *args,
                           "--json", "databaseId,headSha,createdAt,displayTitle", "--jq", select], workflow,
                          self.settings.find_seconds, 5)

    def find_titled(self, workflow: str, title: str, seconds: float, poll: float) -> Optional[str]:
        select = f'[.[] | select(.displayTitle == {json.dumps(title)})] | first | .databaseId // empty'
        return self._poll(["run", "list", "--workflow", workflow, "--event", "workflow_dispatch", "--limit", "50",
                           "--json", "databaseId,displayTitle", "--jq", select], workflow, seconds, poll)

    def in_flight(self, workflow: str, ref: str, sha: str) -> Optional[str]:
        select = f'[.[] | select(.headSha == {json.dumps(sha)} and .status != "completed")] | first | .databaseId // empty'
        out = self._read(["run", "list", "--workflow", workflow, "--branch", ref, "--limit", "20",
                          "--json", "databaseId,headSha,status", "--jq", select])
        if out is None:
            say(f"could not list {workflow}'s runs on {ref}")
            return None
        return out.strip()

    def _poll(self, args: Sequence[str], workflow: str, seconds: float, poll: float) -> Optional[str]:
        deadline = self.clock() + seconds
        while True:
            out = self._read(args)
            if out is None:
                say(f"could not list {workflow}'s runs")
                return None
            if out.strip():
                return out.strip()
            if self.clock() >= deadline:
                say(f"dispatched {workflow}, but no run of it appeared in {seconds:g}s")
                return None
            self.sleep(poll)

    def eta(self, workflow: str, branch_prefix: str, args: Sequence[str] = (), quiet: bool = False) -> Optional[str]:
        select = ('[.[] | select(.status == "completed" and (.conclusion == "success" or .conclusion == "failure")'
                  f' and ((.headBranch // "") | startswith({json.dumps(branch_prefix)})))'
                  ' | (.updatedAt | fromdateiso8601) - (.createdAt | fromdateiso8601)] | sort'
                  ' | if length == 0 then empty else "\\(.[length / 2 | floor] / 60 | ceil) \\(length)" end')
        out = self._read(["run", "list", "--workflow", workflow, "--limit", "30", *args,
                          "--json", "status,conclusion,createdAt,updatedAt,headBranch", "--jq", select])
        if out is None:
            if not quiet:
                say(f"could not list {workflow}'s runs for an ETA")
            return None
        figures = out.split()
        if len(figures) != 2 or not all(figure.isdigit() for figure in figures):
            return None
        return f"~{figures[0]} min (median of {figures[1]} recent runs)"


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if len(argv) != 4 or argv[0] != "in-flight":
        raise tool_cli.UsageError("usage: tool_dispatch.py in-flight <workflow> <ref> <sha>")
    settings = Settings.of(env)
    gh = tool_github.GitHub(tool_process.SubprocessRunner(), env, env.get("DISPATCH_GH") or "gh")
    found = Dispatcher(gh, settings).in_flight(argv[1], argv[2], argv[3])
    if found is None:
        return 1
    if found:
        print(found)
    return 0


if __name__ == "__main__":
    sys.exit(tool_cli.run_main(lambda: main(sys.argv[1:], os.environ)))
