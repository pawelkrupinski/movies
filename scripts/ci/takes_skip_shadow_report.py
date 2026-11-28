#!/usr/bin/env python3
"""The takes skip gate shadow's tally: every skip-gate status of the newest commits, agreeing or not
THE SHADOW'S TALLY (scripts/ci/takes_skip_gate.py): every `takes skip gate (<cc>)` status the takes rows posted on the
newest <commits> commits of a ref (default main, 300) -- how often the gate said SKIP, how often the takes then moved
(a DISAGREEMENT: the gate was wrong, and must never be enforced), and how often it said RUN. Enforcing the gate
(the convergence workflows' `takes-skip: enforce`) waits for at least 20 SKIPs and none disagreeing.

usage: takes-skip-shadow-report.sh [ref] [commits]     env: GH_REPO (owner/repo), GH_TOKEN
Prints one line a status, then the tally; exit 0 when there are >= 20 SKIPs and 0 disagreements (ready to enforce),
1 otherwise, 2 when the statuses could not be read (a read that fails is never a clean tally).
Tests: scripts/ci/takes_skip_shadow_report_test.py."""
from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from typing import List, Mapping, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from vcs import tool_github  # noqa: E402

TOOL = "takes-skip-shadow-report"
TIMEOUT = 60.0
STATUS_FILTER = ('.[] | select(.context | startswith("takes skip gate (")) | '
                 '"\\(.context)\\t\\(.state)\\t\\(.description)"')
Status = Tuple[str, str, str]  # context, state, description
READY_SKIPS = 20


@dataclass
class Tally:
    seen: int = 0
    skip: int = 0
    disagree: int = 0
    run: int = 0

    def add(self, description: str) -> None:
        if "gate SKIP" in description:
            self.skip += 1
            self.disagree += 1 if "DISAGREE" in description.split("gate SKIP", 1)[1] else 0
        elif "gate RUN" in description:
            self.run += 1

    def ready(self) -> bool:
        return self.disagree == 0 and self.skip >= READY_SKIPS


def newest_per_context(lines: List[str]) -> List[Status]:
    """One status a context -- its newest, the first listed -- split at tabs (the description keeps any further)."""
    found: List[Status] = []
    for line in lines:
        context, state, description = (line.split("\t", 2) + ["", ""])[:3]
        if context and all(context != c for c, _, _ in found):
            found.append((context, state, description))
    return found


def say(line: str) -> None:
    print(f"{TOOL}: {line}", file=sys.stderr)


def commit_lines(github: tool_github.GitHub, repo: str, sha: str) -> Optional[List[str]]:
    answer = github.read(["api", f"repos/{repo}/commits/{sha}/statuses?limit=100&per_page=100", "--jq", STATUS_FILTER],
                         TIMEOUT)
    return None if answer is None else answer.splitlines()


def page_shas(github: tool_github.GitHub, repo: str, ref: str, page: int) -> Optional[List[str]]:
    answer = github.read(["api", f"repos/{repo}/commits?sha={ref}&limit=50&per_page=50&page={page}", "--jq", ".[].sha"],
                         TIMEOUT)
    return None if answer is None else answer.split()


def count_commit(github: tool_github.GitHub, repo: str, sha: str, tally: Tally) -> bool:
    """The commit's statuses printed and counted; False when they could not be read."""
    lines = commit_lines(github, repo, sha)
    if lines is None:
        say(f"could not read {sha[:9]}'s statuses")
        return False
    for context, state, description in newest_per_context(lines):
        print(f"{sha[:9]} {context} {state} {description}")
        tally.add(description)
    return True


def scan(github: tool_github.GitHub, repo: str, ref: str, want: int, tally: Tally) -> bool:
    """Every status of the newest `want` commits tallied; False when a read failed."""
    page = 1
    while tally.seen < want:
        shas = page_shas(github, repo, ref, page)
        if shas is None:
            say(f"could not list {ref}'s commits (page {page})")
            return False
        if not shas:
            break
        for sha in shas[:want - tally.seen]:
            tally.seen += 1
            if not count_commit(github, repo, sha, tally):
                return False
        page += 1
    return True


def report(ref: str, tally: Tally) -> int:
    print(f"{TOOL}: {tally.seen} commit(s) of {ref}: gate SKIP {tally.skip} (disagreeing {tally.disagree}), "
          f"gate RUN {tally.run}")
    if tally.ready():
        print(f"{TOOL}: READY -- {tally.skip} SKIPs, none disagreeing: the gate may be enforced")
        return 0
    print(f"{TOOL}: NOT READY -- enforcing needs >= {READY_SKIPS} SKIPs and 0 disagreements")
    return 1


def main(argv: List[str], env: Mapping[str, str], github: tool_github.GitHub) -> int:
    ref, want = (argv + ["main", "300"][len(argv):])[:2]
    repo = env.get("GH_REPO", "")
    if not repo:
        say("GH_REPO names no repository")
        return 2
    tally = Tally()
    return report(ref, tally) if scan(github, repo, ref, int(want), tally) else 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment(), tool_github.GitHub.here()))
