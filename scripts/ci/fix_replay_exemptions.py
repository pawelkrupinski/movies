#!/usr/bin/env python3
"""THE WEEKLY FIX-REPLAY EXEMPTION LIST (the owner, 2026-11-20: "3 approve and enforce for gate reds").

A `Fixes-Red:` commit for a gate red (Main, Identity model convergence, Identity takes) carries `Fix-Replay:
<command>`, which agentctl's land runs at origin/main and at the fix (base=FAIL head=PASS), or the escape hatch
`Fix-Replay: none — <why>` for a red that genuinely cannot replay (infra, a CI-only race). The escape hatches are
listed weekly for the owner, beside the coupling exemptions (.github/workflows/coupling-exemptions.yml), so he can
judge each reason.

  scripts/ci/fix_replay_exemptions.py [--since 7d] [--rev REV]

prints, as Markdown, each commit of REV (default origin/main, else HEAD) since `since` whose final trailer block holds
`Fix-Replay: none ...`: its date, sha, the reds it names and the reason. Exit 0 always (a report, never a gate)."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from typing import List, NamedTuple

NONE = re.compile(r"(?i)^none\b\s*(?:—|–|--|-|:)?\s*(.*)$")
FIELD, RECORD = "\x1f", "\x1e"


class Exemption(NamedTuple):
    day: str
    sha: str
    subject: str
    fixes: str
    reason: str


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def default_rev() -> str:
    found = subprocess.run(["git", "rev-parse", "-q", "--verify", "origin/main"], capture_output=True)
    return "origin/main" if found.returncode == 0 else "HEAD"


def exemptions(since: str, rev: str) -> List[Exemption]:
    """The `Fix-Replay: none` trailers (git's own trailer parse: the final block only) of rev's commits since `since`
    (`7d`, or anything `git log --since` reads), oldest first."""
    days = re.fullmatch(r"(\d+)d", since)
    after = f"{days.group(1)} days ago" if days else since
    fmt = FIELD.join(("%H", "%cs", "%s", "%(trailers:key=Fixes-Red,valueonly,unfold,separator=%x2c )",
                      "%(trailers:key=Fix-Replay,valueonly,unfold,separator=%x1d)")) + RECORD
    out = []
    for record in git("log", "--reverse", f"--since={after}", f"--format={fmt}", rev).split(RECORD):
        cols = record.strip("\n").split(FIELD)
        if len(cols) < 5:
            continue
        sha, day, subject, fixes, replays = cols
        for value in replays.split("\x1d"):
            m = NONE.match(value.strip())
            if value.strip() and m:
                out.append(Exemption(day, sha, subject, fixes.strip() or "-", m.group(1).strip()))
    return out


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--since", default="7d")
    parser.add_argument("--rev", default="")
    args = parser.parse_args(argv)
    rev = args.rev or default_rev()
    found = exemptions(args.since, rev)
    print(f"# Fix-Replay exemptions since {args.since} (on {rev})\n")
    for e in found:
        print(f"- {e.day} {e.sha[:9]} Fixes-Red: {e.fixes}: {e.reason}")
        print(f"  commit: {e.subject}")
    if not found:
        print("No `Fix-Replay: none` exemptions.")
    print("\nEach is a gate red's fix landed without a replay agentctl ran; a reason you do not accept: say so, and the "
          "next fix of that kind replays.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
