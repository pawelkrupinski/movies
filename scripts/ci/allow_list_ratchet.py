#!/usr/bin/env python3
# A NEW CHECK LANDS GREEN; ITS ALLOW-LIST ONLY SHRINKS (convergence-failures-2026-10-29 §4 G1, recommendation 2).
#
# Why: 15 of 36 red convergence runs and ~43 of 67 red hours in the week to 2026-10-29 came from a check turned on while
# main's own pinned recording already failed it -- the drift gate 9bb54c1a4 (10-23) and the flip gate a2c684f80 (10-28).
# Main then stayed red for a day while fixers chased findings that had existed before the gate did. The rule:
#   - a commit that gives birth to a check (or tightens one) carries a `New-Check: <what>` trailer, and EVERYTHING the
#     check finds at birth goes onto its allow-list in that commit, each row TEMPORARY with an owner and when it goes:
#     `TEMPORARY owner: <who>; remove when <fix>; <why>`. The check then lands green;
#   - any other commit may only REMOVE rows from a registered allow-list (.github/allow-lists.txt): a finding after
#     birth is fixed, never listed.
# THE KNOWN-FLIPS BASELINE ONLY SHRINKS (test/resources/fixtures/identity-flips-baseline.tsv; the owner, 2026-10-29: "fail
# build only on new flips, focus in eliminating all old flips and then fail on all flips"): every flip main showed when the
# gate turned to failing on new flips only is a row there, generated (scripts/ci/identity_flips_baseline.py). No commit
# adds a row to it, whatever its trailers: no override. Only its creation, once, writes rows; a file that existed before
# is never made again.
# A registered allow-list is a TSV whose last column is the row's reason; `#` lines and the header (its first non-comment
# line) are not rows. Every allow-list file in the repository must be registered (AllowListRatchetSpec), so a new check
# with a new list gets the ratchet the moment it is added.
#
# usage: allow_list_ratchet.py --range A..B [--range C..D ...] [--registry .github/allow-lists.txt]
# Prints each refused row and why; exit 0 none refused, 1 some, 2 could not read the range.
# Run by the pre-push hook's `allowlists` check on any push touching a registered list. Tested by allow_list_ratchet_test.py.

from __future__ import annotations

import os
import re
import subprocess
import sys

REGISTRY = ".github/allow-lists.txt"
TRAILER = "New-Check"
BASELINES = ("test/resources/fixtures/identity-flips-baseline.tsv",)
TEMPORARY = re.compile(r"^TEMPORARY\b.*\bowner:\s*\S+.*\bremove when\b\s*\S+", re.I)


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def registered(registry: str) -> list[str]:
    try:
        with open(registry) as f:
            return [line.strip() for line in f if line.strip() and not line.lstrip().startswith("#")]
    except OSError:
        return []


def rows(text: str) -> list[str]:
    """The data rows of an allow-list: no comment, no blank line, no header (the first other line)."""
    body = [line for line in text.splitlines() if line.strip() and not line.lstrip().startswith("#")]
    return body[1:]


def at(rev: str, path: str) -> str:
    try:
        return git("show", f"{rev}:{path}")
    except subprocess.CalledProcessError:
        return ""  # absent there: no rows


def judge(added: list[str], births: bool) -> list[str]:
    """Why each added row is refused; empty when every one may land."""
    why = []
    for row in added:
        reason = row.split("\t")[-1].strip()
        if not births:
            why.append(f"{row!r}: an allow-list only shrinks -- a finding after its check's birth is fixed, never listed "
                       f"(a check's birth carries `{TRAILER}: <what>`)")
        elif not TEMPORARY.search(reason):
            why.append(f"{row!r}: a row added at a check's birth is TEMPORARY with an owner and when it goes: "
                       "`TEMPORARY owner: <who>; remove when <fix>; <why>`")
    return why


def existed_before(sha: str, path: str) -> bool:
    """Whether any ancestor of `sha` held `path`: a baseline is created once, never made again."""
    try:
        return bool(git("log", "--format=%H", "-1", f"{sha}^", "--", path).strip())
    except subprocess.CalledProcessError:
        return False


def commits(rng: str) -> list[str]:
    return [c for c in git("rev-list", "--reverse", rng).split() if c]


def check(ranges: list[str], registry: str) -> list[str]:
    lists = registered(registry)
    refused = []
    for rng in ranges:
        for sha in commits(rng):
            parent = f"{sha}^"
            trailers = git("log", "-1", f"--format=%(trailers:key={TRAILER},valueonly)", sha).strip()
            changed = set(git("diff-tree", "--no-commit-id", "--name-only", "-r", "--root", sha).split())
            for path in lists:
                if path not in changed:
                    continue
                before = set(rows(at(parent, path)))
                added = [r for r in rows(at(sha, path)) if r not in before]
                if path in BASELINES:
                    born = not before and not at(parent, path) and not existed_before(sha, path)
                    refused += [f"{sha[:9]} {path}: {r!r}: the known-flips baseline only shrinks -- a new flip is fixed, never "
                                "baselined (no override)" for r in added if not born]
                    continue
                for reason in judge(added, bool(trailers)):
                    refused.append(f"{sha[:9]} {path}: {reason}")
    return refused


def main(argv: list[str]) -> int:
    ranges, registry, i = [], REGISTRY, 0
    while i < len(argv):
        if argv[i] == "--range" and i + 1 < len(argv):
            ranges.append(argv[i + 1])
            i += 2
        elif argv[i] == "--registry" and i + 1 < len(argv):
            registry = argv[i + 1]
            i += 2
        else:
            print(f"allow-list-ratchet: unknown argument {argv[i]!r}", file=sys.stderr)
            return 2
    if not ranges:
        print("usage: allow_list_ratchet.py --range A..B [...] [--registry FILE]", file=sys.stderr)
        return 2
    if not os.path.exists(registry):
        print(f"allow-list-ratchet: no registry {registry}", file=sys.stderr)
        return 2
    try:
        refused = check(ranges, registry)
    except subprocess.CalledProcessError as e:
        print(f"allow-list-ratchet: could not read the range: {e.stderr.strip() or e}", file=sys.stderr)
        return 2
    for line in refused:
        print(f"allow-list-ratchet: {line}", file=sys.stderr)
    return 1 if refused else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
