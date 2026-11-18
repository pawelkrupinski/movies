#!/usr/bin/env python3
"""THE TRAILER PLACEMENT RULE: git reads a `Key: value` line as a trailer only in the message's final trailer block --
its last paragraph, beside `Co-Authored-By:` with no blank line between. Written anywhere else (a paragraph of its own
above Co-Authored-By, indented, mid-body) it is prose, and every ratchet reading the trailer sees none: the push is
refused as if no trailer was given (2026-11-19: an agent's `Coupling:` line above a blank line cost a landing round).

`misplaced(message)` finds each known key's line outside the block, with its line number; `misplaced_notes(rng, key,
path)` says so for the commits of a range a ratchet refuses for want of that key; the ratchets print it beside their
refusal.

  scripts/ci/trailers.py lint --range A..B [--range ...]   warn (stderr) about every misplaced known-trailer line of
                                                           the pushed commits; always exits 0 (pre-push runs it)"""

from __future__ import annotations

import re
import subprocess
import sys
from typing import Iterable, List, NamedTuple, Optional

# Every trailer key a ratchet, commit rule or the red gate reads.
KNOWN_KEYS = ("Coupling", "Complexity", "Duplication", "Length", "Pure-Move", "Fixes-Red", "Identity-Design",
              "Drift-Harness-Refactor")
KEY_LINE = re.compile(r"^(\s*)(" + "|".join(map(re.escape, KNOWN_KEYS)) + r")\s*:(.*)$", re.IGNORECASE)
CANONICAL = {k.lower(): k for k in KNOWN_KEYS}


class Misplaced(NamedTuple):
    key: str    # as KNOWN_KEYS spells it
    line: int   # 1-based in the message, the subject line 1
    text: str   # the line as written, stripped


def git(*args: str, stdin: Optional[str] = None) -> str:
    return subprocess.run(["git", *args], input=stdin, check=True, capture_output=True, text=True).stdout


def parsed(message: str) -> set:
    """(key lowercased, value) of every trailer git reads from `message`."""
    out = git("interpret-trailers", "--parse", stdin=message)
    return {(k.strip().lower(), v.strip()) for k, sep, v in (line.partition(":") for line in out.splitlines()) if sep}


def misplaced(message: str, keys: Iterable[str] = KNOWN_KEYS) -> List[Misplaced]:
    """Each line of `message` (the subject aside) starting with one of `keys` that git does not read as a trailer."""
    wanted = {k.lower() for k in keys}
    lines = message.splitlines()
    candidates = [(i, m) for i, line in enumerate(lines[1:], start=2) if (m := KEY_LINE.match(line))
                  and m.group(2).lower() in wanted]
    if not candidates:
        return []
    read = parsed(message)
    nonblank = [i for i, line in enumerate(lines, start=1) if line.strip()]
    block_start = next((i + 1 for i in range(nonblank[-1] - 1, 0, -1) if not lines[i - 1].strip()), 1)
    found = []
    for number, m in candidates:
        key, value = m.group(2).lower(), m.group(3).strip()
        if m.group(1) or number < block_start or (key, value) not in read:
            found.append(Misplaced(CANONICAL[key], number, lines[number - 1].strip()))
    return found


def explain(m: Misplaced) -> str:
    return (f"found `{m.text}` on line {m.line}, but it is not in the final trailer block (it must sit with"
            " Co-Authored-By, no blank line between); git does not read it as a trailer")


def message_of(sha: str) -> str:
    return git("log", "-1", "--format=%B", sha)


def misplaced_notes(rng: str, key: str, path: Optional[str] = None) -> List[str]:
    """`<sha9>: found ...` for every `key:` line of a commit of `rng` (changing `path`, when given) that git does not
    read as a trailer: why a ratchet refusing for want of `key` saw none."""
    shas = git("rev-list", rng, *(["--", path] if path else [])).split()
    return [f"{sha[:9]}: {explain(m)}" for sha in shas for m in misplaced(message_of(sha), (key,))]


def main(argv: List[str]) -> int:
    usage = "usage: trailers.py lint --range A..B [--range ...]"
    if not argv or argv[0] != "lint" or len(argv) < 3 or len(argv[1:]) % 2 or any(a != "--range" for a in argv[1::2]):
        print(usage, file=sys.stderr)
        return 2
    try:
        for rng in argv[2::2]:
            for sha in git("rev-list", "--reverse", rng).split():
                for m in misplaced(message_of(sha)):
                    print(f"trailers: {sha[:9]}: {explain(m)}", file=sys.stderr)
    except subprocess.CalledProcessError as e:
        print(f"trailers: could not read the range: {(e.stderr or '').strip() or e}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
