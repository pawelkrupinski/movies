#!/usr/bin/env python3
"""THE TRAILER PLACEMENT RULE: git reads a `Key: value` line as a trailer only in the message's final trailer block --
its last paragraph, beside `Co-Authored-By:` with no blank line between. Written anywhere else (a paragraph of its own
above Co-Authored-By, indented, mid-body) it is prose, and every ratchet reading the trailer sees none: the push is
refused as if no trailer was given (2026-11-19: an agent's `Coupling:` line above a blank line cost a landing round).

`misplaced(message)` finds each known key's line outside the block, with its line number; `misplaced_notes(rng, key,
path)` says so for the commits of a range a ratchet refuses for want of that key; the ratchets print it beside their
refusal.

THE ATTRIBUTION RULE (2026-11-19): a ratchet's trailer justifies the rise of the commit carrying it, never another's.
A batch push stacks several agents' changes, and a trailer read from any commit of the range let one change's
`Coupling:` excuse another's rise. `unjustified_rises(rng, path, key, rose)` walks the commits of the range changing the
checked-in file `path` (a baseline, a layer or exemption table) and judges each one's own step -- its parent's text to
its own, as if it were pushed alone: every commit whose step grows the file must carry the trailer. The file itself is the
per-commit measure: each change records its rows in its own commit (agentctl folds a regenerated baseline into the
member's commit), so no per-commit sbt run is needed.

DIAGNOSTICS ARE NO FIX (agentctl, 2026-11-21: a853b2ee4 said `Fixes-Red: 94` and only added diagnostics to Order
independence; agentctl took it for the red's fix, released the fixer, and nobody worked the red for a day): a commit
that only collects evidence says `Diagnoses-Red: <run>`, which never moves the red to fix-pending. `lint` suggests it for
a `Fixes-Red:` commit touching only CI or report plumbing (`plumbing_only`).

  scripts/ci/trailers.py lint --range A..B [--range ...]   warn (stderr) about every misplaced known-trailer line of
                                                           the pushed commits, and every Fixes-Red commit of plumbing
                                                           alone; always exits 0 (pre-push runs it)"""

from __future__ import annotations

import re
import subprocess
import sys
from typing import Callable, Iterable, List, NamedTuple, Optional

# Every trailer key a ratchet, commit rule, the red gate or agentctl (Rework-Of: a revert notice closed) reads.
KNOWN_KEYS = ("Coupling", "Complexity", "Duplication", "Length", "Typing", "Pure-Move", "Fixes-Red", "Fix-Replay",
              "Diagnoses-Red", "Identity-Design", "Drift-Harness-Refactor", "Rework-Of", "Design", "Naming")
# CI and report plumbing: what a commit collecting a red's evidence touches, and a fix seldom does alone
PLUMBING = (".github/", "scripts/run-script-tests", "scripts/ci/report", "scripts/ci/job-summary", "docs/")
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
        # git unfolds a value continued on indented lines, so the line holds only the read value's start
        if m.group(1) or number < block_start or not any(k == key and v.startswith(value) for k, v in read):
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


def print_misplaced(rng: str, key: str, path: Optional[str] = None) -> None:
    """Says misplaced_notes, indented: under a ratchet's refusal for want of `key`."""
    for note in misplaced_notes(rng, key, path):
        print(f"  {note}")


def carries(sha: str, key: str) -> bool:
    """Whether commit `sha` carries a non-empty `key:` trailer (git's reading: the final trailer block)."""
    value = git("log", "-1", f"--format=%(trailers:key={key},valueonly,separator=%x20)", sha)
    return bool(value.strip())


def text_at(rev: str, path: str) -> str:
    """`path` at `rev`, "" where it does not exist (or `rev` does not: a root commit's parent)."""
    shown = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return shown.stdout if shown.returncode == 0 else ""


def exists_at(rev: str, path: str) -> bool:
    """Whether `path` exists at `rev` (an empty file does: text_at's "" does not say)."""
    return subprocess.run(["git", "cat-file", "-e", f"{rev}:{path}"], capture_output=True).returncode == 0


def unjustified_rises(rng: str, path: str, key: str, rose: Callable[[str, str], List[str]]) -> List[str]:
    """The commits of `rng` whose own change to `path` grows it (`rose(its parent's text, its text)`, the ratchet's growth,
    non-empty) and that carry no `key:` trailer, oldest first: `<sha9> <subject>: <its rise>` each (THE ATTRIBUTION RULE,
    above). A commit creating or deleting `path` is no step (the range's own check has no baseline to compare). When no
    single commit's step grows it although the range does (each step within its parent's limit), some commit of the
    range changing `path` must carry the trailer. Empty when every rise is justified where it was made."""
    changed = git("rev-list", "--reverse", rng, "--", path).split()
    raisers, refused = 0, []
    for sha in changed:
        before, after = text_at(f"{sha}^", path), text_at(sha, path)
        # created or deleted is no step; a baseline committed EMPTY is a file, and what is added to it is growth
        own = rose(before, after) if exists_at(f"{sha}^", path) and exists_at(sha, path) else []
        if own:
            raisers += 1
            if not carries(sha, key):
                subject = git("log", "-1", "--format=%s", sha).strip()
                refused.append(f"{sha[:9]} {subject}: " + "; ".join(own))
    if not raisers and not any(carries(sha, key) for sha in changed):
        refused.append(f"no commit of {rng} changing {path} carries `{key}:`")
    return refused


def plumbing_only(paths: Iterable[str]) -> bool:
    """Every path is CI or report plumbing (PLUMBING), or Markdown."""
    paths = list(paths)
    return bool(paths) and all(p.startswith(PLUMBING) or p.endswith(".md") for p in paths)


def diagnoses_hint(sha: str) -> Optional[str]:
    """The suggestion for a `Fixes-Red:` commit of plumbing alone (DIAGNOSTICS ARE NO FIX), or None."""
    message = message_of(sha)
    if not any(k == "fixes-red" for k, _v in parsed(message)):
        return None
    paths = git("diff-tree", "--no-commit-id", "--name-only", "-r", "--root", sha).split()
    if not plumbing_only(paths):
        return None
    return ("it says `Fixes-Red:` but touches only CI or report plumbing (" + ", ".join(paths[:3])
            + (", ..." if len(paths) > 3 else "") + "): if it only collects evidence, say `Diagnoses-Red: <run>` "
            "instead -- a Fixes-Red moves the red to fix-pending and stands its fixer down")


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
                hint = diagnoses_hint(sha)
                if hint:
                    print(f"trailers: {sha[:9]}: {hint}", file=sys.stderr)
    except subprocess.CalledProcessError as e:
        print(f"trailers: could not read the range: {(e.stderr or '').strip() or e}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
