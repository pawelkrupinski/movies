#!/usr/bin/env python3
# Purpose: The fan-out lint: a parallel runner in movies' scripts starts only through the governed fan-out (gov-parallel, tool_pool), or its allow row says why
"""THE FAN-OUT LINT (the owner, 2026-11-25: "fix structurally"; a guard so it stays fixed). Load 103 on 18 cores while
`agentctl gov status` said 0 admitted: the mutation tests' `xargs -P 4`, run beside eight others, held no ticket. Every
parallel runner sizes itself through THE GOVERNED FAN-OUT (scripts/jvm-gate.sh gov_width): `scripts/gov-parallel` for
`xargs -P`, `scripts/lib/tool_pool.py` governed_workers for a Python pool, run-script-tests' own pool. This refuses,
in every tool under the roots below:

- an `xargs ... -P N` (or --max-procs) anywhere but scripts/gov-parallel;
- a ThreadPoolExecutor, ProcessPoolExecutor, multiprocessing pool or pytest `-n auto` in a file that does not size it
  with governed_workers( -- unless the file has a row in scripts/ci/fanout-allow.tsv (<path> TAB <why>, 30+
  characters: a network-bound pool, a race under test, workers each holding a ticket of their own);
- an allow row whose file no longer fans out, or names no file (stale).

  python3 scripts/ci/fanout_lint.py [--root DIR]
Exit 0 clean, 1 findings (each `<path>:<line>: <why>`), 2 a malformed allow table."""
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from typing import Dict, List, Optional, Set, Tuple

ROOTS = ("scripts/", ".github/scripts/", ".github/actions/", "infra/bin/", "infra/nix/files/", "android/scripts/",
         "ios/scripts/")
HELPERS = {"scripts/gov-parallel", "scripts/lib/tool_pool.py", "scripts/ci/fanout_lint.py",
           "scripts/ci/fanout_lint_test.py"}
ALLOW = "scripts/ci/fanout-allow.tsv"
XARGS = re.compile(r"\bxargs\b[^|;&]*?(\s-P\s*\S|\s--max-procs)")
POOL = re.compile(r"\b(ThreadPoolExecutor|ProcessPoolExecutor|multiprocessing)\b|\s-n\s+auto\b")
GOVERNED = re.compile(r"\bgoverned_workers\(")
MIN_WHY = 30


def tool_files(root: str) -> List[str]:
    out = subprocess.run(["git", "-C", root, "ls-files", "--", *ROOTS], capture_output=True, text=True, check=True)
    return [p for p in out.stdout.splitlines() if os.path.isfile(os.path.join(root, p))]


def read_allow(root: str) -> Dict[str, str]:
    rows: Dict[str, str] = {}
    try:
        with open(os.path.join(root, ALLOW)) as f:
            for n, line in enumerate(f, 1):
                if not line.strip() or line.startswith("#"):
                    continue
                path, sep, why = line.rstrip("\n").partition("\t")
                if not sep or len(why.strip()) < MIN_WHY:
                    raise ValueError(f"{ALLOW}:{n}: <path> TAB <why of {MIN_WHY}+ characters>")
                rows[path] = why
    except FileNotFoundError:
        pass
    return rows


def is_comment(line: str) -> bool:
    return line.lstrip().startswith(("#", "//"))


def logical_lines(text: str) -> List[Tuple[int, str]]:
    """The lines, a shell line continued with a trailing backslash joined to its continuation (`xargs -n1 \\` with `-P 4`
    on the next line is one command), numbered by its first physical line."""
    out: List[Tuple[int, str]] = []
    start = 0
    parts: List[str] = []
    for n, line in enumerate(text.splitlines(), 1):
        start = start if parts else n
        if line.endswith("\\") and (parts or not is_comment(line)):
            parts.append(line[:-1].rstrip())
            continue
        out.append((start, " ".join([*parts, line])))
        parts = []
    if parts:
        out.append((start, " ".join(parts)))
    return out


def code_lines(text: str) -> List[Tuple[int, str]]:
    return [(n, line) for n, line in logical_lines(text) if not is_comment(line)]


XARGS_WHY = "an `xargs -P` fan-out: run it through scripts/gov-parallel --job J -P N"
POOL_WHY = f"a worker pool not sized by governed_workers (scripts/lib/tool_pool.py), nor allowed in {ALLOW}"


def line_fanout(line: str, governed: bool) -> Optional[str]:
    """None when the line starts no fan-out; else "" for a governed one, or why it is not."""
    if XARGS.search(line):
        return XARGS_WHY
    if POOL.search(line):
        return "" if governed else POOL_WHY
    return None


def file_findings(rel: str, text: str, allowed: bool) -> Tuple[bool, List[str]]:
    """(whether the file fans out, its findings)."""
    governed = bool(GOVERNED.search(text))
    seen = [(n, line_fanout(line, governed)) for n, line in code_lines(text)]
    fans = [(n, why) for n, why in seen if why is not None]
    return bool(fans), [] if allowed else [f"{rel}:{n}: {why}" for n, why in fans if why]


def read_text(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as f:
            return f.read()
    except (OSError, UnicodeDecodeError):
        return ""


def findings(root: str, files: List[str], allow: Dict[str, str]) -> List[str]:
    out: List[str] = []
    fanning: Set[str] = set()
    for rel in files:
        if rel in HELPERS:
            continue
        fans, found = file_findings(rel, read_text(os.path.join(root, rel)), rel in allow)
        if fans:
            fanning.add(rel)
        out += found
    out += [f"{ALLOW}: {rel}: stale -- the file no longer fans out" for rel in sorted(allow) if rel not in fanning]
    return out


def main(argv: List[str]) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--root", default=os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
    a = ap.parse_args(argv)
    try:
        allow = read_allow(a.root)
    except ValueError as e:
        print(f"fanout lint: {e}", file=sys.stderr)
        return 2
    found = findings(a.root, tool_files(a.root), allow)
    for line in found:
        print(line)
    if found:
        print(f"fanout lint: {len(found)} ungoverned fan-out(s) (THE GOVERNED FAN-OUT, scripts/ci/fanout_lint.py)",
              file=sys.stderr)
        return 1
    print("fanout lint: every fan-out governed")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
