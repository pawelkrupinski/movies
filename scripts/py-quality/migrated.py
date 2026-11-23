"""THE MIGRATED TOOLS (scripts/py-quality/report.py; the owner, 2026-11-23): the files of the shell-to-Python migration's
tools, held to every limit with NO baseline row -- a row there is refused even under its kind's trailer ("migrated tools
stay under the bar -- split it"). They are every tool scripts/shell-py-progress/migrate-tools.tsv lists (its path, and
its path now), and every `.py` file new since the migration's base (the list's header, `its base <sha>`) that one of
them reaches: named in its text (a shell entry's `exec python3 "$(dirname "$0")/x.py"`) or imported (imports.py's
graph), transitively through new files only -- a file the base already had is the old code the tool calls, frozen as
it was, and what it imports is not the tool's."""
from __future__ import annotations

import os
import re
import subprocess
from typing import Callable, Dict, Iterable, List, Set

TOOLS = "scripts/shell-py-progress/migrate-tools.tsv"
BASE = re.compile(r"\bits base ([0-9a-f]{7,40})\b")
NAMED = re.compile(r"[\w./-]*\w\.py\b")


def listed(text: str) -> List[str]:
    """The list's tool paths (`rank TAB path [TAB path now]`), both columns."""
    out: List[str] = []
    for line in text.splitlines():
        if line.startswith("#"):
            continue
        out += [p for p in line.split("\t")[1:3] if p.strip()]
    return out


def named(path: str, text: str, measured: Set[str]) -> Set[str]:
    """The measured `.py` files `text` (the file at `path`) names: a bare name beside it, or a repository path."""
    out: Set[str] = set()
    for name in NAMED.findall(text):
        for candidate in (os.path.normpath(os.path.join(os.path.dirname(path), os.path.basename(name))), name):
            if candidate in measured:
                out.add(candidate)
    return out


def reached(root: str, tools: Iterable[str], sources: Dict[str, str], edges: Dict[str, Set[str]],
            new: Callable[[str], bool]) -> Set[str]:
    """The tools' own files and every new measured file they reach, through new files only (an old one is never taken,
    so what it imports is never followed)."""
    measured = set(sources)
    found: Set[str] = set()
    todo = [t for t in tools if t in measured or os.path.isfile(os.path.join(root, t))]
    while todo:
        path = todo.pop()
        if path in found:
            continue
        found.add(path)
        text = sources[path] if path in sources else read(os.path.join(root, path))
        todo += [p for p in named(path, text, measured) | edges.get(path, set()) if new(p)]
    return found


def read(path: str) -> str:
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def files(root: str, sources: Dict[str, str], edges: Dict[str, Set[str]]) -> Set[str]:
    """The migrated tools' files in the checkout at `root`; none without the list. A base the clone lacks is an error."""
    text = read(os.path.join(root, TOOLS))
    if not text:
        return set()
    base = BASE.search(text)
    if base is None:
        raise ValueError(f"{TOOLS} names no migration base (`its base <sha>` in its header)")
    shown = subprocess.run(["git", "-C", root, "ls-tree", "-r", "--name-only", base.group(1)],
                           capture_output=True, text=True)
    if shown.returncode != 0:
        raise ValueError(f"the migration base {base.group(1)} ({TOOLS}) is not in this clone: git fetch origin main")
    before = set(shown.stdout.splitlines())
    return reached(root, listed(text), sources, edges, lambda p: p not in before)
