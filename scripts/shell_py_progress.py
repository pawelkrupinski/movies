#!/usr/bin/env python3
"""THE SHELL-TO-PYTHON MIGRATION'S PROGRESS (/Users/pawel/Documents/shell-to-python-migration.md): the analysis's
MIGRATE tools (scripts/shell-py-progress/migrate-tools.tsv, its 95 in rank order) done, queued and left, by name, and the
repository's shell and Python lines -- tools and tests apart -- at the analysis's base against now.

  scripts/shell_py_progress.py [--start REV] [--rev REV] [--queued REF]... [--tools TSV] [--repo DIR] [--no-fetch]

  --start REV   the analysis's base (default 674883d11)
  --rev REV     now (default origin/main, fetched first unless --no-fetch)
  --queued REF  a queued change's branch (repeatable): a tool left at --rev but migrated there counts as queued
  --tools TSV   the MIGRATE list (default scripts/shell-py-progress/migrate-tools.tsv): `rank TAB path [TAB path now]`

A tool is DONE at a revision when its file there is Python (a python shebang), or a thin forwarder: at most 6 code
lines, one of them an `exec python3` (the migration's 3-line exec at the old path); GONE when its file is no longer
there (retired: counted with done); else LEFT. Counted files: every `*.sh`, `*.py` and extensionless file under a
directory with a shell or python shebang, outside any `src/`, `fixtures/`, `node_modules/` and `web/`; a TEST is a
`*-test.sh`, `*_test.py`, `test_*.py` or a file under a `test/` directory. Exit 0; 64 usage."""
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Tuple

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "scripts", "lib"))
import tool_git  # noqa: E402

TOOLS = os.path.join(ROOT, "scripts", "shell-py-progress", "migrate-tools.tsv")
START = "674883d11"
SKIPPED = re.compile(r"(^|/)src/|(^|/)fixtures/|node_modules/|^web/")
TEST = re.compile(r"(-test\.sh|_test\.py)$|(^|/)test_[^/]*\.py$|(^|/)test/")
THIN_LINES = 6


@dataclass(frozen=True)
class Tool:
    rank: int
    path: str
    now: str

    def read_at(self, tree: "Tree") -> str:
        """Its path in `tree`: the new one once the rename is there."""
        return self.now if self.now and tree.has(self.now) else self.path


class Tree:
    """One revision's files, read through a single `git cat-file --batch`."""

    def __init__(self, repo: str, rev: str) -> None:
        self.rev = rev
        listing = tool_git.Git.here(repo).text("ls-tree", "-r", "--name-only", rev)
        self.paths = [p for p in listing.splitlines() if p]
        self._present = set(self.paths)
        self._repo = repo
        self._texts: Dict[str, str] = {}

    def has(self, path: str) -> bool:
        return path in self._present

    def texts(self, paths: Sequence[str]) -> Dict[str, str]:
        wanted = [p for p in paths if p not in self._texts and p in self._present]
        if wanted:
            self._texts.update(zip(wanted, cat_files(self._repo, self.rev, wanted)))
        return {p: self._texts[p] for p in paths if p in self._texts}


def cat_files(repo: str, rev: str, paths: Sequence[str]) -> List[str]:
    request = "".join(f"{rev}:{p}\n" for p in paths).encode()
    raw = subprocess.run(["git", "-C", repo, "cat-file", "--batch"], input=request, check=True,
                         capture_output=True).stdout
    texts: List[str] = []
    at = 0
    for _ in paths:
        header_end = raw.index(b"\n", at)
        size = int(raw[at:header_end].split()[2])
        texts.append(raw[header_end + 1:header_end + 1 + size].decode("utf-8", "replace"))
        at = header_end + 1 + size + 1
    return texts


def language(path: str, text: str) -> Optional[str]:
    if path.endswith(".sh"):
        return "sh"
    if path.endswith(".py"):
        return "py"
    first = text.split("\n", 1)[0] if text.startswith("#!") else ""
    if "python" in first:
        return "py"
    if "bash" in first or "/sh" in first:
        return "sh"
    return None


def candidates(tree: Tree) -> List[str]:
    return [p for p in tree.paths if not SKIPPED.search(p)
            and (p.endswith((".sh", ".py")) or ("/" in p and "." not in os.path.basename(p)))]


def line_counts(tree: Tree) -> Dict[Tuple[str, str], Tuple[int, int]]:
    """(language, tool|test) -> (files, lines) at the tree."""
    counts: Dict[Tuple[str, str], Tuple[int, int]] = {}
    for path, text in tree.texts(candidates(tree)).items():
        lang = language(path, text)
        if lang is None:
            continue
        key = (lang, "test" if TEST.search(path) else "tool")
        files, lines = counts.get(key, (0, 0))
        counts[key] = (files + 1, lines + text.count("\n"))
    return counts


def code_lines(text: str) -> List[str]:
    return [line for line in text.splitlines() if line.strip() and not line.lstrip().startswith("#")]


def state(tree: Tree, tool: Tool) -> str:
    path = tool.read_at(tree)
    if not tree.has(path):
        return "gone"
    text = tree.texts([path]).get(path, "")
    if text.startswith("#!") and "python" in text.split("\n", 1)[0]:
        return "done"
    code = code_lines(text)
    if len(code) <= THIN_LINES and any(re.search(r"\bexec\b.*\bpython3?\b", line) for line in code):
        return "done"
    return "left"


def read_tools(path: str) -> List[Tool]:
    tools: List[Tool] = []
    with open(path) as f:
        for line in f:
            if line.startswith("#") or not line.strip():
                continue
            cells = line.rstrip("\n").split("\t") + ["", ""]
            tools.append(Tool(int(cells[0]), cells[1], cells[2]))
    return tools


def statuses(now: Tree, queued: Sequence[Tree], tools: Sequence[Tool]) -> Dict[str, List[Tool]]:
    by: Dict[str, List[Tool]] = {"done": [], "gone": [], "queued": [], "left": []}
    for tool in tools:
        status = state(now, tool)
        if status == "left" and any(state(q, tool) != "left" for q in queued):
            status = "queued"
        by[status].append(tool)
    return by


def render_tools(now: Tree, by: Dict[str, List[Tool]], total: int) -> List[str]:
    done = len(by["done"]) + len(by["gone"])
    lines = [f"MIGRATE tools: {done} done, {len(by['queued'])} queued, {len(by['left'])} left, of {total}"]
    for status, label in (("done", "done"), ("gone", "retired"), ("queued", "queued"), ("left", "left")):
        for tool in by[status]:
            lines.append(f"  {label:<8} {tool.rank:>3} {tool.read_at(now)}")
    return lines


def render_lines(start: Dict[Tuple[str, str], Tuple[int, int]],
                 now: Dict[Tuple[str, str], Tuple[int, int]]) -> List[str]:
    lines = ["lines (files)        start      now   change"]
    for key in (("sh", "tool"), ("sh", "test"), ("py", "tool"), ("py", "test")):
        (f0, l0), (f1, l1) = start.get(key, (0, 0)), now.get(key, (0, 0))
        lines.append(f"  {key[0]} {key[1]:<5} {l0:>8} ({f0:>3}) {l1:>8} ({f1:>3}) {l1 - l0:>+8}")
    added = sum(now.get(("py", k), (0, 0))[1] - start.get(("py", k), (0, 0))[1] for k in ("tool", "test"))
    removed = sum(start.get(("sh", k), (0, 0))[1] - now.get(("sh", k), (0, 0))[1] for k in ("tool", "test"))
    lines.append(f"  shell lines removed {removed:+d}, Python lines added {added:+d}")
    return lines


def options(argv: Optional[Sequence[str]]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(prog="shell_py_progress.py", add_help=True)
    parser.add_argument("--start", default=START)
    parser.add_argument("--rev", default="origin/main")
    parser.add_argument("--queued", action="append", default=[])
    parser.add_argument("--tools", default=TOOLS)
    parser.add_argument("--repo", default=ROOT)
    parser.add_argument("--no-fetch", action="store_true")
    try:
        return parser.parse_args(argv)
    except SystemExit as stop:
        raise SystemExit(0 if stop.code == 0 else 64)


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = options(argv)
    if not args.no_fetch:
        tool_git.Git.here(args.repo).call("fetch", "-q", "origin")
    start, now = Tree(args.repo, args.start), Tree(args.repo, args.rev)
    tools = read_tools(args.tools)
    out = [f"START {args.start} ({tool_git.Git.here(args.repo).text('log', '-1', '--format=%cs', args.start).strip()}), "
           f"NOW {args.rev} ({tool_git.Git.here(args.repo).text('log', '-1', '--format=%h %cs', args.rev).strip()})"]
    out += render_tools(now, statuses(now, [Tree(args.repo, q) for q in args.queued], tools), len(tools))
    out += render_lines(line_counts(start), line_counts(now))
    sys.stdout.write("\n".join(out) + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
