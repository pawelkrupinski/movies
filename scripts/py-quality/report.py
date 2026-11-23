#!/usr/bin/env python3
"""THE PYTHON QUALITY REPORT (scripts/py-quality-report): the Scala and JS/TS code-quality measures (CLAUDE.md, "Code
quality ratchets") taken over the repository's Python tools -- every `.py` and extension-less Python tool under scripts/,
.github/scripts/, infra/bin/, infra/nix/bin/, infra/nix/files/, android/scripts/ and ios/scripts/ -- and GATED by THE PYTHON QUALITY RATCHET (scripts/ci/py_quality_ratchet.py, the pre-push
`pyquality` check) against test/resources/py-quality-baseline.tsv:

  scripts/py-quality-report --check [--range A..B]... [--paths FILE]   held rows against the baseline (exit 1 past it);
                                     --paths (the push's changed paths, one a line) type-checks only those files
  scripts/py-quality-report --update                the baseline rewritten from the measure (falls recorded; a rise is
                                                    committed with its kind's trailer)
  scripts/py-quality-report [--out DIR]             the worst rows as markdown; DIR keeps over-limit.tsv too
  scripts/py-quality-report --tools-dir             where the pinned pyright lives (installed or not)

The measures (stdlib `ast` and `tokenize`, and pyright from scripts/py-quality's lock):
  cognitive <= 10, cyclomatic <= 10, inner defs <= 2, function <= 50 lines   functions.py
  file <= 500 lines; clone groups of 50+ tokens                               clones.py
  fan-out <= 9 (measured modules imported), no import cycle                    imports.py
  strict pyright errors: 0 for a new file                                      pyright.py
  the migrated tools' files, held with NO row (a trailer admits none)          migrated.py"""
from __future__ import annotations

import argparse
import ast
import os
import subprocess
import sys
import tempfile
from typing import Dict, List, Optional, Sequence, Set, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "ci"))
import clones  # noqa: E402
import functions  # noqa: E402
import imports  # noqa: E402
import migrated  # noqa: E402
import pyright  # noqa: E402
import baseline_ratchet  # noqa: E402
import py_quality_ratchet  # noqa: E402

ROOTS = ("scripts", ".github/scripts", "infra/bin", "infra/nix/bin", "infra/nix/files", "android/scripts", "ios/scripts")
LIMITS: Tuple[Tuple[str, float], ...] = (
    ("cognitive", 10), ("cyclomatic", 10), ("inner-defs", 2), ("method-lines", 50), ("file-lines", 500),
    ("copies", 0), ("clone-tokens", clones.MIN_TOKENS), ("fanout", 9), ("cycle", 0), ("pyright", 0))
Row = Tuple[str, str, float]


def is_python(root: str, path: str) -> bool:
    if path.endswith(".py"):
        return True
    if "." in os.path.basename(path):
        return False
    try:
        with open(os.path.join(root, path), "rb") as f:
            first = f.readline(200)
    except OSError:
        return False
    return first.startswith(b"#!") and b"python" in first


def python_files(root: str) -> List[str]:
    listed = subprocess.run(["git", "-C", root, "ls-files", "--cached", "--others", "--exclude-standard", "--", *ROOTS],
                            check=True, capture_output=True, text=True).stdout.split("\n")
    return sorted(p for p in set(listed) if p and os.path.isfile(os.path.join(root, p)) and is_python(root, p))


def read_sources(root: str, paths: Sequence[str]) -> Dict[str, str]:
    out: Dict[str, str] = {}
    for p in paths:
        with open(os.path.join(root, p), encoding="utf-8", errors="replace") as f:
            out[p] = f.read()
    return out


def parse_all(sources: Dict[str, str]) -> Dict[str, ast.AST]:
    trees: Dict[str, ast.AST] = {}
    for path, text in sources.items():
        try:
            trees[path] = ast.parse(text, filename=path)
        except SyntaxError as e:
            print(f"py-quality-report: {path} does not parse ({e.msg}, line {e.lineno}): not measured", file=sys.stderr)
    return trees


def function_rows(trees: Dict[str, ast.AST]) -> List[Row]:
    rows: List[Row] = []
    for path, tree in sorted(trees.items()):
        seen: Dict[str, int] = {}
        for fn in functions.functions(tree):
            seen[fn.name] = seen.get(fn.name, 0) + 1
            key = f"{path}:{fn.name}" + (f"#{seen[fn.name]}" if seen[fn.name] > 1 else "")
            rows += [("cognitive", key, fn.cognitive), ("cyclomatic", key, fn.cyclomatic),
                     ("inner-defs", key, fn.inner_defs), ("method-lines", key, fn.lines)]
    return rows


def file_rows(sources: Dict[str, str], trees: Dict[str, ast.AST]) -> List[Row]:
    rows: List[Row] = [("file-lines", p, len(text.splitlines())) for p, text in sorted(sources.items())]
    copies: Dict[str, int] = {}
    for group in clones.groups(sources):
        key = "|".join(sorted({c.file for c in group.copies}))
        copies[key] = copies.get(key, 0) + len(group.copies)
    rows += [("copies", key, n) for key, n in sorted(copies.items())]
    edges = imports.graph(trees)
    rows += [("fanout", p, len(targets)) for p, targets in sorted(edges.items())]
    rows += [("cycle", "|".join(members), len(members)) for members in imports.cycles(edges)]
    return rows


def pyright_rows(root: str, files: List[str], typed: Optional[List[str]], baseline: baseline_ratchet.Rows) -> List[Row]:
    """Every file's strict errors -- or, with `typed` (the push's own files), those files' and the baseline's rows for
    the rest, which nothing in the push can have moved."""
    asked = pyright.checked(files if typed is None else [p for p in typed if p in files])
    counts = pyright.errors(root, asked, os.environ)
    rows: List[Row] = [("pyright", p, n) for p, n in sorted(counts.items())]
    if typed is not None:
        rows += [(k, key, v) for (k, key), v in baseline.items() if k == "pyright" and key not in counts]
    return rows


def over_limit(rows: List[Row]) -> List[Row]:
    limits = dict(LIMITS)
    return [("limit", kind, value) for kind, value in LIMITS] + [r for r in rows if r[2] > limits[r[0]]]


def measure(root: str, typed: Optional[List[str]], checking: bool) -> Tuple[List[Row], Set[str]]:
    """The rows, and, `checking`, the migrated tools' files (migrated.py: held with no row)."""
    files = python_files(root)
    sources = read_sources(root, files)
    trees = parse_all(sources)
    baseline_path = os.path.join(root, py_quality_ratchet.BASELINE)
    baseline: baseline_ratchet.Rows = {}
    if typed is not None and os.path.isfile(baseline_path):
        with open(baseline_path) as f:
            baseline = baseline_ratchet.parse(f.read())
    migrated_files: Set[str] = migrated.files(root, sources, imports.graph(trees)) if checking else set()
    return function_rows(trees) + file_rows(sources, trees) + pyright_rows(root, files, typed, baseline), migrated_files


def write_rows(path: str, rows: List[Row]) -> None:
    with open(path, "w") as f:
        f.write("".join(f"{k}\t{key}\t{baseline_ratchet.show(v)}\n" for k, key, v in rows))


def markdown(rows: List[Row]) -> str:
    lines = ["# Python quality report", ""]
    for kind, limit in LIMITS:
        worst = sorted((r for r in rows if r[0] == kind), key=lambda r: -r[2])[:10]
        lines += [f"## {kind} (limit {baseline_ratchet.show(limit)})", ""]
        lines += [f"- `{key}`: {baseline_ratchet.show(v)}" for _, key, v in worst if v > limit] or ["- none past it"]
        lines.append("")
    return "\n".join(lines)


def read_paths(path: Optional[str]) -> Optional[List[str]]:
    if path is None:
        return None
    with open(path) as f:
        return [line.strip() for line in f if line.strip()]


def run(args: argparse.Namespace) -> int:
    if args.tools_dir:
        print(pyright.tools_dir(os.environ))
        return 0
    os.chdir(args.root)
    checking = args.ratchet == "check"
    rows, migrated_files = measure(args.root, read_paths(args.paths) if checking else None, checking)
    out = args.out or tempfile.mkdtemp(prefix="py-quality-")
    os.makedirs(out, exist_ok=True)
    current = os.path.join(out, "over-limit.tsv")
    write_rows(current, over_limit(rows))
    if args.ratchet:
        return baseline_ratchet.run(py_quality_ratchet.spec_for(migrated_files), args.ratchet, current, args.range)
    print(markdown(rows))
    return 0


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(prog="py-quality-report", description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--check", dest="ratchet", action="store_const", const="check")
    parser.add_argument("--update", dest="ratchet", action="store_const", const="update")
    parser.add_argument("--range", action="append", default=[])
    parser.add_argument("--paths")
    parser.add_argument("--out")
    parser.add_argument("--root", default=os.path.dirname(os.path.dirname(HERE)))
    parser.add_argument("--tools-dir", action="store_true")
    try:
        return run(parser.parse_args(argv))
    except pyright.ToolMissing as e:
        print(f"py-quality-report: {e}", file=sys.stderr)
        return 2
    except (OSError, RuntimeError, subprocess.CalledProcessError, ValueError) as e:
        print(f"py-quality-report: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
