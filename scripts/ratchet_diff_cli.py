#!/usr/bin/env python3
"""The ratchet diff (CLAUDE.md, the coupling and code-quality ratchets): every frozen row that changed between two refs
THE RATCHET DIFF (CLAUDE.md, the coupling and code-quality ratchets): every frozen row that changed between two refs --
complexity (cognitive, cyclomatic, inner-defs), length (method, file), duplication, fan-out, package D, cycles, and the
JS/TS ratchet's rows (`js <kind>`, test/resources/js-quality-baseline.tsv) -- as a
before -> after table marked fell / rose / new / measured / removed / moved, then per metric the totals and how many rows are
still over the limit. A lane's report pastes this, never numbers written by hand.

  scripts/ratchet-diff <base>..<head>          markdown, read from the committed baselines at each ref (cheap)
  scripts/ratchet-diff <base>                  the same, <head> HEAD
  scripts/ratchet-diff --tsv <base>..<head>    the same rows as TSV
  scripts/ratchet-diff --measure <base>        the head side measured from this WORKING TREE (scripts/quality-report
                                               and scripts/coupling-report --out, through sbt-warm, and
                                               scripts/js-quality-report's production areas) instead of its
                                               committed baselines: a fall nobody recorded with --update shows too
                                               (run this checkout's own copy: another's measures its tree)

The table and its marks: scripts/ci/ratchet_diff.py.
"""
from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from typing import List, NoReturn, Optional

# stdlib only: ratchet_diff_cli_test.py runs a copy of it in a scratch checkout, beside own-checkout and nothing else
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# each measure: its tool, its --out directory's name, and the rows file it must leave there
MEASURES = (("quality", "quality-report", "rows.tsv"), ("coupling", "coupling-report", "rows.tsv"),
            ("js", "js-quality-report", "over-limit.tsv"))
ROW_FLAGS = {"quality": "--quality-rows", "coupling": "--coupling-rows", "js": "--js-rows"}


class Refused(Exception):
    """Exit 2, with its line on stderr (none: the line was already said)."""


@dataclass
class Options:
    measure: bool = False
    tsv: bool = False
    range: str = ""


def read(argv: List[str]) -> Optional[Options]:
    """The options, None when --help came first (each word is read in turn, as the shell tool did)."""
    o = Options()
    for word in argv:
        if word in ("-h", "--help"):
            return None
        if word == "--measure":
            o.measure = True
        elif word == "--tsv":
            o.tsv = True
        elif word.startswith("-"):
            raise Refused(f"ratchet-diff: unknown argument {word}")
        elif o.range:
            raise Refused("ratchet-diff: one range")
        else:
            o.range = word
    if not o.range:
        raise Refused("ratchet-diff: <base>..<head> (or <base>) is required")
    return o


def rev(ref: str) -> str:
    return subprocess.run(["git", "rev-parse", ref], stdout=subprocess.PIPE, text=True).stdout.strip()


def guard_checkout(head: str) -> None:
    """The measure reads this script's checkout, the refs come from the current one: they must be the same checkout (a
    worktree calling the root checkout's copy measured main's tree and said "No ratchet row changed")."""
    if subprocess.run([os.path.join(ROOT, "scripts", "own-checkout"), os.path.abspath(__file__)]).returncode != 0:
        raise Refused()
    if head and rev(head) != rev("HEAD"):
        raise Refused("ratchet-diff: --measure measures this working tree: give <base> or <base>..HEAD")


def measured_rows(work: str) -> List[str]:
    """Each measure run over this tree, its output in a log; the table's row flags, or Refused naming one that wrote
    none (its log's tail first)."""
    rows: List[str] = []
    for name, tool, _ in MEASURES:
        command = [os.path.join(ROOT, "scripts", tool), "--out", os.path.join(work, name)]
        if name == "js":
            command[1:1] = ["--areas", os.path.join(ROOT, "scripts", "js-quality", "ratchet-areas.tsv")]
        with open(os.path.join(work, name + ".log"), "w") as log:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT)
    for name, _, rows_file in MEASURES:
        path = os.path.join(work, name, rows_file)
        if not os.path.isfile(path) or os.path.getsize(path) == 0:
            with open(os.path.join(work, name + ".log")) as log:
                sys.stderr.write("".join(log.readlines()[-20:]))
            raise Refused(f"ratchet-diff: the {name} measure wrote no rows")
        rows += [ROW_FLAGS[name], path]
    return rows


def table(args: List[str], o: Options) -> int:
    command = [sys.executable, os.path.join(ROOT, "scripts", "ci", "ratchet_diff.py"), *args]
    return subprocess.run(command + (["--tsv"] if o.tsv else [])).returncode


def ended(signum: int, _frame: object) -> NoReturn:
    sys.exit(128 + signum)  # through the finally that removes the measures' directory


def run(argv: List[str]) -> int:
    o = read(argv)
    if o is None:
        print("Purpose: " + (__doc__ or ""), end="")
        return 0
    base, dots, head = o.range.partition("..")
    args = ["--base", base]
    if not o.measure:
        return table(args + (["--head", head] if dots and head else []), o)
    guard_checkout(head)
    signal.signal(signal.SIGTERM, ended)
    work = tempfile.mkdtemp()
    try:
        return table(args + measured_rows(work), o)
    finally:
        shutil.rmtree(work, ignore_errors=True)


def main() -> int:
    try:
        return run(sys.argv[1:])
    except Refused as e:
        if str(e):
            print(e, file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
