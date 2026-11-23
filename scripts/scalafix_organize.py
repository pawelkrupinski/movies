#!/usr/bin/env python3
"""Organise imports with scalafix (.scalafix.conf: OrganizeImports, Scala 3 dialect), never by hand.

IMPORTS ORGANISED BY SCALAFIX, never by hand (.scalafix.conf: OrganizeImports, Scala 3 dialect -- each source's imports
sorted and grouped java/javax, scala, everything else -- and RemoveUnused): over every module's sources, rewritten
from the compiler's own record (SemanticDB), in the kept scalafix tree (scripts/lib/scalafix_tree.py: the caller's
checkout never recompiles with SemanticDB on), the result laid back over the checkout and its index.

  scripts/scalafix-organize [--check] [--main]

  --check   change nothing in the checkout: list the sources whose imports scalafix would change (exit 1 when there
            are any)
  --main    the production sources only (every module's Compile configuration), not the specs and fixtures

Run it after a move or a merge that left imports out of order, instead of editing them by hand; commit what it
changed on its own (`git diff --cached --stat` shows it), with the layers the sources touch green. RemoveUnused
finds nothing while the build compiles with `-Wunused:all -Werror`: an unused import already fails the compile.
Exit 0 done (or nothing to change); 1 --check found changes, or a step failed (its log named); 2 a usage error."""
from __future__ import annotations

import os
import sys
import time
from dataclasses import dataclass
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import scalafix_tree  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402

TOOL = "scalafix-organize"


@dataclass
class Options:
    check: bool = False
    main: bool = False


def options(argv: List[str]) -> Optional[Options]:
    """The options; None when --help asked for the header (printed)."""
    o = Options()
    for word in argv:
        if word in ("-h", "--help"):
            print((__doc__ or "").split("\n\n", 1)[1])
            return None
        if word not in ("--check", "--main"):
            raise tool_cli.UsageError(f"unknown argument {word}")
        o.check = o.check or word == "--check"
        o.main = o.main or word == "--main"
    return o


def check(tree: scalafix_tree.ScalafixTree, started: int) -> int:
    """What scalafix changed in the tree, listed and dropped: 1 when anything."""
    status = tree.git_tree.out("status", "--porcelain") or ""
    files = [line[3:] for line in status.splitlines()]
    tree.discard()
    if not files:
        print(f"{TOOL}: every source's imports are in order ({int(time.time()) - started} s)")
        return 0
    print("\n".join(files))
    print(f"{TOOL}: {len(files)} source(s) would change ({int(time.time()) - started} s)")
    return 1


def organize(o: Options, tree: scalafix_tree.ScalafixTree) -> int:
    started = int(time.time())
    commands = [c for c in scalafix_tree.commands("") if not o.main or "/Compile/" in c]
    log = tree.path("scalafix.log")
    if tree.sbt(log, commands) != 0:
        sys.stderr.write("".join(scalafix_tree.log_lines(log)[-30:]))
        raise scalafix_tree.Failed("scalafix failed (log above)")
    if o.check:
        return check(tree, started)
    if not tree.apply():
        print(f"{TOOL}: every source's imports are in order ({int(time.time()) - started} s)")
        return 0
    print(tree.staged_stat())
    print(f"{TOOL}: laid over the checkout and its index ({int(time.time()) - started} s)")
    return 0


def main() -> int:
    try:
        o = options(sys.argv[1:])
    except tool_cli.UsageError as e:
        return tool_cli.usage_failed(TOOL, e)
    if o is None:
        return 0
    try:
        tree = scalafix_tree.ScalafixTree.opened(TOOL, tool_process.SubprocessRunner(), os.environ)
        try:
            return organize(o, tree)
        finally:
            tree.close()
    except scalafix_tree.Failed as e:
        sys.stdout.flush()
        print(f"{TOOL}: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
