#!/usr/bin/env python3
"""The class cycles of one package (CLAUDE.md, Design rules; project/ClassCycles.scala, `sbt couplingClassCycles`)

THE CLASS CYCLES OF ONE PACKAGE: which of a package's classes cannot be pulled apart before it is split -- from the
coupling report's own graph (Zinc's record and the class files), through this checkout's kept sbt server
(scripts/sbt-warm).

  scripts/class-cycles <package>                  the strongly connected components of 2+ classes among the classes of
                                                  the package and its sub-packages, largest first
  scripts/class-cycles <package> --order a<b<c    and, for that order of groups bottom up (each a comma-separated list of
                                                  class names, full or relative to the package, `*` any run of
                                                  characters: `Listing,Evidence<*Measures<*`), every class edge
                                                  pointing up it, by group pair
  scripts/class-cycles <package> --order FILE     the order as a `<class><TAB><group>` table (groups bottom up by their
                                                  number when all are numbers, else by their first row)
  --out FILE                                      also keep the markdown in FILE

Quote an inline order: `<` is the shell's redirection. Exit 0 with the report on stdout, 2 on a usage or sbt error."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from typing import List, Optional

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
USAGE = "usage: scripts/class-cycles <package> [--order 'a<b<c' | --order FILE] [--out FILE]"
LOG_TAIL = 30
HELP = (__doc__ or "").split("\n", 1)[1].strip("\n")


class Usage(Exception):
    """A command line it does not take: its message said (with the usage line), exit 2."""


@dataclass
class Options:
    package: str = ""
    order: str = ""
    out: str = ""


def flag_value(argv: List[str], i: int) -> str:
    if i + 1 >= len(argv):
        raise Usage(USAGE)
    return argv[i + 1]


def read_word(o: Options, argv: List[str], i: int) -> int:
    """`o` with the word at argv[i] read into it; the index of the last word it took."""
    word = argv[i]
    if word == "--order":
        o.order = flag_value(argv, i)
        return i + 1
    if word == "--out":
        value = flag_value(argv, i)
        o.out = value if os.path.isabs(value) else os.path.join(os.getcwd(), value)
        return i + 1
    if word.startswith("-"):
        raise Usage(f"class-cycles: unknown argument {word}\n{USAGE}")
    if o.package:
        raise Usage(USAGE)
    o.package = word
    return i


def read_options(argv: List[str]) -> Optional[Options]:
    """The options, None when --help asked for the header (printed)."""
    if any(word in ("-h", "--help") for word in argv):
        print(HELP)
        return None
    o = Options()
    i = 0
    while i < len(argv):
        i = read_word(o, argv, i) + 1
    if not o.package:
        raise Usage(USAGE)
    if " " in o.order:
        raise Usage("class-cycles: an order has no spaces (groups split by <, classes by ,)")
    if o.order and os.path.isfile(o.order):
        o.order = os.path.abspath(o.order)  # the sbt server runs in the checkout's root
    return o


def sbt_failure(log: str, why: str) -> int:
    print("\n".join(log.splitlines()[-LOG_TAIL:]), file=sys.stderr)
    print(f"class-cycles: {why}", file=sys.stderr)
    return 2


def report(o: Options) -> int:
    with tempfile.TemporaryDirectory(prefix="class-cycles-") as tmp:
        cycles = os.path.join(tmp, "cycles.md")
        task = f"couplingClassCycles {o.package} {cycles}" + (f" {o.order}" if o.order else "")
        done = subprocess.run([os.path.join(ROOT, "scripts", "sbt-warm"), task], cwd=ROOT, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if done.returncode != 0:
            return sbt_failure(done.stdout, "sbt couplingClassCycles failed")
        if not os.path.exists(cycles) or os.path.getsize(cycles) == 0:
            return sbt_failure(done.stdout, "sbt couplingClassCycles wrote no report")
        if o.out:
            shutil.copyfile(cycles, o.out)  # it measures its own checkout: the copy is made while the file exists
        with open(cycles) as f:
            sys.stdout.write(f.read())
    return 0


def main(argv: List[str]) -> int:
    own = subprocess.run([os.path.join(ROOT, "scripts", "own-checkout"), os.path.join(ROOT, "scripts", "class-cycles")])
    if own.returncode != 0:
        return 2  # it measures its own checkout: never another one's
    try:
        o = read_options(argv)
    except Usage as e:
        print(e, file=sys.stderr)
        return 2
    return 0 if o is None else report(o)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
