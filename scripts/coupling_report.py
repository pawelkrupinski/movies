#!/usr/bin/env python3
"""The coupling report (CLAUDE.md, Design rules): the production code's coupling, from Zinc's class-dependency record.

THE COUPLING REPORT (CLAUDE.md, Design rules): the production code's coupling, from Zinc's class-dependency record
and, between the classes of one source file, their class files (`sbt couplingReport`, project/Coupling.scala), through
this checkout's kept sbt server (scripts/sbt-warm).

  scripts/coupling-report             the markdown report (module matrix, packages' Ca/Ce/I/A/D, package cycles with
                                      the class references behind every edge, top fan-in/fan-out classes, god hubs,
                                      split packages) to stdout, then the ratchet's verdict against the baseline
  scripts/coupling-report --update    the same, then the measured rows written to test/resources/coupling-baseline.tsv:
                                      every fall recorded; a rise must be committed with `Coupling: <why>`; each
                                      frozen distance row's ceiling kept (`distance-cap`, only falling; over a
                                      conflicted baseline the least of every side's); and
                                      each frozen class's reference set to test/resources/coupling-fanout-refs.tsv
                                      (what a later `Coupling: split of <X>` claim is held to) and every top-level
                                      type's verdict to test/resources/coupling-collaborators.tsv (a type that was a
                                      collaborator and is data needs the `Coupling:` trailer)
  scripts/coupling-report --out DIR   keep rows.tsv, report.md, edges.tsv (every package edge's class references),
                                      fanouts.tsv (every class's held fan-out), collaborators.tsv (every type's
                                      verdict: data, or collaborator and why -- only a collaborator counts) and
                                      logic.tsv (every type's non-trivial methods and decisions: the data-logic cut's
                                      distribution) in DIR
                                      (default: a temporary directory; a relative DIR is under the checkout)
  scripts/coupling-report --check-split <X> <piece>... [--base REV]
                                      whether the ratchet would accept the WORKING TREE's split of frozen class X
                                      into those pieces under the current rule (each piece below X's row, their
                                      references' union within X's at REV -- default the merge base with
                                      origin/main -- and the distance rows it moves, each with its ceiling and the
                                      headroom left under it), and the `Coupling: split of`
                                      trailer to use: scripts/ci/coupling_ratchet.py split-check, the ratchet's own
                                      code. Exit 0 accepted, 1 refused.
  scripts/coupling-report --simulate <moves.tsv>
                                      what moving classes would do, with no code changed (project/CouplingSimulation.scala):
                                      the graph with each `<class or pattern><TAB><package>` row's classes moved, each
                                      `<class><TAB>into <class>` row's folded into that class (its fan-out the union of
                                      both), each `layer`/`rank`/`allow` row added to the layer table (a `rank` row
                                      replacing its package's) -- each changed package's types, A, Ca, Ce, I and D,
                                      the baseline rows that change (cycle edges, layer and rank violations, Ce,
                                      distance, fan-out), the package cycles after, every affected class's fan-out
  scripts/coupling-report --exemption-slack
                                      every `class` exemption row's collaborators, read from its class files' methods
                                      (project/ExemptionSlack.scala): each pass-through (only forwarded into a
                                      constructor), builder (used only while built), co-used group (the same methods
                                      use them all), reached-through one (used only beside another that hands it
                                      out), and the suggested cut with the fan-out it leaves; a class within the limit
                                      after it is a stale exemption (coupling-exemptions.yml reports them weekly)
  scripts/coupling-report --new-exemptions [--since 7d]
                                      the exemption rows added on origin/main since then (default 7 days), each with
                                      its date, commit, reason and `Coupling: exempt` trailer, and whether it is still
                                      there: the owner's weekly list to strike from (no sbt run)

The pre-push hook's `coupling` check runs the same task and scripts/ci/coupling_ratchet.py over the pushed range.
It measures its own checkout: run from inside another one, it refuses (scripts/own-checkout). Exit: the ratchet's
(0 within the baseline, 1 past it; --update 0), 2 a usage error or a failed sbt task."""
from __future__ import annotations

import os
import sys
import tempfile
from dataclasses import dataclass, field
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from processes import machine  # noqa: E402
import tool_process  # noqa: E402

TOOL = "coupling-report"
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RATCHET = os.path.join(ROOT, "scripts", "ci", "coupling_ratchet.py")
REPORT_FILES = ("rows.tsv", "report.md", "edges.tsv", "fanouts.tsv", "collaborators.tsv", "logic.tsv")


class Usage(Exception):
    """A command line it does not take: its message said, exit 2."""


@dataclass
class Options:
    update: bool = False
    out: str = ""
    base: str = ""
    new_exemptions: bool = False
    since: str = "7d"
    simulate: str = ""
    slack: bool = False
    split: str = ""
    pieces: List[str] = field(default_factory=lambda: [])


def value(argv: List[str], i: int) -> str:
    if i + 1 >= len(argv):
        raise Usage(f"{argv[i]} needs a value")
    return argv[i + 1]


def read_split(o: Options, argv: List[str], i: int) -> int:
    """--check-split <X> <piece>...: the index of its last piece."""
    if len(argv) - i < 3:
        raise Usage("--check-split <X> <piece>...")
    o.split = argv[i + 1]
    i += 2
    while i < len(argv) and not argv[i].startswith("--"):
        o.pieces.append(argv[i])
        i += 1
    if not o.pieces:
        raise Usage("--check-split <X> needs at least one piece")
    return i - 1


def read_simulate(o: Options, argv: List[str], i: int) -> int:
    if i + 1 >= len(argv) or not os.path.isfile(argv[i + 1]):
        raise Usage("--simulate <moves.tsv> (an existing file)")
    o.simulate = os.path.abspath(argv[i + 1])
    return i + 1


FLAGS = {"--update": "update", "--new-exemptions": "new_exemptions", "--exemption-slack": "slack"}
VALUES = {"--out": "out", "--base": "base", "--since": "since"}


def options(argv: List[str]) -> Optional[Options]:
    """The options; None when --help asked for the header (printed)."""
    o = Options()
    i = 0
    while i < len(argv):
        word = argv[i]
        if word in ("-h", "--help"):
            print((__doc__ or "").split("\n\n", 1)[1])
            return None
        if word in FLAGS:
            setattr(o, FLAGS[word], True)
        elif word in VALUES:
            setattr(o, VALUES[word], value(argv, i))
            i += 1
        elif word == "--simulate":
            i = read_simulate(o, argv, i)
        elif word == "--check-split":
            i = read_split(o, argv, i)
        else:
            raise Usage(f"unknown argument {word}")
        i += 1
    return o


class CouplingReport:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner, out: str) -> None:
        self.o = o
        self.env = env
        self.runner = runner
        self.out = out

    def ratchet(self, *args: str) -> int:
        sys.stdout.flush()
        return self.runner.run(tool_process.Command(["python3", RATCHET, *args], self.env, cwd=ROOT))

    def sbt(self, task: str, written: str) -> bool:
        """The task in the checkout's kept sbt server, its output in <out>/sbt.log; False (its tail and why said) when
        it failed or left `written` empty."""
        log = os.path.join(self.out, "sbt.log")
        code = self.runner.run(tool_process.Command([os.path.join(ROOT, "scripts", "sbt-warm"), task], self.env,
                                                    log=log, cwd=ROOT))
        if code == 0 and os.path.isfile(written) and os.path.getsize(written) > 0:
            return True
        with open(log, errors="replace") as f:
            sys.stderr.write("".join(f.readlines()[-30:]))
        print(f"{TOOL}: sbt {task.split(' ')[0]} failed (log: {log})", file=sys.stderr)
        return False

    def task_markdown(self) -> int:
        """A simulation or the exemption slack: its own task over the same graph, its markdown to stdout."""
        if self.o.simulate:
            md = os.path.join(self.out, "simulation.md")
            task = f"couplingSimulate {self.o.simulate} {md}"
        else:
            md = os.path.join(self.out, "slack.md")
            task = f"couplingExemptionSlack {md}"
        if not self.sbt(task, md):
            return 2
        with open(md, errors="replace") as f:
            sys.stdout.write(f.read())
        return 0

    def report(self) -> int:
        files = [os.path.join(self.out, name) for name in REPORT_FILES]
        rows, report, edges, fanouts = files[0], files[1], files[2], files[3]
        if not self.sbt("couplingReport " + " ".join(files), rows):
            return 2
        if self.o.split:
            into = [word for piece in self.o.pieces for word in ("--into", piece)]
            base = ["--base", self.o.base] if self.o.base else []
            return self.ratchet("split-check", "--current", rows, "--fanouts", fanouts, "--split", self.o.split,
                                *into, *base)
        with open(report, errors="replace") as f:
            sys.stdout.write(f.read() + "\n")
        collaborators = files[4]
        code = self.ratchet("check", "--current", rows, "--edges", edges, "--fanouts", fanouts, "--collaborators", collaborators)
        return self.update(rows, fanouts, collaborators) if self.o.update else code

    def update(self, rows: str, fanouts: str, collaborators: str) -> int:
        code = self.ratchet("update", "--current", rows, "--baseline", "test/resources/coupling-baseline.tsv",
                            "--fanouts", fanouts, "--collaborators", collaborators)
        if code != 0:
            return code
        refs = self.runner.capture(tool_process.Command(
            ["python3", RATCHET, "refs", "--current", rows, "--fanouts", fanouts], self.env, cwd=ROOT))
        sys.stderr.write(refs.stderr)
        with open(os.path.join(ROOT, "test", "resources", "coupling-fanout-refs.tsv"), "w") as f:
            f.write(refs.stdout)
        if refs.code != 0:
            return refs.code
        print(f"{TOOL}: wrote test/resources/coupling-baseline.tsv, coupling-fanout-refs.tsv and coupling-collaborators.tsv -- a rise is committed "
              "with a `Coupling: <why>` trailer")
        return 0

    def main(self) -> int:
        os.makedirs(self.out, exist_ok=True)
        if self.o.simulate or self.o.slack:
            return self.task_markdown()
        return self.report()


def main() -> int:
    runner = tool_process.SubprocessRunner()
    env = dict(machine.environment())
    own = runner.run(tool_process.Command([os.path.join(ROOT, "scripts", "own-checkout"),
                                           os.path.join(ROOT, "scripts", TOOL)], env))
    if own != 0:
        return 2
    try:
        o = options(sys.argv[1:])
    except Usage as e:
        print(f"{TOOL}: {e}", file=sys.stderr)
        return 2
    if o is None:
        return 0
    if o.new_exemptions:
        sys.stdout.flush()
        os.chdir(ROOT)
        os.execvp("python3", ["python3", RATCHET, "new-exemptions", "--since", o.since])
    if o.out:
        return CouplingReport(o, env, runner, os.path.join(ROOT, o.out)).main()
    with tempfile.TemporaryDirectory(prefix=f"{TOOL}.", dir=env.get("TMPDIR") or None) as out:
        return CouplingReport(o, env, runner, out).main()


if __name__ == "__main__":
    sys.exit(main())
