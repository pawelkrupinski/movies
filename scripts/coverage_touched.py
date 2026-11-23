#!/usr/bin/env python3
"""Coverage of the methods a change touches (scoverage; a report, never a gate)
COVERAGE OF THE METHODS A CHANGE TOUCHES (scoverage; a report, never a gate): which touched production methods the
specs that reach them leave unrun, so the pinning tests are written FIRST -- before a refactor moves what they would
have to pin.

  scripts/coverage-touched <base>..<head> [--below <percent>] [--out DIR]

Over a throwaway worktree at <head> (scripts/lib/touched_tree.py; the caller's checkout and its build are never
touched): the touched methods (a hunk's line inside a `def`: scripts/touched_methods.py over quality.MethodSpans), the
specs that reach them (`touchedSpecs`: a spec of the method's source, or of a production caller using its name), then
ONLY the touched methods' modules compiled with coverage on (`set <module>/coverageEnabled := true`: the Scala 3
compiler's own instrumentation, sbt-scoverage reading it), only those specs run (`<module>/testOnly`, in whichever
module they are), and each instrumented module's report read: every touched method's statements and how many ran,
those under --below (default 80%) first. A spec that fails still counts what it ran (said in the report).
Instrumentation is never on in the normal build: the plugin's coverageEnabled is false by default, and nothing here
sets it outside the worktree.

  --below N   the statement coverage under which a method is listed first (default 80)
  --out DIR   keep touched.tsv, specs.txt, the sbt logs, the XML reports and report.md in DIR (default: a temporary
              directory, removed)

Exit 0 with the report (also when nothing is touched); 1 when a step failed; 2 a usage error. The heavy step runs under
one governor ticket (scripts/gov-run --job coverage-touched).
"""
from __future__ import annotations

import os
import shutil
import sys
from typing import List, Optional, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import tool_cli  # noqa: E402
import touched_tree  # noqa: E402
from touched_tree import CommandLine, TouchedTree  # noqa: E402

TOOL = "coverage-touched"


def measure(tree: TouchedTree, line: CommandLine) -> int:
    below = line.flags["--below"]
    if not tree.methods():
        print(f"{TOOL}: the range touches no production method")
        return 0
    tree.specs()
    modules = tree.modules()
    enable = [f"set {m}/coverageEnabled := true" for m in modules]
    specs = tree.reaching_specs()
    runs = test_runs(specs)
    print(f"{TOOL}: {len(tree.touched_rows())} touched method(s) in {' '.join(modules)} - {len(specs)} spec(s) reach them",
          flush=True)
    tests_rc = tree.governed(TOOL, 4, 4, tree.at("tests.log"), enable + runs) if runs else 0
    log = tree.at("report.log")
    if tree.sbt(log, enable + [f"{m}/coverageReport" for m in modules]) != 0:
        tree.fail_with_log(log, 20, f"coverageReport failed (log: {log})")
    report = tree.touched_methods("the coverage report failed", "coverage", "--touched", tree.at("touched.tsv"),
                                  "--below", below, "--root", tree.tree, *kept_reports(tree, modules))
    return tree.finish(report, notes(tree, runs, tests_rc))


def test_runs(specs: Sequence[Tuple[str, str]]) -> List[str]:
    """`<module>/testOnly <spec>...` for each module the reaching specs are in."""
    return [f"{module}/testOnly " + " ".join(spec for m, spec in specs if m == module)
            for module in sorted({m for m, _ in specs})]


def kept_reports(tree: TouchedTree, modules: Sequence[str]) -> List[str]:
    """`--xml <report>` for each instrumented module's scoverage.xml, each also kept as out/scoverage-<module>.xml."""
    found: List[str] = []
    for module in modules:
        report = scoverage_xml(os.path.join(tree.tree, module, "target"))
        if report is not None:
            found += ["--xml", report]
            shutil.copy(report, tree.at(f"scoverage-{module}.xml"))
    return found


def scoverage_xml(target: str) -> Optional[str]:
    for directory, _, files in sorted(os.walk(target)):
        if os.path.basename(directory) == "scoverage-report" and "scoverage.xml" in files:
            return os.path.join(directory, "scoverage.xml")
    return None


def notes(tree: TouchedTree, runs: Sequence[str], tests_rc: int) -> List[str]:
    said: List[str] = []
    if not runs:
        said.append("No spec reaches the touched methods: every one is unpinned.")
    if tests_rc != 0:
        said.append(f"Some reaching specs FAILED (exit {tests_rc}; log: {tree.at('tests.log')}): what they ran still "
                    "counts.")
    return said


def checked(line: CommandLine) -> CommandLine:
    if not touched_tree.is_number(line.flags["--below"], r"[0-9.]+"):
        raise tool_cli.UsageError(f"--below wants a percentage, not '{line.flags['--below']}'")
    return line


def main(argv: List[str]) -> int:
    return touched_tree.main(TOOL, argv, __doc__ or "", {"--below": "80"},
                             measure, check=checked)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
