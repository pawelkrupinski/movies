#!/usr/bin/env python3
"""Mutation testing of the methods a change touches (Stryker4s; a report, never a gate)
MUTATION TESTING OF THE METHODS A CHANGE TOUCHES (Stryker4s; a report, never a gate): whether the specs that reach
the touched production methods would catch a change of behaviour in them. Run it on the range a refactor is about to
rewrite, BEFORE the refactor: every surviving mutant is a behaviour the pinning tests do not pin (2026-11-20: a
"behaviour-preserving" scraper lane widened two venues' genre parsing and every test stayed green).

  scripts/mutation-check <base>..<head> [--out DIR] [--max-specs N] [--concurrency N]

Over a throwaway worktree at <head> (scripts/lib/code_tools/touched_tree.py; the caller's checkout and its build are never
touched): the touched methods (a hunk's line inside a `def`: scripts/touched_methods.py over quality.MethodSpans) and
the specs that reach them (`touchedSpecs`: a spec of the method's source, or of a production caller using its name);
then, a module at a time, Stryker4s mutates ONLY the sources holding a touched method (`strykerMutate`) and runs ONLY
that module's reaching specs (`strykerTestFilter`; Stryker's own coverage pass then runs, per mutant, only the specs
that executed it). The report counts each touched method's mutants -- killed, survived, not covered -- and names every
survivor (line, mutator, replacement); mutants of the same files outside the touched methods are counted apart, not
reported. A spec of ANOTHER module (a worker spec over a common class) cannot kill a mutant: Stryker runs a module's
own specs. The mutated compile drops -Werror (a mutant may warn) and never reaches the normal build. Stryker's base
directory is the repository root, which its test JVMs run in: specs read `test/resources/...` by a root-relative path
(project/ForkedTests.scala).

  --out DIR         keep touched.tsv, specs.txt, the sbt logs, every Stryker report.json and report.md in DIR
                    (default: a temporary directory, removed)
  --max-specs N     refuse a module whose reaching specs number more than N (default 60): scope the range tighter, or
                    raise it knowingly -- each spec runs once whole, then per mutant it covers
  --concurrency N   Stryker's test runners (forked JVMs; default 2)

Exit 0 with the report (also when nothing is touched); 1 when a step failed; 2 a usage error. Each module's run holds
one governor ticket (scripts/gov-run --job mutation-check).
"""
from __future__ import annotations

import os
import shutil
import sys
from typing import List, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from cli import tool_cli  # noqa: E402
from code_tools import touched_tree  # noqa: E402
from code_tools.touched_tree import CommandLine, Failed, TouchedTree, quoted  # noqa: E402

TOOL = "mutation-check"


def measure(tree: TouchedTree, line: CommandLine) -> int:
    if not tree.methods():
        print(f"{TOOL}: the range touches no production method")
        return 0
    tree.specs()
    reports: List[str] = []
    notes: List[str] = []
    for module in tree.modules():
        report = mutated(tree, line, module, notes)
        if report is not None:
            reports += ["--report", report]
    text = ""
    if reports:
        text = tree.touched_methods("the mutation report failed", "mutation", "--touched", tree.at("touched.tsv"),
                                    "--root", tree.tree, *reports)
    return tree.finish(text, notes)


def mutated(tree: TouchedTree, line: CommandLine, module: str, notes: List[str]) -> Optional[str]:
    """Stryker over `module`'s touched sources and reaching specs: its kept report.json; None (a note said why) when
    no spec of the module reaches them."""
    files = sorted({row[0] for row in tree.touched_rows() if row[0].startswith(f"{module}/")})
    specs = [spec for m, spec in tree.reaching_specs() if m == module]
    if not specs:
        notes.append(f"{module}: no spec of {module} reaches the touched methods -- none can kill a mutant there; "
                     "write the pinning tests first")
        return None
    most = int(line.flags["--max-specs"])
    if len(specs) > most:
        raise Failed(f"{module}: {len(specs)} specs reach the touched methods, over --max-specs {most} "
                     f"(list: {tree.at('specs.txt')})")
    print(f"{TOOL}: {module} -- {len(files)} source(s) mutated, {len(specs)} spec(s) run", flush=True)
    log = tree.at(f"stryker-{module}.log")
    reports = os.path.join(tree.tree, "target", "stryker4s-report")
    shutil.rmtree(reports, ignore_errors=True)
    rc = tree.governed(TOOL, 4, 6, log, settings(module, files, specs, line.flags["--concurrency"]))
    report = newest_report(reports)
    if rc != 0 or report is None:
        tree.fail_with_log(log, 30, f"{module}: Stryker failed (exit {rc}; log: {log})")
    kept = tree.at(f"report-{module}.json")
    shutil.copy(report, kept)
    return kept


def settings(module: str, files: Sequence[str], specs: Sequence[str], concurrency: str) -> List[str]:
    return [f"set {module}/strykerBaseDir := (LocalRootProject / baseDirectory).value",
            f"set {module}/strykerMutate := Seq({quoted(files)})",
            f"set {module}/strykerTestFilter := Seq({quoted(specs)})",
            f'set {module}/strykerReporters := Seq("json", "console")',
            f"set {module}/strykerConcurrency := {concurrency}",
            f"set {module}/strykerOpenReport := false",
            f'set {module}/scalacOptions ~= (_.filterNot(_ == "-Werror"))',
            f"{module}/stryker"]


def newest_report(reports: str) -> Optional[str]:
    found = sorted(os.path.join(directory, "report.json") for directory, _, files in os.walk(reports)
                   if "report.json" in files)
    return found[-1] if found else None


def checked(line: CommandLine) -> CommandLine:
    if not (touched_tree.is_number(line.flags["--max-specs"]) and touched_tree.is_number(line.flags["--concurrency"])):
        raise tool_cli.UsageError("--max-specs and --concurrency want whole numbers")
    return line


def main(argv: List[str]) -> int:
    return touched_tree.main(TOOL, argv, __doc__ or "", {"--max-specs": "60", "--concurrency": "2"}, measure,
                             check=checked)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
