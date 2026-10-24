#!/usr/bin/env python3
"""Setting tools.costs.PerformanceBudgets' `measured` figures from what a CI run measured: the logic behind
scripts/adopt-ci-budgets (its header has the usage); tests: scripts/adopt_ci_budgets_test.py.

Every budget check appends `name<TAB>actual<TAB>measured<TAB>floor<TAB>limit` to a `performance-budgets.tsv`
(tools.costs.PerformanceBudget.Report), and each ci.yml row that runs budget specs uploads its file as a
`performance-budgets-<row>` artifact. An allocation ratchet (a row with a floor) takes the MEDIAN of the run's figures
for its name, written in the unit its `measured` already uses (`mb(...)`, `kb(...)`) at the precision the file uses.
A ceiling or an operation count (no floor) is not a measurement and is never touched.

A figure outside its budget's band is a red check, which a regression is too: it is listed and left as it is unless
--out-of-band says the move is a deliberate trade.
"""
from __future__ import annotations

import argparse
import glob
import math
import os
import re
import statistics
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
SOURCE = os.path.join(ROOT, "testkit/src/main/scala/tools/costs/PerformanceBudgets.scala")
ARTIFACTS = "performance-budgets-*"
MB = 1024 * 1024
KB = 1024


class Figure:
    """One budget's figures in the run: what it measured each time, and what it was held to."""

    def __init__(self, name: str, measured: int, floor: int, limit: int) -> None:
        self.name, self.measured, self.floor, self.limit = name, measured, floor, limit
        self.actuals: list[int] = []

    @property
    def median(self) -> int:
        return int(round(statistics.median(self.actuals)))

    @property
    def in_band(self) -> bool:
        return self.floor <= self.median <= self.limit


def read_figures(paths: list[str]) -> dict[str, Figure]:
    """Every ratchet's figures across the TSV files, by name. A malformed line is skipped, as the report writes best effort."""
    figures: dict[str, Figure] = {}
    for path in paths:
        with open(path, encoding="utf-8") as f:
            for line in f:
                cells = line.rstrip("\n").split("\t")
                if len(cells) != 5:
                    continue
                name = cells[0]
                try:
                    actual, measured, floor, limit = (int(c) for c in cells[1:])
                except ValueError:
                    continue
                if floor <= 0:
                    continue
                figures.setdefault(name, Figure(name, measured, floor, limit)).actuals.append(actual)
    return figures


def tsv_files(where: str) -> list[str]:
    if os.path.isfile(where):
        return [where]
    return sorted(glob.glob(os.path.join(where, "**", "performance-budgets.tsv"), recursive=True))


def download(run: str, repo: str | None, into: str) -> None:
    """The run's budget artifacts into `into`, through scripts/forge-gh (GitHub's gh, or Forgejo's artifact registry)."""
    cmd = [os.environ.get("KINOWO_GH", os.path.join(HERE, "forge-gh")), "run", "download", run, "-p", ARTIFACTS, "-D", into]
    if repo:
        cmd += ["-R", repo]
    subprocess.run(cmd, check=True)


def written(value: int, unit: str) -> str:
    """`value` bytes as the file writes a figure: MB to two decimals under 10, one above; KB to one decimal under 100,
    whole above; a bare count of bytes as is."""
    if unit == "mb":
        v = value / MB
        return f"mb({v:.2f})" if v < 10 else f"mb({v:.1f})"
    if unit == "kb":
        v = value / KB
        return f"kb({v:.1f})" if v < 100 else f"kb({int(round(v))})"
    return str(value)


def as_bytes(expr: str) -> int:
    m = re.fullmatch(r"(mb|kb)\(([\d.]+)\)", expr)
    if not m:
        return int(expr.replace("L", "").replace("_", ""))
    factor = MB if m.group(1) == "mb" else KB
    # The file's own helpers truncate (`(n * 1024).toLong`).
    return int(math.floor(float(m.group(2)) * factor))


def budget_pattern(name: str) -> re.Pattern:
    return re.compile(r'(bytes\(\s*"' + re.escape(name) + r'"\s*,\s*measured\s*=\s*)((?:mb|kb)\([\d.]+\)|[\d_]+L?)')


def adopt(source: str, figures: dict[str, Figure], out_of_band: bool) -> tuple[str, list[str]]:
    """`source` with each measured figure set to its CI median, and one line per budget saying what happened."""
    report = []
    for name in sorted(figures):
        fig = figures[name]
        m = budget_pattern(name).search(source)
        if not m:
            report.append(f"NOT IN THE FILE  {name}: measured {fig.median} bytes in the run, no `bytes(\"{name}\", measured = ...)`")
            continue
        old = m.group(2)
        unit = old[:2] if old[:2] in ("mb", "kb") else ""
        new = written(fig.median, unit)
        change = f"{old} -> {new}" if new != old else f"{old} (unchanged)"
        moved = (fig.median - as_bytes(old)) / as_bytes(old) * 100 if as_bytes(old) else 0.0
        figs = ", ".join(written(a, unit) for a in sorted(fig.actuals))
        if not fig.in_band and not out_of_band:
            report.append(f"OUT OF BAND      {name}: {change} ({moved:+.1f}%; run figures {figs}): left as it is; "
                          "a regression is fixed, a deliberate trade is adopted with --out-of-band")
            continue
        source = source[:m.start(2)] + new + source[m.end(2):]
        tag = "ADOPTED         " if new != old else "SAME            "
        report.append(f"{tag} {name}: {change} ({moved:+.1f}%; run figures {figs})")
    for name in re.findall(r'bytes\(\s*"([^"]+)"\s*,\s*measured', source):
        if name not in figures:
            report.append(f"NOT MEASURED     {name}: no figure in the run (its spec's row skipped, or failed before it)")
    return source, report


def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(prog="adopt-ci-budgets", add_help=False)
    p.add_argument("run")
    p.add_argument("--repo")
    p.add_argument("--file", default=SOURCE)
    p.add_argument("--out-of-band", action="store_true")
    p.add_argument("--dry-run", action="store_true")
    args = p.parse_args(argv)

    with tempfile.TemporaryDirectory() as tmp:
        if os.path.exists(args.run):
            paths = tsv_files(args.run)
        else:
            if not re.fullmatch(r"\d+", args.run):
                print(f"adopt-ci-budgets: {args.run} is neither a run id nor a file or directory", file=sys.stderr)
                return 2
            download(args.run, args.repo, tmp)
            paths = tsv_files(tmp)
        figures = read_figures(paths)
        if not figures:
            print(f"adopt-ci-budgets: no allocation figure in {args.run} ({len(paths)} file(s))", file=sys.stderr)
            return 1

    with open(args.file, encoding="utf-8") as f:
        source = f.read()
    adopted, report = adopt(source, figures, args.out_of_band)
    print("\n".join(report))
    if adopted != source and not args.dry_run:
        with open(args.file, "w", encoding="utf-8") as f:
            f.write(adopted)
        print(f"\nwrote {os.path.relpath(args.file, ROOT)}: say in the commit which run the figures are from, and each "
              "move; the dated notes beside the budgets are yours to update")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
