#!/usr/bin/env python3
# THE HOTSPOT REPORT (scripts/hotspots; a report, not a gate -- CLAUDE.md, "Code quality ratchets"): where the
# production code is both complex and changing, from git history and the quality measure's per-source numbers.
#
#   hotspots.py --files <files.tsv> [--since "90 days ago"] [--top 15] [--pairs 10] [--min-shared 5]
#               [--max-commit-files 40]
#
# <files.tsv> is quality.QualityReport's `--files` output: `path<TAB>lines<TAB>cognitive` a production source.
#
#   HOTSPOTS  each production source still present, ranked by RELATIVE CHURN x COGNITIVE COMPLEXITY: its lines added and
#             deleted since --since over its lines today (Nagappan & Ball's relative churn), times its methods' summed
#             cognitive complexity.
#   CHANGE COUPLING  pairs of production sources in different modules or packages (directories) changed together in at
#             least --min-shared commits (both still present), with their co-change degree: shared commits over the mean of the two's
#             commits. A commit touching more than --max-commit-files production sources (a sweep, a rename) is left out.
#
# Prints a markdown report to stdout.
from __future__ import annotations

import argparse
import subprocess
import sys
from collections import Counter
from itertools import combinations
from typing import Dict, List, Tuple

PRODUCTION_MODULES = ("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")


def is_production(path: str) -> bool:
    parts = path.split("/")
    return len(parts) > 3 and parts[0] in PRODUCTION_MODULES and parts[1:3] == ["src", "main"] and path.endswith(".scala")


def read_files(path: str) -> Dict[str, Tuple[int, int]]:
    out = {}
    with open(path) as f:
        for line in f:
            if line.strip():
                name, lines, cognitive = line.rstrip("\n").split("\t")
                out[name] = (int(lines), int(cognitive))
    return out


def history(since: str) -> List[Dict[str, int]]:
    """Each non-merge commit since `since`: its production sources and the lines added + deleted in each."""
    log = subprocess.run(["git", "log", f"--since={since}", "--no-merges", "--no-renames", "--format=%x00", "--numstat",
                          "--", *[f"{m}/src/main" for m in PRODUCTION_MODULES]],
                         check=True, capture_output=True, text=True).stdout
    commits = []
    for entry in log.split("\x00"):
        changed: Dict[str, int] = {}
        for line in entry.splitlines():
            cols = line.split("\t")
            if len(cols) == 3 and is_production(cols[2]) and cols[0].isdigit() and cols[1].isdigit():
                changed[cols[2]] = changed.get(cols[2], 0) + int(cols[0]) + int(cols[1])
        if changed:
            commits.append(changed)
    return commits


def hotspots(files: Dict[str, Tuple[int, int]], commits: List[Dict[str, int]]) -> List[Tuple[str, float, int, int, int, float]]:
    """(path, score, churn, lines, cognitive, relative churn), highest score first."""
    churn: Counter = Counter()
    for changed in commits:
        churn.update(changed)
    rows = []
    for path, (lines, cognitive) in files.items():
        if churn[path] and lines:
            relative = churn[path] / lines
            rows.append((path, relative * cognitive, churn[path], lines, cognitive, relative))
    return sorted(rows, key=lambda r: (-r[1], r[0]))


def package_of(path: str) -> Tuple[str, str]:
    """A source's module and package directory."""
    return path.split("/")[0], path.rsplit("/", 1)[0]


def coupling(commits: List[Dict[str, int]], present, min_shared: int, max_files: int) -> List[Tuple[str, str, int, float]]:
    """(a, b, shared commits, degree %) for pairs of sources still `present`, across modules or packages, most shared
    first."""
    revisions: Counter = Counter()
    shared: Counter = Counter()
    for changed in commits:
        if len(changed) > max_files:
            continue
        revisions.update(changed.keys())
        for a, b in combinations(sorted(changed), 2):
            if package_of(a) != package_of(b) and a in present and b in present:
                shared[(a, b)] += 1
    rows = [(a, b, n, 100.0 * n / ((revisions[a] + revisions[b]) / 2)) for (a, b), n in shared.items() if n >= min_shared]
    return sorted(rows, key=lambda r: (-r[2], -r[3], r[0], r[1]))


def report(files, commits, since, top, pairs, min_shared, max_files) -> List[str]:
    out = [f"## Hotspots (production Scala, since {since}: {len(commits)} commits)", "",
           "Relative churn (lines added + deleted / lines today) x the source's summed cognitive complexity.", "",
           "| # | source | score | churn | lines | cognitive |", "|---|---|---|---|---|---|"]
    for n, (path, score, churn, lines, cognitive, _) in enumerate(hotspots(files, commits)[:top], 1):
        out.append(f"| {n} | `{path}` | {score:.0f} | {churn} | {lines} | {cognitive} |")
    out += ["", f"## Change coupling (across modules or packages, >= {min_shared} shared commits; commits of over"
            f" {max_files} sources left out)", "",
            "| # | source | source | shared commits | co-change |", "|---|---|---|---|---|"]
    for n, (a, b, count, degree) in enumerate(coupling(commits, files, min_shared, max_files)[:pairs], 1):
        out.append(f"| {n} | `{a}` | `{b}` | {count} | {degree:.0f}% |")
    return out


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--files", required=True)
    parser.add_argument("--since", default="90 days ago")
    parser.add_argument("--top", type=int, default=15)
    parser.add_argument("--pairs", type=int, default=10)
    parser.add_argument("--min-shared", type=int, default=5)
    parser.add_argument("--max-commit-files", type=int, default=40)
    args = parser.parse_args(argv)
    try:
        lines = report(read_files(args.files), history(args.since), args.since, args.top, args.pairs, args.min_shared,
                       args.max_commit_files)
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"hotspots: {e}", file=sys.stderr)
        return 2
    print("\n".join(lines))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
