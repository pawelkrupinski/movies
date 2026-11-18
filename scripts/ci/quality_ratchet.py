#!/usr/bin/env python3
# THE CODE-QUALITY RATCHETS (the pre-push sbt check's quality step; CLAUDE.md, "Code quality ratchets"): the production
# code's measures (`quality/runMain quality.QualityReport`, scripts/quality-report) may not grow past their checked-in
# baselines -- the coupling ratchet's model (scripts/ci/coupling_ratchet.py) over other measures. Each ratchet holds
# some row kinds to its own baseline, under its own trailer:
#
#   complexity   cognitive / cyclomatic / inner-defs of a method    test/resources/complexity-baseline.tsv   Complexity:
#   length       method-lines of a method, file-lines of a source   test/resources/length-baseline.tsv       Length:
#   duplication  copies of the clone groups over a set of files     test/resources/duplication-baseline.tsv  Duplication:
#
# A row is `kind<TAB>key<TAB>value`, `limit<TAB>kind<TAB>value` a kind's limit; `#` lines are comments. The measure
# writes a row for each key past its kind's limit. A key with a baseline row may not rise past it; a key without one
# fails (a NEW method over a limit) -- unless it is a MOVE: a baseline row of the same name less its directories, gone
# from the measure, at or above it (a source moved to another package re-keys its rows); the measure's limit may not rise past the baseline's. The baseline itself may grow
# (a row added or raised, a limit raised) only in a commit changing it that carries the ratchet's trailer
# (`Complexity: <why>`, `Length: <why>`, `Duplication: <why>`). A fall needs none: `update` writes the measured rows, every fall included.
#
#   quality_ratchet.py check --current <rows> [--ratchet NAME ...] [--range A..B ...]
#       violations: measured rows past a baseline (exit 1), and, for each range, a baseline grown between A and B with
#       no commit in the range changing it carrying its trailer (exit 1). Falls not yet recorded are said, and pass.
#   quality_ratchet.py update --current <rows> [--ratchet NAME ...]
#       each ratchet's baseline rewritten from the measured rows (a rise is then committed with its trailer).
from __future__ import annotations

import argparse
import subprocess
import sys
from dataclasses import dataclass
from typing import Dict, List, Tuple

Rows = Dict[Tuple[str, str], int]


@dataclass(frozen=True)
class Ratchet:
    name: str
    baseline: str
    trailer: str
    kinds: Tuple[str, ...]
    header: str
    hint: str


RATCHETS: Dict[str, Ratchet] = {r.name: r for r in (
    Ratchet("complexity", "test/resources/complexity-baseline.tsv", "Complexity", ("cognitive", "cyclomatic", "inner-defs"),
            """# THE COMPLEXITY BASELINE (scripts/quality-report --update writes it; scripts/ci/quality_ratchet.py holds every push to it).
# Every production method (*/src/main of a deployed module, quality.MethodMeasures) past a limit, frozen at its value:
#   limit       cognitive / cyclomatic / inner-defs: a method without a row stays at or below it
#   cognitive   <path>:<Owner.method>, its cognitive complexity (SonarSource's, nesting increments included)
#   cyclomatic  <path>:<Owner.method>, its cyclomatic complexity (McCabe)
#   inner-defs  <path>:<Owner.method>, the defs nested in it (CLAUDE.md: refactor past two inner methods)
# A row may only fall (--update records it, no trailer); a row added or raised, or a limit raised, lands only in a
# commit carrying `Complexity: <why>` (CLAUDE.md, "Code quality ratchets").""",
            "Split the method (extract the branches, name the inner defs as methods), or record it: scripts/quality-report"
            " --update, committed with a `Complexity: <why>` trailer."),
    Ratchet("duplication", "test/resources/duplication-baseline.tsv", "Duplication", ("copies",),
            """# THE DUPLICATION BASELINE (scripts/quality-report --update writes it; scripts/ci/quality_ratchet.py holds every push to it).
# Every clone group in the production sources (*/src/main of a deployed module; PMD CPD, 100 tokens or more), frozen:
#   limit   copies 0: a file set without a row holds no clone
#   copies  <path>|<path>..., the files a clone group spans (sorted), and the copies of every group spanning exactly
#           them: a new group adds a row or raises one, a group gaining a copy too; an edit moving a clone's lines
#           leaves its row
# A row may only fall (--update records it, no trailer); a row added or raised lands only in a commit carrying
# `Duplication: <why>` (CLAUDE.md, "Code quality ratchets").""",
            "Extract the copied block (CLAUDE.md: extract at two uses; scripts/quality-report lists every group and its"
            " lines), or record it: scripts/quality-report --update, committed with a `Duplication: <why>` trailer."),
    Ratchet("length", "test/resources/length-baseline.tsv", "Length", ("method-lines", "file-lines"),
            """# THE LENGTH BASELINE (scripts/quality-report --update writes it; scripts/ci/quality_ratchet.py holds every push to it).
# Every production method and source (*/src/main of a deployed module) past its length limit, frozen at its length:
#   limit         method-lines / file-lines: a method or source without a row stays at or below it
#   method-lines  <path>:<Owner.method>, the lines from its `def` to its body's end
#   file-lines    <path>, the source's lines
# A row may only fall (--update records it, no trailer); a row added or raised, or a limit raised, lands only in a
# commit carrying `Length: <why>` (CLAUDE.md, "Code quality ratchets").""",
            "Split the method or the source (a class, an object, a file of its own), or record it: scripts/quality-report"
            " --update, committed with a `Length: <why>` trailer."),
)}


def parse(text: str) -> Rows:
    rows: Rows = {}
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) != 3 or not parts[2].lstrip("-").isdigit():
            raise ValueError(f"line {n}: not `kind<TAB>key<TAB>value`: {line!r}")
        rows[(parts[0], parts[1])] = int(parts[2])
    return rows


def of_kind(rows: Rows, kind: str) -> Dict[str, int]:
    return {key: value for (k, key), value in rows.items() if k == kind}


def limit_of(rows: Rows, kind: str) -> int:
    limits = of_kind(rows, "limit")
    if kind not in limits:
        raise ValueError(f"no `limit<TAB>{kind}<TAB>N` row")
    return limits[kind]


def identity(key: str) -> str:
    """A key less its directories (`<File>.scala:<Owner.method>`, `<File>.scala`, `<File>.scala|<File>.scala`): what a
    move of a source to another package or module leaves alone."""
    return "|".join(part.rsplit("/", 1)[-1] for part in key.split("|"))


def moved_from(key: str, value: int, before: Dict[str, int], present: Dict[str, int]):
    """The row `key` is a MOVE of: a row of `before` of the same identity, no longer `present`, at or above `value`
    (a source moved to another package re-keys its rows, and the move adds nothing). None for none."""
    return next((old for old, was in sorted(before.items())
                 if old not in present and identity(old) == identity(key) and was >= value), None)


def violations(ratchet: Ratchet, current: Rows, baseline: Rows) -> List[str]:
    """The measured rows past the baseline, one sentence each."""
    out = []
    for kind in ratchet.kinds:
        limit = limit_of(baseline, kind)
        if limit_of(current, kind) > limit:
            out.append(f"the measure's {kind} limit {limit_of(current, kind)} is past the baseline's {limit}")
        base = of_kind(baseline, kind)
        for key, value in sorted(of_kind(current, kind).items()):
            if key in base:
                if value > base[key]:
                    out.append(f"{kind} {key}: {value}, past its baseline {base[key]}")
            elif value > limit and not moved_from(key, value, base, of_kind(current, kind)):
                out.append(f"{kind} {key}: {value}, past the limit {limit} (no baseline row)")
    return out


def growth(ratchet: Ratchet, old: Rows, new: Rows) -> List[str]:
    """How the baseline grew from `old` to `new`: a limit raised, a row added past the old limit or raised."""
    out = []
    for kind in ratchet.kinds:
        old_limit = of_kind(old, "limit").get(kind)
        new_limit = limit_of(new, kind)
        if old_limit is not None and new_limit > old_limit:
            out.append(f"limit {kind} raised {old_limit} -> {new_limit}")
        allowed_new = old_limit if old_limit is not None else new_limit
        before = of_kind(old, kind)
        after = of_kind(new, kind)
        for key, value in sorted(after.items()):
            if value > before.get(key, allowed_new) and not (key not in before and moved_from(key, value, before, after)):
                out.append(f"{kind} {key} raised {before.get(key, f'<={allowed_new}')} -> {value}")
    return out


def falls(ratchet: Ratchet, current: Rows, baseline: Rows) -> List[str]:
    """The baseline rows the measured code is below (or no longer past the limit): falls `update` would record."""
    out = []
    for kind in ratchet.kinds:
        now = of_kind(current, kind)
        for key, value in sorted(of_kind(baseline, kind).items()):
            if now.get(key, 0) < value:
                out.append(f"{kind} {key} {value} -> {now.get(key, 'within the limit')}")
    return out


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def file_at(rev: str, path: str) -> str:
    shown = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return shown.stdout if shown.returncode == 0 else ""


def justified(rng: str, path: str, trailer: str) -> bool:
    """Whether a commit of `rng` changing `path` carries a non-empty `<trailer>:` trailer."""
    log = git("log", f"--format=%x00%(trailers:key={trailer},valueonly,separator=%x20)", rng, "--", path)
    return any(entry.strip() for entry in log.split("\x00"))


def check_one(ratchet: Ratchet, current: Rows, ranges: List[str]) -> bool:
    with open(ratchet.baseline) as f:
        baseline = parse(f.read())
    failed = False
    found = violations(ratchet, current, baseline)
    if found:
        failed = True
        print(f"{ratchet.name.upper()} GREW past {ratchet.baseline}:")
        for v in found:
            print(f"  - {v}")
        print(f"  {ratchet.hint}")
    for rng in ranges:
        old = file_at(rng.split("..")[0], ratchet.baseline)
        grown = growth(ratchet, parse(old), baseline) if old else []
        if grown and not justified(rng, ratchet.baseline, ratchet.trailer):
            failed = True
            print(f"THE {ratchet.name.upper()} BASELINE GREW in {rng} with no `{ratchet.trailer}: <why>` trailer on the"
                  " commit changing it:")
            for g in grown:
                print(f"  - {g}")
    down = falls(ratchet, current, baseline)
    if down:
        print(f"{ratchet.name} fell ({len(down)} rows below {ratchet.baseline}, not yet recorded: scripts/quality-report"
              " --update records them, no trailer needed):")
        for d in down[:20]:
            print(f"  - {d}")
        if len(down) > 20:
            print(f"  - ... and {len(down) - 20} more")
    if not failed:
        rows = sum(len(of_kind(current, kind)) for kind in ratchet.kinds)
        print(f"{ratchet.name}: within {ratchet.baseline} ({rows} rows past a limit)")
    return not failed


def update_one(ratchet: Ratchet, current: Rows) -> None:
    lines = [ratchet.header]
    lines += [f"limit\t{kind}\t{limit_of(current, kind)}" for kind in ratchet.kinds]
    for kind in ratchet.kinds:
        lines += [f"{kind}\t{key}\t{value}" for key, value in sorted(of_kind(current, kind).items())]
    with open(ratchet.baseline, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"{ratchet.name}: wrote {ratchet.baseline} -- a rise is committed with a `{ratchet.trailer}: <why>` trailer")


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    for name in ("check", "update"):
        c = sub.add_parser(name)
        c.add_argument("--current", required=True)
        c.add_argument("--ratchet", action="append", choices=sorted(RATCHETS), default=[])
        if name == "check":
            c.add_argument("--range", action="append", default=[])
    args = parser.parse_args(argv)
    try:
        with open(args.current) as f:
            current = parse(f.read())
        chosen = [RATCHETS[name] for name in (args.ratchet or sorted(RATCHETS))]
        if args.command == "update":
            for ratchet in chosen:
                update_one(ratchet, current)
            return 0
        results = [check_one(ratchet, current, args.range) for ratchet in chosen]
        return 0 if all(results) else 1
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"quality_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
