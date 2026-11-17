#!/usr/bin/env python3
# THE COUPLING RATCHET (the pre-push sbt check's coupling step; CLAUDE.md, Design rules): the production code's coupling,
# measured from Zinc's class-dependency record (`sbt couplingReport`, project/Coupling.scala), may not grow past the
# checked-in baseline (test/resources/coupling-baseline.tsv) -- no new package cycle edge, no new cross-module edge
# kind, no package's Ce (the outside packages its classes use) past its row (or past the new-package limit), no
# class's fan-out past its row (or, without a row, to the limit), no package edge into a higher layer (the layering,
# test/resources/coupling-layers.tsv: a `layer-violation` row fails whatever the baseline says) -- and the baseline itself may grow only in a commit
# carrying `Coupling: <why>`. A fall needs no
# trailer: `scripts/coupling-report --update` writes the measured rows, every fall included.
#
#   coupling_ratchet.py check --current <rows> --baseline <file> [--range A..B ...]
#       violations: the measured rows past the baseline (exit 1), and, for each range, a baseline grown between A and B
#       with no commit in the range that changed it carrying `Coupling:` (exit 1). Falls not yet recorded are said, and
#       pass.
#
# A row is `kind<TAB>key<TAB>value`; `#` lines are comments (project/Coupling.scala's BaselineHeader has the kinds).
from __future__ import annotations

import argparse
import subprocess
import sys
from typing import Dict, List, Tuple

BASELINE_PATH = "test/resources/coupling-baseline.tsv"
PRESENCE = ("module-edge", "cycle-edge")  # only the pair is held, its weight is information
VALUED = ("ce", "fanout")
DEFAULT_LIMITS = {"fanout": 30, "new-package-ce": 8}

Rows = Dict[Tuple[str, str], int]


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


def limits(rows: Rows) -> Dict[str, int]:
    out = dict(DEFAULT_LIMITS)
    out.update({key: value for (kind, key), value in rows.items() if kind == "limit"})
    return out


def of_kind(rows: Rows, kind: str) -> Dict[str, int]:
    return {key: value for (k, key), value in rows.items() if k == kind}


def violations(current: Rows, baseline: Rows) -> List[str]:
    """The measured rows past the baseline, one sentence each."""
    lim = limits(baseline)
    out = []
    for key, refs in sorted(of_kind(current, "layer-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no layer: give it a row in test/resources/coupling-layers.tsv")
        else:
            out.append(f"a package edge into a higher layer: {key} ({refs} class references) -- cut it, or allow it with its why "
                       "in test/resources/coupling-layers.tsv")
    for key, refs in sorted(of_kind(current, "module-edge").items()):
        if key not in of_kind(baseline, "module-edge"):
            out.append(f"a new cross-module edge kind: {key} ({refs} class references)")
    for key, refs in sorted(of_kind(current, "cycle-edge").items()):
        if key not in of_kind(baseline, "cycle-edge"):
            out.append(f"a new package cycle edge: {key} ({refs} class references) closes a package cycle")
    base_ce = of_kind(baseline, "ce")
    for key, ce in sorted(of_kind(current, "ce").items()):
        allowed = base_ce.get(key, lim["new-package-ce"])
        if ce > allowed:
            was = f"its baseline {allowed}" if key in base_ce else f"the new-package limit {allowed}"
            out.append(f"package {key}: Ce {ce} past {was}")
    base_fan = of_kind(baseline, "fanout")
    for key, fan in sorted(of_kind(current, "fanout").items()):
        if key in base_fan:
            if fan > base_fan[key]:
                out.append(f"class {key}: fan-out {fan} past its baseline {base_fan[key]}")
        elif fan >= lim["fanout"]:
            out.append(f"class {key}: fan-out {fan} reaches the limit {lim['fanout']}")
    return out


def growth(old: Rows, new: Rows) -> List[str]:
    """How the baseline grew from `old` to `new`: rows added, values raised (the presence kinds' weights aside)."""
    out = []
    old_lim, new_lim = limits(old), limits(new)
    for key in sorted(new_lim):
        if new_lim[key] > old_lim.get(key, 0):
            out.append(f"limit {key} raised {old_lim.get(key, 0)} -> {new_lim[key]}")
    for kind in PRESENCE:
        for key in sorted(set(of_kind(new, kind)) - set(of_kind(old, kind))):
            out.append(f"{kind} {key} added")
    old_ce = of_kind(old, "ce")
    for key, ce in sorted(of_kind(new, "ce").items()):
        allowed = old_ce.get(key, old_lim["new-package-ce"])
        if ce > allowed:
            out.append(f"ce {key} raised {allowed} -> {ce}")
    old_fan = of_kind(old, "fanout")
    for key, fan in sorted(of_kind(new, "fanout").items()):
        if fan > old_fan.get(key, old_lim["fanout"] - 1):
            out.append(f"fanout {key} raised {old_fan.get(key, '<' + str(old_lim['fanout']))} -> {fan}")
    return out


def falls(current: Rows, baseline: Rows) -> List[str]:
    """The baseline rows the measured code is below (or no longer has): falls `--update` would record."""
    out = []
    for kind in PRESENCE:
        out += [f"{kind} {key} gone" for key in sorted(set(of_kind(baseline, kind)) - set(of_kind(current, kind)))]
    for kind in VALUED:
        cur = of_kind(current, kind)
        for key, value in sorted(of_kind(baseline, kind).items()):
            if cur.get(key, 0) < value:
                out.append(f"{kind} {key} {value} -> {cur.get(key, 'below the limit' if kind == 'fanout' else 0)}")
    return out


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def baseline_at(rev: str) -> Rows:
    shown = subprocess.run(["git", "show", f"{rev}:{BASELINE_PATH}"], capture_output=True, text=True)
    return parse(shown.stdout) if shown.returncode == 0 else {}


def justified(rng: str) -> bool:
    """Whether a commit of `rng` changing the baseline carries a non-empty `Coupling:` trailer."""
    log = git("log", "--format=%x00%(trailers:key=Coupling,valueonly,separator=%x20)", rng, "--", BASELINE_PATH)
    return any(entry.strip() for entry in log.split("\x00"))


def check(current_path: str, baseline_path: str, ranges: List[str]) -> int:
    with open(current_path) as f:
        current = parse(f.read())
    with open(baseline_path) as f:
        baseline = parse(f.read())
    failed = False
    found = violations(current, baseline)
    if found:
        failed = True
        print(f"COUPLING GREW past {baseline_path}:")
        for v in found:
            print(f"  - {v}")
        print("  Cut the new dependency (scripts/coupling-report shows the class references behind each edge), or record")
        print("  it: scripts/coupling-report --update, committed with a `Coupling: <why this coupling is right>` trailer.")
    for rng in ranges:
        base = rng.split("..")[0]
        grown = growth(baseline_at(base), baseline)
        if grown and not justified(rng):
            failed = True
            print(f"THE COUPLING BASELINE GREW in {rng} with no `Coupling: <why>` trailer on the commit changing it:")
            for g in grown:
                print(f"  - {g}")
    down = falls(current, baseline)
    if down:
        print(f"coupling fell ({len(down)} rows below the baseline, not yet recorded: scripts/coupling-report --update"
              " records them, no trailer needed):")
        for d in down[:20]:
            print(f"  - {d}")
        if len(down) > 20:
            print(f"  - ... and {len(down) - 20} more")
    if not failed:
        print(f"coupling: within {baseline_path} ({len(current)} rows)")
    return 1 if failed else 0


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    c = sub.add_parser("check")
    c.add_argument("--current", required=True)
    c.add_argument("--baseline", default=BASELINE_PATH)
    c.add_argument("--range", action="append", default=[])
    args = parser.parse_args(argv)
    try:
        return check(args.current, args.baseline, args.range)
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"coupling_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
