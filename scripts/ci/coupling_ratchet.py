#!/usr/bin/env python3
# THE COUPLING RATCHET (the pre-push sbt check's coupling step; CLAUDE.md, Design rules): the production code's coupling,
# measured from Zinc's class-dependency record (`sbt couplingReport`, project/Coupling.scala), has NO package cycle --
# any `cycle-edge` row fails, measured or recorded in the baseline, and no `Coupling:` trailer excuses one -- and may
# not grow past the checked-in baseline (test/resources/coupling-baseline.tsv): no new cross-module edge kind, no
# package's Ce (the outside packages its classes use) past its row (or past the new-package limit), no class's fan-out
# past its row (or, without a row, to the limit), no package edge into a higher layer or, inside an ordered layer, into
# a higher rank (the layering, test/resources/coupling-layers.tsv: a `layer-violation` or `rank-violation` row fails
# whatever the baseline says, naming the direction allowed), no package without its layer or rank -- and the baseline
# itself may grow, and the layer table loosen (an `allow` row added, a package given another layer or rank: either lets
# an edge through that failed before), only in a commit changing that file carrying `Coupling: <why>`. A fall needs no
# trailer: `scripts/coupling-report --update` writes the measured rows, every fall included; a new package's row, or a
# row dropped, loosens nothing.
#
#   coupling_ratchet.py check --current <rows> --baseline <file> [--layers <file>] [--edges <file>] [--range A..B ...]
#       violations: a package cycle edge and the measured rows past the baseline (exit 1), and, for each range, a
#       baseline grown or a layer table loosened between A and B with no commit in the range that changed it carrying
#       `Coupling:` (exit 1). Falls not yet recorded are said, and pass. `--edges` (couplingReport's third output, every
#       class reference with its package edge) names the class references behind each refused edge.
#
# A row is `kind<TAB>key<TAB>value`; `#` lines are comments (project/Coupling.scala's BaselineHeader has the kinds).
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from typing import Dict, List, Tuple

BASELINE_PATH = "test/resources/coupling-baseline.tsv"
LAYERS_PATH = "test/resources/coupling-layers.tsv"
PRESENCE = ("module-edge", "cycle-edge")  # only the pair is held, its weight is information (a cycle edge: none at all)
VALUED = ("ce", "fanout")
DEFAULT_LIMITS = {"fanout": 30, "new-package-ce": 8}

Rows = Dict[Tuple[str, str], int]
Edges = Dict[str, List[str]]  # "<from package>-><to package>" -> its class references, "<from class> -> <to class>"


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


def read_edges(text: str) -> Edges:
    """couplingReport's edges file: `<from package>-><to package><TAB><from class>-><to class>` a line."""
    out: Edges = {}
    for line in text.splitlines():
        if "\t" in line:
            key, ref = line.split("\t", 1)
            out.setdefault(key, []).append(ref.replace("->", " -> "))
    return out


def behind(edges: Edges, key: str) -> str:
    """The class references behind a package edge, the first three: what to cut."""
    refs = edges.get(key, [])
    return f": {', '.join(refs[:3])}{', ...' if len(refs) > 3 else ''}" if refs else ""


def violations(current: Rows, baseline: Rows, ranks: Dict[str, int] = None, edges: Edges = None) -> List[str]:
    """The measured rows the ratchet refuses -- a package cycle edge, a layer or rank violation, a row past the
    baseline -- one sentence each."""
    lim = limits(baseline)
    ranks, edges = ranks or {}, edges or {}
    out = []
    for key, refs in sorted(of_kind(current, "cycle-edge").items()):
        out.append(f"a package cycle edge: {key} ({refs} class references{behind(edges, key)}) -- the package graph has no "
                   "cycle, and no `Coupling:` trailer excuses one: cut it (scripts/coupling-report names every edge of the cycle)")
    for key, refs in sorted(of_kind(current, "layer-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no layer: give it a row in test/resources/coupling-layers.tsv")
        else:
            out.append(f"a package edge into a higher layer: {key} ({refs} class references{behind(edges, key)}) -- cut it, or "
                       "allow it with its why in test/resources/coupling-layers.tsv")
    for key, refs in sorted(of_kind(current, "rank-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no rank in its layer: give it a `rank` row in test/resources/coupling-layers.tsv")
        else:
            at = lambda p: f" (rank {ranks[p]})" if p in ranks else ""
            out.append(f"an upward edge inside a layer: {a}{at(a)} -> {b}{at(b)} ({refs} class references{behind(edges, key)}) -- "
                       f"the allowed direction is {b} -> {a}: cut it (move the type down, or a trait into {a}), or re-rank with "
                       "its why in test/resources/coupling-layers.tsv")
    for key, refs in sorted(of_kind(current, "module-edge").items()):
        if key not in of_kind(baseline, "module-edge"):
            out.append(f"a new cross-module edge kind: {key} ({refs} class references)")
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


Layers = Tuple[Dict[str, int], set, Dict[str, int]]


def parse_layers(text: str) -> Layers:
    """The layer table's `layer` rows (prefix -> layer), `allow` edges and `rank` rows (package -> rank)
    (project/Coupling.scala's Layers.parse is the one that judges the table's shape; this reads only what loosening
    compares and the violations cite)."""
    rows: Dict[str, int] = {}
    allowed = set()
    ranks: Dict[str, int] = {}
    for line in text.splitlines():
        cols = line.split("\t")
        if cols[0] == "layer" and len(cols) >= 3 and cols[1].isdigit():
            rows[cols[2]] = int(cols[1])
        elif cols[0] == "rank" and len(cols) >= 3 and cols[1].isdigit():
            ranks[cols[2]] = int(cols[1])
        elif cols[0] == "allow" and len(cols) >= 2:
            allowed.add(cols[1])
    return rows, allowed, ranks


def layer_of(rows: Dict[str, int], pkg: str):
    """A package's layer: the longest prefix row naming it (Coupling.Layers.layerOf), None for none."""
    named = [p for p in rows if pkg == p or pkg.startswith(p + ".")]
    return rows[max(named, key=len)] if named else None


def loosening(old: Layers, new: Layers) -> List[str]:
    """How the layer table loosened from `old` to `new`: an `allow` edge added, or a package that had a layer or a rank
    given another (up, its own edges may point higher; down, lower packages may reach it). A new package taking its
    first layer or rank, or a row dropped (its packages unplaced: a violation), loosens nothing."""
    out = [f"allow {edge} added" for edge in sorted(new[1] - old[1])]
    for prefix in sorted(set(old[0]) | set(new[0])):
        was, now = layer_of(old[0], prefix), layer_of(new[0], prefix)
        if was is not None and now is not None and was != now:
            out.append(f"{prefix} moved from layer {was} to {now}")
    for pkg in sorted(set(old[2]) & set(new[2])):
        if old[2][pkg] != new[2][pkg]:
            out.append(f"{pkg} re-ranked from {old[2][pkg]} to {new[2][pkg]}")
    return out


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def file_at(rev: str, path: str) -> str:
    shown = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return shown.stdout if shown.returncode == 0 else ""


def justified(rng: str, path: str = BASELINE_PATH) -> bool:
    """Whether a commit of `rng` changing `path` carries a non-empty `Coupling:` trailer."""
    log = git("log", "--format=%x00%(trailers:key=Coupling,valueonly,separator=%x20)", rng, "--", path)
    return any(entry.strip() for entry in log.split("\x00"))


def check(current_path: str, baseline_path: str, ranges: List[str], layers_path: str = LAYERS_PATH,
          edges_path: str = "") -> int:
    with open(current_path) as f:
        current = parse(f.read())
    with open(baseline_path) as f:
        baseline = parse(f.read())
    ranks: Dict[str, int] = {}
    if os.path.exists(layers_path):
        with open(layers_path) as f:
            ranks = parse_layers(f.read())[2]
    edges: Edges = {}
    if edges_path:
        with open(edges_path) as f:
            edges = read_edges(f.read())
    failed = False
    recorded = sorted(of_kind(baseline, "cycle-edge"))
    if recorded:
        failed = True
        print(f"THE COUPLING BASELINE RECORDS A PACKAGE CYCLE ({baseline_path}): the package graph has none, and no "
              "`Coupling:` trailer lets one in:")
        for key in recorded:
            print(f"  - cycle-edge {key}")
    found = violations(current, baseline, ranks, edges)
    if found:
        failed = True
        print(f"COUPLING REFUSED (a package cycle, a layer or rank violation, or a row past {baseline_path}):")
        for v in found:
            print(f"  - {v}")
        print("  Cut the new dependency (scripts/coupling-report shows the class references behind each edge). Only a row past")
        print("  the baseline -- never a cycle, a layer or a rank violation -- may be recorded instead: scripts/coupling-report")
        print("  --update, committed with a `Coupling: <why this coupling is right>` trailer.")
    for rng in ranges:
        base = rng.split("..")[0]
        grown = growth(parse(file_at(base, BASELINE_PATH)), baseline)
        if grown and not justified(rng):
            failed = True
            print(f"THE COUPLING BASELINE GREW in {rng} with no `Coupling: <why>` trailer on the commit changing it:")
            for g in grown:
                print(f"  - {g}")
        if os.path.exists(layers_path):
            with open(layers_path) as f:
                loosened = loosening(parse_layers(file_at(base, LAYERS_PATH)), parse_layers(f.read()))
            if loosened and not justified(rng, LAYERS_PATH):
                failed = True
                print(f"THE LAYER TABLE LOOSENED in {rng} ({layers_path}) with no `Coupling: <why>` trailer on the commit"
                      " changing it:")
                for item in loosened:
                    print(f"  - {item}")
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
    c.add_argument("--layers", default=LAYERS_PATH)
    c.add_argument("--edges", default="")
    c.add_argument("--range", action="append", default=[])
    args = parser.parse_args(argv)
    try:
        return check(args.current, args.baseline, args.range, args.layers, args.edges)
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"coupling_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
