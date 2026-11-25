"""The coupling ratchet's rows (coupling_ratchet.py, its module docstring): the `kind<TAB>key<TAB>value` rows the
couplingReport measures and the baseline holds, their limits, the layer table, the frozen kinds and how they are
measured, the reference sets, and the baseline `update` writes (each frozen distance row followed by its ceiling)."""
from __future__ import annotations

import os
import subprocess
from typing import AbstractSet, Dict, List, Optional, Set, Tuple

import baseline_ratchet

BASELINE_PATH = "test/resources/coupling-baseline.tsv"
LAYERS_PATH = "test/resources/coupling-layers.tsv"
EXEMPTIONS_PATH = "test/resources/coupling-exemptions.tsv"
REFS_PATH = "test/resources/coupling-fanout-refs.tsv"
DISTANCE_SPLIT_TOLERANCE = 2  # hundredths a valid split may raise the distance of a package holding X, a piece, or a use
DISTANCE_CAP = "distance-cap"  # a frozen distance row's ceiling (THE CEILING): it only falls
PRESENCE = ("module-edge", "cycle-edge")  # only the pair is held, its weight is information (a cycle edge: none at all)
VALUED = ("ce", "fanout", "distance")
FROZEN = (("fanout", "class", "fan-out"), ("distance", "package", "distance D (hundredths)"))  # only an exemption admits a new one
DEFAULT_LIMITS = {"fanout": 9, "fanout-measure": 1, "new-package-ce": 8, "distance": 20, "distance-min-types": 8,
                  "distance-measure": 1}
# How a frozen kind is measured (project/Coupling.scala's FanOutMeasure, PackageMeasure): a baseline without the row is
# measure 1. A measurement fix raises it, re-taking the kind's rows at their true values under its `Coupling:` trailer;
# a distance re-measure keeps every ceiling (THE CEILING): it re-takes the rows, never the owner's caps.
MEASURES = ("fanout-measure", "distance-measure")
LIMIT_KEYS = {"fanout": ("fanout", "fanout-measure"), "distance": ("distance", "distance-min-types", "distance-measure")}

Rows = Dict[Tuple[str, str], int]
Edges = Dict[str, List[str]]  # "<from package>-><to package>" -> its class references, "<from class> -> <to class>"
Layers = Tuple[Dict[str, int], Set[str], Dict[str, int]]
Fanouts = Tuple[Dict[str, int], Dict[str, Set[str]]]  # every class's held fan-out, and what it counts (when given)


def parse(text: str) -> Rows:
    return baseline_ratchet.parse_rows(text, int)


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


def moved(key: str, value: int, old: Dict[str, int], new: Dict[str, int]) -> bool:
    """Whether `key` is a frozen row that moved: a row of the same simple name, at or above `value`, gone from `new` --
    and no more rows of that simple name new in `new` than gone from it: one row gone to two is no move."""
    simple = key.rsplit(".", 1)[-1]
    gone = [v for k, v in old.items() if k.rsplit(".", 1)[-1] == simple and k not in new]
    arrived = [k for k in new if k.rsplit(".", 1)[-1] == simple and k not in old]
    return any(v >= value for v in gone) and len(arrived) <= len(gone)


def parse_layers(text: str) -> Layers:
    """The layer table's `layer` rows (prefix -> layer), `allow` edges and `rank` rows (package -> rank)
    (project/Coupling.scala's Layers.parse is the one that judges the table's shape; this reads only what loosening
    compares and the violations cite)."""
    rows: Dict[str, int] = {}
    allowed: Set[str] = set()
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


def layer_of(rows: Dict[str, int], pkg: str) -> Optional[int]:
    """A package's layer: the longest prefix row naming it (Coupling.Layers.layerOf), None for none."""
    named = [p for p in rows if pkg == p or pkg.startswith(p + ".")]
    return rows[max(named, key=len)] if named else None


def layer_units(layers_text: str) -> Set[str]:
    """The packages of three or more segments the layer table gives a row of their own (Coupling.Layers.units): each is
    measured as a package of its own, not as part of its top two segments."""
    rows, _, ranks = parse_layers(layers_text)
    return {p for p in list(rows) + list(ranks) if p.count(".") >= 2}


def package_of(cls: str, units: AbstractSet[str] = frozenset()) -> str:
    """A class's package as the report groups it (Coupling.packageOf): the longest of `units` (the deeper packages
    measured on their own, `layer_units`) that is its full package or holds it, else the full package cut to its top two
    segments."""
    full = cls.rpartition(".")[0]
    if not full:
        return "<root>"
    held = [u for u in units if full == u or full.startswith(u + ".")]
    return max(held, key=len) if held else ".".join(full.split(".")[:2])


def recorded_refs(text: str) -> Dict[str, Set[str]]:
    """The frozen reference sets (test/resources/coupling-fanout-refs.tsv): `<class><TAB><target>` a line."""
    out: Dict[str, Set[str]] = {}
    for line in text.splitlines():
        if "\t" in line and not line.startswith("#"):
            cls, target = line.split("\t", 1)
            out.setdefault(cls, set()).add(target)
    return out


def read_fanouts(path: str) -> Optional[Fanouts]:
    """couplingReport's fanouts file: `<class><TAB><held fan-out>[<TAB><target>,<target>...]` a line, every class; None
    without one."""
    if not path:
        return None
    counts: Dict[str, int] = {}
    targets: Dict[str, Set[str]] = {}
    with open(path) as f:
        for cols in (line.split("\t") for line in f.read().splitlines() if "\t" in line):
            counts[cols[0]] = int(cols[1])
            if len(cols) > 2:
                targets[cols[0]] = set(filter(None, cols[2].split(",")))
    return counts, targets


def refrozen(old: Rows, new: Rows, kind: str) -> bool:
    """Whether the limits of a frozen kind changed between `old` and `new`: its rows are then taken again, under the new
    limit -- the change that sets a limit (a raise is growth, needing its trailer; a fall tightens) or how the kind is
    measured (a measure's absent row is measure 1)."""
    old_lim, new_lim = limits(old), limits(new)
    return any((("limit", key) not in old and key not in MEASURES) or old_lim.get(key) != new_lim.get(key)
               for key in LIMIT_KEYS[kind])


def remeasured_only(old: Rows, new: Rows, kind: str) -> bool:
    """Whether only how `kind` is measured changed between `old` and `new` (its MEASURES row), not a limit."""
    old_lim, new_lim = limits(old), limits(new)
    keys = LIMIT_KEYS[kind]
    return all(old_lim.get(k) == new_lim.get(k) for k in keys if k not in MEASURES) and refrozen(old, new, kind)


def ceilings(rows: Rows) -> Dict[str, int]:
    """Each frozen distance row's ceiling (THE CEILING): its `distance-cap` row, else the row + DISTANCE_SPLIT_TOLERANCE."""
    caps = of_kind(rows, DISTANCE_CAP)
    return {pkg: caps.get(pkg, d + DISTANCE_SPLIT_TOLERANCE) for pkg, d in of_kind(rows, "distance").items()}


def with_ceilings(measured_text: str, sides: List[Rows]) -> str:
    """The baseline `update` writes: the measured rows, each frozen distance row followed by its ceiling -- the least of
    each side's (`sides`: the baselines it replaces) and the row + DISTANCE_SPLIT_TOLERANCE."""
    measured = parse(measured_text)
    caps = [ceilings(side) for side in sides]
    out: List[str] = []
    for line in measured_text.splitlines():
        parts = line.split("\t")
        if len(parts) == 3 and parts[0] == DISTANCE_CAP:
            continue
        out.append(line)
        if len(parts) == 3 and parts[0] == "distance":
            pkg, d = parts[1], of_kind(measured, "distance")[parts[1]]
            out.append(f"{DISTANCE_CAP}\t{pkg}\t{min([c[pkg] for c in caps if pkg in c] + [d + DISTANCE_SPLIT_TOLERANCE])}")
    return "\n".join(out) + "\n"


def sides_of(path: str) -> List[Rows]:
    """The baselines an update replaces: the file as it stands and, while git holds it unmerged (a rebase's conflict
    that agentctl's regen resolves), each stage of it -- the base, ours and theirs."""
    sides: List[Rows] = []
    if os.path.exists(path):
        with open(path) as f:
            text = f.read()
        if "\n<<<<<<< " not in "\n" + text:
            sides.append(parse(text))
    for stage in (1, 2, 3):
        found = subprocess.run(["git", "show", f":{stage}:{path}"], capture_output=True, text=True)
        if found.returncode == 0:
            sides.append(parse(found.stdout))
    return sides


def update(current_path: str, baseline_path: str) -> int:
    with open(current_path) as f:
        measured = f.read()
    text = with_ceilings(measured, sides_of(baseline_path))
    with open(baseline_path, "w") as f:
        f.write(text)
    return 0
