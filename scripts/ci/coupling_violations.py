"""The measured rows the coupling ratchet refuses (coupling_ratchet.py, its module docstring): a package cycle edge, a
layer or rank violation, a new cross-module edge kind, a Ce past its row, a frozen fan-out or distance past its row or
newly past the limit -- one sentence each."""
from __future__ import annotations

from typing import Callable, Dict, List, Optional, Tuple

from coupling_rows import EXEMPTIONS_PATH, FROZEN, Edges, Rows, limits, moved, of_kind

Renamed = Dict[Tuple[str, str], str]


def behind(edges: Edges, key: str) -> str:
    """The class references behind a package edge, the first three: what to cut."""
    refs = edges.get(key, [])
    return f": {', '.join(refs[:3])}{', ...' if len(refs) > 3 else ''}" if refs else ""


def cycle_edges(current: Rows, edges: Edges) -> List[str]:
    return [f"a package cycle edge: {key} ({refs} class references{behind(edges, key)}) -- the package graph has no "
            "cycle, and no `Coupling:` trailer excuses one: cut it (scripts/coupling-report names every edge of the cycle)"
            for key, refs in sorted(of_kind(current, "cycle-edge").items())]


def layer_violations(current: Rows, edges: Edges) -> List[str]:
    out: List[str] = []
    for key, refs in sorted(of_kind(current, "layer-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no layer: give it a row in test/resources/coupling-layers.tsv")
        else:
            out.append(f"a package edge into a higher layer: {key} ({refs} class references{behind(edges, key)}) -- cut it, or "
                       "allow it with its why in test/resources/coupling-layers.tsv")
    return out


def rank_violations(current: Rows, ranks: Dict[str, int], edges: Edges) -> List[str]:
    def at(p: str) -> str:
        return f" (rank {ranks[p]})" if p in ranks else ""

    out: List[str] = []
    for key, refs in sorted(of_kind(current, "rank-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no rank in its layer: give it a `rank` row in test/resources/coupling-layers.tsv")
        else:
            out.append(f"an upward edge inside a layer: {a}{at(a)} -> {b}{at(b)} ({refs} class references{behind(edges, key)}) -- "
                       f"the allowed direction is {b} -> {a}: cut it (move the type down, or a trait into {a}), or re-rank with "
                       "its why in test/resources/coupling-layers.tsv")
    return out


def module_edges(current: Rows, baseline: Rows) -> List[str]:
    known = of_kind(baseline, "module-edge")
    return [f"a new cross-module edge kind: {key} ({refs} class references)"
            for key, refs in sorted(of_kind(current, "module-edge").items()) if key not in known]


def ce_rows(current: Rows, baseline: Rows) -> List[str]:
    out: List[str] = []
    base_ce = of_kind(baseline, "ce")
    for key, ce in sorted(of_kind(current, "ce").items()):
        allowed = base_ce.get(key, limits(baseline)["new-package-ce"])
        if ce > allowed:
            was = f"its baseline {allowed}" if key in base_ce else f"the new-package limit {allowed}"
            out.append(f"package {key}: Ce {ce} past {was}")
    return out


def admits(kind: str) -> str:
    """What may admit a row newly past its limit: an exemption row for a fan-out or a distance; for the data limits
    (a data type's references, a class's data types) nothing -- cut it: a type moved into the data bucket is no way out."""
    if kind in ("datafanout", "databag"):
        return "no exemption admits it: split the type or hand its shapes on as one value, never --update"
    return f"only an exemption row admits it ({EXEMPTIONS_PATH}, with `Coupling: <why>`), never --update"


def frozen_kind_rows(kind: str, noun: str, what: str, current: Rows, baseline: Rows, renamed: Renamed) -> List[str]:
    out: List[str] = []
    limit = limits(baseline)[kind]
    base, cur = of_kind(baseline, kind), of_kind(current, kind)
    for key, value in sorted(cur.items()):
        if key in base:
            if value > base[key]:
                out.append(f"{noun} {key}: {what} {value} past its frozen row {base[key]} -- a frozen row only falls")
        elif not moved(key, value, base, cur) and (kind, key) not in renamed:
            out.append(f"{noun} {key}: {what} {value} past the limit {limit} -- cut it below the limit; " + admits(kind))
    return out


def frozen_rows(current: Rows, baseline: Rows, renamed: Renamed) -> List[str]:
    return [line for kind, noun, what in FROZEN for line in frozen_kind_rows(kind, noun, what, current, baseline, renamed)]


def violations(current: Rows, baseline: Rows, ranks: Optional[Dict[str, int]] = None, edges: Optional[Edges] = None,
               renamed: Optional[Renamed] = None) -> List[str]:
    """The measured rows the ratchet refuses -- a package cycle edge, a layer or rank violation, a row past the
    baseline -- one sentence each. `renamed`: the (kind, key) rows a range's renames carry over (`rename_credit`)."""
    ranks, edges, renamed = ranks or {}, edges or {}, renamed or {}
    parts: List[Callable[[], List[str]]] = [
        lambda: cycle_edges(current, edges),
        lambda: layer_violations(current, edges),
        lambda: rank_violations(current, ranks, edges),
        lambda: module_edges(current, baseline),
        lambda: ce_rows(current, baseline),
        lambda: frozen_rows(current, baseline, renamed),
    ]
    return [line for part in parts for line in part()]
