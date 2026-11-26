"""PACKAGES, for every language (CLAUDE.md, "Coupling is reviewed"): a Python or JS directory stands for a Scala
package. Three rules over the directory graph the modules' imports make:

  distance  Martin's D = |A + I - 1| of a directory of `distance-min-modules`+ modules, where A counts COLLABORATORS
            only (abstract collaborators / collaborators: the owner's 2026-11-26 V2) and I = Ce / (Ca + Ce) -- measured
            only in a `distance` row's directory (where abstractions exist: Protocol/ABC, TypeScript)
  layer     a directory uses only its own rank or a lower one (`layer` rows); an edge up is a row `<from>-><to>`
  unplaced  a directory holding a measured module of a language that has `layer` rows, with no row of its own
  dircycle  a cycle of directories (none at all in Scala; frozen elsewhere)"""
from __future__ import annotations

from typing import Dict, List, Optional, Set, Tuple

from imports import cycles  # scripts/py-quality's Tarjan: one implementation
from model import Module
from rules_table import Rules

Row = Tuple[str, str, float]


def directory_edges(modules: Dict[str, Module]) -> Dict[str, Set[str]]:
    edges: Dict[str, Set[str]] = {}
    for m in modules.values():
        edges.setdefault(m.package, set())
        for target in m.imports:
            other = modules.get(target)
            if other is not None and other.package != m.package:
                edges[m.package].add(other.package)
    return edges


def instability(inside: Set[str], modules: Dict[str, Module]) -> Optional[float]:
    """Ce / (Ca + Ce) of the modules `inside`; None when nothing outside uses them or is used (no distance)."""
    efferent = {t for p in inside for t in modules[p].imports if t not in inside}
    afferent = {p for p, m in modules.items() if p not in inside and m.imports & inside}
    total = len(efferent) + len(afferent)
    return len(efferent) / total if total else None


def abstractness(inside: Set[str], modules: Dict[str, Module], collaborating: Set[str]) -> float:
    """Abstract collaborators / collaborators (V2: data types dilute nothing); 0 with no collaborator."""
    members = inside & collaborating
    return sum(1 for p in members if modules[p].abstract_types) / len(members) if members else 0.0


def distance(directory: str, modules: Dict[str, Module], collaborating: Set[str]) -> Optional[float]:
    inside = {p for p, m in modules.items() if m.package == directory}
    i = instability(inside, modules)
    return None if i is None else round(abs(abstractness(inside, modules, collaborating) + i - 1), 2)


def distance_rows(language: str, modules: Dict[str, Module], collaborating: Set[str], rules: Rules) -> List[Row]:
    out: List[Row] = []
    for lang, directory in sorted(rules.distance_dirs):
        size = sum(1 for m in modules.values() if m.package == directory)
        if lang != language or size < rules.limit("distance-min-modules"):
            continue
        d = distance(directory, modules, collaborating)
        if d is not None and d > rules.limit("distance"):
            out.append(("distance", directory, d))
    return out


def layer_rows(language: str, modules: Dict[str, Module], rules: Rules) -> List[Row]:
    if not any(layer.language == language for layer in rules.layers):
        return []
    out: List[Row] = []
    for source, targets in sorted(directory_edges(modules).items()):
        here = rules.layer_of(language, source)
        if here is None:
            out.append(("unplaced", source, 1))
            continue
        for target in sorted(targets):
            there = rules.layer_of(language, target)
            if there is not None and there.rank > here.rank:
                out.append(("layer", f"{source}->{target}", 1))
    return out


def rows(language: str, modules: Dict[str, Module], collaborating: Set[str], rules: Rules) -> List[Row]:
    out = distance_rows(language, modules, collaborating, rules) + layer_rows(language, modules, rules)
    out += [("dircycle", "|".join(group), len(group)) for group in cycles(directory_edges(modules))]
    return out
