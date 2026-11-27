"""THE NAMING RULES, for every language (the owner's "names must match behaviour", 2026-11-19; CLAUDE.md, "Design
rules"): one implementation over the names every extractor reads -- a Scala source's top-level types and their
members, a Python or JS/TS module's own name, its top-level types and functions.

  permutation  BLOCKS (naming_lint.py): two names of one package whose words are the same in another order
               (PruneSweep / SweepPrune, prune_sweep.py / sweep_prune.py)
  action       WARNS, new modules only (naming_lint.py): a type named for an action (...Apart, ...Plans, ...Reads) with at
               most one public def
  suffix       a frozen row (design_check.py): a type or module named ending in a table `suffix` (Impl, Manager, ...)
  abbreviation a frozen row (design_check.py): a type, module or top-level function with a word the table lists as an
               `abbreviation` and no `idiom` row allows"""
from __future__ import annotations

import re
from typing import Dict, Iterable, List, Set, Tuple

from model import Module, Name
from rules_table import Rules

WORD = re.compile(r"[A-Z]+(?![a-z])|[A-Z]?[a-z0-9]+")
ACTION_SUFFIX = re.compile(r"(Apart|Stages|Reads?|Readings?|Builds|Rereads|Drafts|Traces|Denials|Plans)$")
Row = Tuple[str, str, float]


def words(name: str) -> Tuple[str, ...]:
    """A name's words, lowercased: `snake_case`, `camelCase`, `PascalCase` and `kebab-case` alike."""
    return tuple(w.lower() for w in WORD.findall(name))


def top_names(m: Module) -> List[str]:
    return [n.name for n in m.names if n.kind in ("type", "module")]


def permutations(modules: Iterable[Module]) -> Dict[Tuple[str, str], Dict[str, str]]:
    """(language, package) -> {name: path} of every group of 2+ names with one set of words in different orders, from
    2+ files (a module and its own class -- build_image.py's ImageBuild -- are read together, as a Scala class and
    its companion are)."""
    groups: Dict[Tuple[str, str, Tuple[str, ...]], Dict[Tuple[str, ...], Tuple[str, str]]] = {}
    for m in modules:
        for name in top_names(m):
            ordered = words(name)
            group = groups.setdefault((m.language, m.package, tuple(sorted(ordered))), {})
            group.setdefault(ordered, (name, m.path))
    out: Dict[Tuple[str, str], Dict[str, str]] = {}
    for (language, package, _), orders in sorted(groups.items()):
        if len(orders) > 1 and len({p for _, p in orders.values()}) > 1:
            out.setdefault((language, package), {}).update({n: p for n, p in orders.values()})
    return out


def action_names(modules: Iterable[Module]) -> List[str]:
    out: List[str] = []
    for m in modules:
        counts: Dict[str, int] = {}
        for n in m.names:
            if n.kind == "type":  # a class and its companion: one name, their defs together
                counts[n.name] = counts.get(n.name, 0) + n.public_defs
        out += [f"{name} ({m.path}): {k} public def" for name, k in counts.items() if ACTION_SUFFIX.search(name) and k <= 1]
    return out


def suffixed(name: str, suffixes: Iterable[str]) -> bool:
    found = words(name)
    return bool(found) and any(found[-1] == s.lower() for s in suffixes)


def naming_names(m: Module) -> List[Name]:
    """The names the naming rules read: a module's types, functions and modules -- not its constants, which the split
    credit reads as moved code (splits.py)."""
    return [n for n in m.names if n.kind != "constant"]


def rows(modules: Iterable[Module], rules: Rules) -> List[Row]:
    refused: Set[str] = rules.words_refused()
    out: List[Row] = []
    for m in modules:
        for n in naming_names(m):
            key = f"{m.path}:{n.name}"
            if n.kind in ("type", "module") and suffixed(n.name, rules.suffixes):
                out.append(("suffix", key, 1))
            if set(words(n.name)) & refused:
                out.append(("abbreviation", key, 1))
    return sorted(set(out))
