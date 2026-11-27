"""THE SPLIT CREDIT FOR IMPORTERS (the owner, 2026-11-27): a module X split into pieces -- its git functions moved into
a new leaf adapter, say -- leaves every importer that used the moved code importing X AND the piece, its fan-out one
higher though it depends on nothing new. A commit whose FINAL trailer block says `Coupling: split of <X> into <A>[,
<B>]` may raise an importer's `fanout` row by at most the number of pieces it newly imports, and only where it is
shown that the importer's use of each piece is code that moved there from X:

  - the importer imported X at the base, and its new collaborators are pieces only (a piece is new to it);
  - its rise, measured base to head, is at most the count of those pieces;
  - every name it reads from a piece is one X defined at the base, and one the importer read from X at the base.

Such a row is re-frozen at its new value (the commit's `update`d baseline) and falls as callers move to parameters.
Every other rise of the commit's Coupling kinds -- an unproven importer, a row of another kind -- is refused: the split
trailer excuses nothing else. One implementation over every language's modules (model.Module's `imports` and
`references`); the language's ratchet hands it the modules at the commit's parent and at the commit."""
from __future__ import annotations

import re
from typing import Dict, List, Optional, Set, Tuple

from design_lint import dotted_module
from model import Module

SPLIT = re.compile(r"^split of (\S+) into (.+)$")
Modules = Dict[str, Module]
Grown = Tuple[str, str, Optional[float], float]   # kind, key, the baseline's value before (None: no row), after


def parse(value: str) -> Optional[Tuple[str, List[str]]]:
    """(X, [pieces]) of a `split of X into A, B` trailer value; None for any other value."""
    m = SPLIT.match(value.strip())
    if not m:
        return None
    return m.group(1), [p.strip() for p in re.split(r",|\band\b", m.group(2)) if p.strip()]


def matches(name: str, path: str) -> bool:
    """Whether a module name (a path, `agentctl.core`, `core`) names the module at `path`."""
    dotted = dotted_module(path)
    wanted = dotted_module(name)
    return dotted == wanted or dotted.endswith("." + wanted)


def resolve(name: str, modules: Modules) -> Optional[str]:
    found = [p for p in modules if matches(name, p)]
    return found[0] if len(found) == 1 else None


def read_from(m: Module, target: str) -> Set[str]:
    """The names `m` reads through its imports of the module at `target`."""
    return {name for module, name in m.references if matches(module, target)}


def defined(m: Module) -> Set[str]:
    return {n.name for n in m.names if n.kind in ("function", "type")}


def credit(importer: str, x: str, pieces: List[str], sides: Tuple[Modules, Modules],
           collaborating: Tuple[Set[str], Set[str]], fanout: Tuple[int, int]) -> List[str]:
    """Why the importer's rise is not this split's to excuse; none when it is."""
    base, head = sides
    before, after = base.get(importer), head[importer]
    if before is None or x not in before.imports:
        return [f"{importer} did not import {x} at the base: its rise is no split's"]
    gained = (after.imports & collaborating[1]) - (before.imports & collaborating[0])
    out = [f"{importer} gained {other}, no piece of {x}" for other in sorted(gained - set(pieces))]
    if fanout[1] - fanout[0] > len(gained & set(pieces)):
        out.append(f"{importer} rose {fanout[0]} -> {fanout[1]}, more than the {len(gained & set(pieces))} piece(s) it "
                   "now imports")
    moved: Set[str] = defined(base[x]) if x in base else set()
    for piece in sorted(gained & set(pieces)):
        foreign = read_from(after, piece) - (moved & read_from(before, x))
        out += [f"{importer} reads {name} from {piece}: not code it read from {x} at the base" for name in sorted(foreign)]
    return out


def refusals(value: str, grown: List[Grown], sides: Tuple[Modules, Modules],
             collaborating: Tuple[Set[str], Set[str]], fanouts: Tuple[Dict[str, int], Dict[str, int]]) -> List[str]:
    """Every rise of a `split of` commit (`grown`: its own baseline step) this split does not excuse."""
    split = parse(value)
    if split is None:
        return [f"`Coupling: {value}` is no `split of <X> into <A>[, <B>]`"]
    x = resolve(split[0], sides[0])
    pieces = [resolve(p, sides[1]) for p in split[1]]
    if x is None or None in pieces:
        return [f"`split of {split[0]} into {', '.join(split[1])}`: each must name one measured module"]
    out: List[str] = []
    for kind, key, _old, _new in grown:
        if kind != "fanout":
            out.append(f"{kind} {key}: a split excuses importers' fan-out only")
            continue
        found = [p for p in pieces if p is not None]
        out += credit(key, x, found, sides, collaborating,
                      (fanouts[0].get(key, 0), fanouts[1].get(key, 0)))
    return out
