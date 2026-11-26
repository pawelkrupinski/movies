"""DATA OR COLLABORATOR, for every language (CLAUDE.md, "Fan-out counts only COLLABORATORS"; the Scala reading is
project/Coupling.scala's `Collaborators`): a module is a COLLABORATOR when it touches the world (any effect), holds
state (module-level names written after load, or a type whose instances are), declares an interface (an abstract
type), holds a data type carrying logic past the DATA GUARD (more non-trivial methods or decisions than the table's
`data-methods` / `data-decisions` limits: logic may not hide in data), or uses a collaborator. Everything else is DATA
-- constants, data types and pure functions -- and is no coupling: fan-out counts collaborators only.

The data guard's two frozen measures, the same thresholds as Scala's: `databag` (a data type naming more types than
the limit) and `datafanout` (a module using more data modules than the limit)."""
from __future__ import annotations

from typing import Dict, List, Set, Tuple

from model import Module
from rules_table import Rules


def carries_logic(m: Module, rules: Rules) -> bool:
    return any(t.methods > rules.limit("data-methods") or t.decisions > rules.limit("data-decisions")
               for t in m.data_types)


def direct(m: Module, rules: Rules) -> bool:
    return bool(any(m.effects.values()) or m.state or m.stateful_types or m.abstract_types or carries_logic(m, rules))


def collaborators(modules: Dict[str, Module], rules: Rules) -> Set[str]:
    """The collaborator modules: the direct ones, and every module using one (to a fixed point)."""
    found = {p for p, m in modules.items() if direct(m, rules)}
    users: Dict[str, Set[str]] = {}
    for p, m in modules.items():
        for target in m.imports:
            users.setdefault(target, set()).add(p)
    queue = list(found)
    while queue:
        for user in users.get(queue.pop(), ()):
            if user not in found:
                found.add(user)
                queue.append(user)
    return found


def fanout(m: Module, collaborating: Set[str]) -> int:
    return len(m.imports & collaborating)


def rows(modules: Dict[str, Module], rules: Rules) -> List[Tuple[str, str, float]]:
    """The data guard's rows past their limits (`databag <path>:<Type>`, `datafanout <path>`)."""
    collaborating = collaborators(modules, rules)
    out: List[Tuple[str, str, float]] = []
    for p, m in sorted(modules.items()):
        out += [("databag", f"{p}:{t.name}", t.field_types) for t in m.data_types
                if t.field_types > rules.limit("databag")]
        data_used = len(m.imports - collaborating)
        if data_used > rules.limit("datafanout"):
            out.append(("datafanout", p, data_used))
    return out
