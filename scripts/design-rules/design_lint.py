"""THE DESIGN LINT, for Python and JS/TS (CLAUDE.md, "Design rules": Dependency Inversion; the injected clock; no
stateful singletons; a tool's `main()` / a page's `init()` is its composition root):

  effect     a module touching the clock, a process or the environment directly (its value: how many of the three) (model.INJECTED) that no `adapter` row
             of the design rules table names for that effect: it should receive them (a clock, a runner, the settings)
  state      a module whose functions write its module-level names (a stateful singleton): hold the state in an object
             the root creates and passes on (web JS: the one page-state object `init()` makes)
  import-io  effects run when the module is LOADED, outside any function and the main guard: module-level code does no
             IO, the root does

Each is a row per module (its count), frozen in test/resources/design-baseline.tsv: what is there today may only fall."""
from __future__ import annotations

from typing import Iterable, List, Tuple

from model import INJECTED, Module
from rules_table import Rules

Row = Tuple[str, str, float]


def rows(modules: Iterable[Module], rules: Rules) -> List[Row]:
    out: List[Row] = []
    for m in modules:
        allowed = rules.adapter_effects(m.language, m.path)
        direct = {kind for kind, n in m.effects.items() if n and kind in INJECTED and kind not in allowed}
        if direct:  # how many of the three it touches, not how often: one more call is no new dependency
            out.append(("effect", m.path, len(direct)))
        if m.state:
            out.append(("state", m.path, len(m.state)))
        if m.import_io:
            out.append(("import-io", m.path, m.import_io))
    return out
