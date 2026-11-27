"""THE DESIGN LINT, for Python and JS/TS (CLAUDE.md, "Design rules": Dependency Inversion; the injected clock; no
stateful singletons; a tool's `main()` / a page's `init()` is its composition root):

  effect     a module touching an INJECTED effect (model.INJECTED) directly that no `adapter` row of the design rules
             table names for that effect: it should receive it (a clock, a runner, the settings, tool_files' Files,
             tool_state's stores, tool_http's Http). The clock, a process and the environment are one `effect <file>`
             row, its value how many of the three; files, storage and the network an `effect <file>:<kind>` row each,
             its value how many direct uses -- so a module migrated as it is visited falls use by use, and a new use
             of one outside its adapter fails
  state      a module whose functions write its module-level names (a stateful singleton): hold the state in an object
             the root creates and passes on (web JS: the one page-state object `init()` makes)
  import-io  effects run when the module is LOADED, outside any function and the main guard: module-level code does no
             IO, the root does

Each is a row per module (its count), frozen in test/resources/design-baseline.tsv: what is there today may only fall."""
from __future__ import annotations

from typing import Iterable, List, Tuple

from model import INJECTED_BY_KIND, INJECTED_BY_USE, Module
from rules_table import Rules

Row = Tuple[str, str, float]


def rows(modules: Iterable[Module], rules: Rules) -> List[Row]:
    out: List[Row] = []
    for m in modules:
        out += effect_rows(m, rules)
        if m.state:
            out.append(("state", m.path, len(m.state)))
        if m.import_io:
            out.append(("import-io", m.path, m.import_io))
    return out


def effect_rows(m: Module, rules: Rules) -> List[Row]:
    """The injected effects it touches outside its adapter rows."""
    allowed = rules.adapter_effects(m.language, m.path)
    direct = {kind: n for kind, n in m.effects.items() if n and kind not in allowed}
    touched = [kind for kind in INJECTED_BY_KIND if kind in direct]
    out: List[Row] = []
    if touched:  # how many of the three it touches, not how often: one more call is no new dependency
        out.append(("effect", m.path, len(touched)))
    out += [("effect", f"{m.path}:{kind}", direct[kind]) for kind in INJECTED_BY_USE if kind in direct]
    return out
