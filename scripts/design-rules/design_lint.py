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

  mixed      a FUNCTION that both performs an effect -- an injected one directly (model.INJECTED), or a call into an
             adapter module (`tool_git.Git.here()`, a resolved import of one) -- and decides: its cyclomatic
             complexity at the `mixed-cyclomatic` limit or past it (3: a loop holding a branch, two branches). Outside
             the adapter modules, whose functions ARE the leaves; and not a composition root (`main`, a page's
             `init`), which wires the adapters. Split it: the decision up (pure, the adapter its parameter), the IO
             down. A row `mixed <file>:<function>`, its value the function's cyclomatic complexity
             (the owner, 2026-11-27: "move side effects to leaves, with fake implementations for testing")

Each is a row per module (its count) or function, frozen in test/resources/design-baseline.tsv: what is there today
may only fall."""
from __future__ import annotations

from typing import Dict, FrozenSet, Iterable, List, Tuple

from model import INJECTED, INJECTED_BY_KIND, INJECTED_BY_USE, Function, Module
from rules_table import Rules

Row = Tuple[str, str, float]
COMPOSITION_ROOTS = ("main", "init")


def rows(modules: Iterable[Module], rules: Rules) -> List[Row]:
    out: List[Row] = []
    for m in modules:
        out += effect_rows(m, rules)
        if m.state:
            out.append(("state", m.path, len(m.state)))
        if m.import_io:
            out.append(("import-io", m.path, m.import_io))
        out += mixed_rows(m, rules)
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


def dotted_module(path: str) -> str:
    """`scripts/lib/tool_git.py` -> `scripts.lib.tool_git`: a module path and an import's dotted name compared alike."""
    stem = path.rsplit(".", 1)[0] if "/" in path or path.endswith((".py", ".js", ".ts")) else path
    return stem.replace("/", ".")


def into_adapter(call: str, adapters: FrozenSet[str]) -> bool:
    """Whether a call's dotted name (or resolved path) starts with an adapter module's name: its whole dotted path, or
    its trailing segments (`tool_git` for scripts/lib/tool_git.py, imported from beside it)."""
    parts = dotted_module(call).split(".")
    prefixes = {".".join(parts[:n]) for n in range(1, len(parts) + 1)}
    return any(a == p or a.endswith("." + p) for a in adapters for p in prefixes)


def effectful(f: Function, adapters: FrozenSet[str]) -> bool:
    return any(f.effects.get(kind) for kind in INJECTED) or any(into_adapter(c, adapters) for c in f.calls)


def adapter_names(rules: Rules, language: str) -> FrozenSet[str]:
    return frozenset(dotted_module(module) for lang, module in rules.adapters if lang == language)


def is_adapter(m: Module, rules: Rules) -> bool:
    return any(language == m.language and module == m.path for language, module in rules.adapters)


def mixed_rows(m: Module, rules: Rules) -> List[Row]:
    """The functions of `m` mixing an effect with decisions (`mixed`, above); none in an adapter module."""
    if is_adapter(m, rules):
        return []
    adapters = adapter_names(rules, m.language)
    limit = rules.limit("mixed-cyclomatic")
    found: Dict[str, int] = {}
    for f in m.functions:
        if f.name not in COMPOSITION_ROOTS and f.cyclomatic >= limit and effectful(f, adapters):
            key = f"{m.path}:{f.name}"
            found[key] = max(found.get(key, 0), f.cyclomatic)
    return [("mixed", key, n) for key, n in sorted(found.items())]
