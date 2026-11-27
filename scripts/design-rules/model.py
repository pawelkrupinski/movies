"""THE LANGUAGE-AGNOSTIC MODEL the general rule core reads (scripts/design-rules/): every language's extractor
(python_facts.py from `ast`, scripts/js-quality/facts.js from the JS/TS parsers, scala_names.py from the Scala
sources) turns a source file into one `Module`, and every rule (naming.py, collaborators.py, packages.py,
design_lint.py) reads only these -- so a rule is written once, for every language."""
from __future__ import annotations

import dataclasses
from typing import Dict, FrozenSet, Tuple

# The effects an extractor names: what reads or changes the world (CLAUDE.md, "Fan-out counts only COLLABORATORS": the
# clock, environment, files, network, database, randomness, threads, logging, a process). The INJECTED ones -- the
# clock, a process, the environment and (the owner, 2026-11-27) files, storage (a database, a key-value store) and the
# network -- only an `adapter` row's module may touch directly; every other module receives them (design_lint.py).
EFFECTS = ("clock", "process", "env", "network", "files", "storage", "random", "threads", "logging", "dom")
INJECTED_BY_KIND = ("clock", "process", "env")        # one `effect <file>` row: how many of these it touches
INJECTED_BY_USE = ("files", "storage", "network")     # an `effect <file>:<kind>` row each: how many direct uses
INJECTED = INJECTED_BY_KIND + INJECTED_BY_USE


@dataclasses.dataclass(frozen=True)
class Name:
    kind: str          # "type", "module", "function" or (Python) "constant": a module-level name bound once
    name: str
    public_defs: int   # a type's public methods (the action-name rule reads it); 0 otherwise
    line: int


@dataclasses.dataclass(frozen=True)
class DataType:
    """A data type (a dataclass, NamedTuple, Enum; a Scala case class or object of constants): its non-trivial methods,
    its decisions and the types its fields name -- the data guard's inputs."""
    name: str
    methods: int
    decisions: int
    field_types: int


@dataclasses.dataclass(frozen=True)
class Function:
    """One function or method (nested ones apart, lambdas and anonymous callbacks folded into the function holding
    them): the effects its own body performs directly, the calls it makes through an import (a dotted name for Python,
    `tool_git.Git.here`; a resolved module path for JS/TS) and its cyclomatic complexity -- the `mixed` rule's inputs."""
    name: str                        # qualified: `Class.method`, `outer.inner`
    line: int
    effects: Dict[str, int] = dataclasses.field(default_factory=lambda: {})  # kind -> occurrences in its own body
    calls: FrozenSet[str] = frozenset()
    cyclomatic: int = 1


@dataclasses.dataclass(frozen=True)
class Module:
    language: str                    # "scala", "py" or "js"
    path: str                        # relative to the checkout
    package: str                     # its package: a Scala package, a Python or JS directory
    names: Tuple[Name, ...] = ()
    effects: Dict[str, int] = dataclasses.field(default_factory=lambda: {})  # kind -> occurrences, anywhere in the module
    import_io: int = 0               # effects run when the module is loaded, outside any function or main guard
    state: Tuple[str, ...] = ()      # module-level names written after load (a stateful singleton)
    stateful_types: Tuple[str, ...] = ()  # types whose instances are written after construction
    abstract_types: int = 0          # Protocol / ABC / interface types it declares
    data_types: Tuple[DataType, ...] = ()
    imports: FrozenSet[str] = frozenset()  # the measured modules (paths) it uses
    new: bool = False                # added by the ranges checked (the action-name rule warns on new ones only)
    functions: Tuple[Function, ...] = ()  # every function's own effects, calls and decisions (the `mixed` rule)
    # (imported module, name) of every name it reads through an import: the module a dotted import name (Python:
    # each prefix of `a.b.f`, matched to a file by splits.py) or a resolved path (JS/TS) -- the split credit's evidence
    references: FrozenSet[Tuple[str, str]] = frozenset()
