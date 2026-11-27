"""THE PYTHON EXTRACTOR of the general rule core: a Python tool's `ast` read into a `model.Module` -- its names, the
effects it touches (by the dotted name each call or attribute resolves to through the module's imports), the effects
run at import, the module-level state functions write, its stateful, abstract and data types. The import graph is
scripts/py-quality/imports.py's (the quality report's own)."""
from __future__ import annotations

import ast
import os
from typing import Dict, FrozenSet, List, Optional, Set, Tuple

from model import DataType, Module, Name

# dotted-name prefixes of each effect: `p.` matches what starts with it, `p` that name or its members
EFFECT_NAMES: Dict[str, Tuple[str, ...]] = {
    "process": ("subprocess", "os.system", "os.popen", "os.execv", "os.execvp", "os.execvpe", "os.spawnv", "os.fork",
                "os.kill", "asyncio.create_subprocess_exec", "asyncio.create_subprocess_shell"),
    "env": ("os.environ", "os.getenv", "os.putenv", "os.unsetenv"),
    "clock": ("time.time", "time.time_ns", "time.monotonic", "time.perf_counter", "time.sleep", "time.localtime",
              "time.gmtime", "time.strftime", "datetime.datetime.now", "datetime.datetime.utcnow",
              "datetime.datetime.today", "datetime.date.today"),
    "network": ("urllib.request", "http.client", "socket", "ssl", "requests", "smtplib"),
    "files": ("open", "os.remove", "os.unlink", "os.rename", "os.replace", "os.makedirs", "os.mkdir", "os.listdir",
              "os.walk", "os.scandir", "os.rmdir", "os.chdir", "os.stat", "os.path.exists", "os.path.isfile",
              "os.path.isdir", "os.path.getmtime", "shutil", "tempfile", "glob"),
    "storage": ("sqlite3.connect", "dbm.open", "shelve.open", "pymongo.MongoClient"),
    "random": ("random", "secrets", "uuid.uuid4", "os.urandom"),
    "threads": ("threading", "concurrent.futures", "multiprocessing", "signal"),
    "logging": ("logging",),
}
AT_IMPORT = ("process", "env", "clock", "network", "files", "storage", "random")
MUTATORS = frozenset({"append", "extend", "insert", "remove", "pop", "clear", "update", "setdefault", "add", "discard",
                      "popitem", "sort", "reverse"})
DATA_BASES = frozenset({"NamedTuple", "Enum", "IntEnum", "StrEnum", "Flag", "IntFlag", "TypedDict"})
ABSTRACT_BASES = frozenset({"Protocol", "ABC"})
# a `match` case where the parser has one (3.10+; macOS's python3 is 3.9)
DECISIONS: Tuple[type, ...] = (ast.If, ast.IfExp, ast.For, ast.AsyncFor, ast.While, ast.ExceptHandler, ast.comprehension,
                              *((ast.match_case,) if hasattr(ast, "match_case") else ()))


def effect_of(dotted: str) -> Optional[str]:
    for kind, prefixes in EFFECT_NAMES.items():
        if any(dotted == p or dotted.startswith(p + ".") for p in prefixes):
            return kind
    return None


def alias_of(node: ast.stmt) -> Dict[str, str]:
    if isinstance(node, ast.Import):
        return {(a.asname or a.name).split(".")[0]: a.name if a.asname else a.name.split(".")[0] for a in node.names}
    if isinstance(node, ast.ImportFrom) and node.module and not node.level:
        return {a.asname or a.name: f"{node.module}.{a.name}" for a in node.names}
    return {}


def aliases(tree: ast.Module) -> Dict[str, str]:
    """local name -> the dotted name it stands for (`import subprocess as sp`, `from os import environ`)."""
    out: Dict[str, str] = {}
    for node in ast.walk(tree):
        if isinstance(node, ast.stmt):
            out.update(alias_of(node))
    return out


def dotted(node: ast.expr, names: Dict[str, str]) -> Optional[str]:
    parts: List[str] = []
    while isinstance(node, ast.Attribute):
        parts.append(node.attr)
        node = node.value
    if not isinstance(node, ast.Name):
        return None
    head = names.get(node.id, node.id)
    return ".".join([head, *reversed(parts)])


def chain_tops(tree: ast.AST) -> List[ast.expr]:
    """Every Name or Attribute that is no other Attribute's object: one per dotted reference."""
    inner: Set[int] = {id(n.value) for n in ast.walk(tree) if isinstance(n, ast.Attribute)}
    return [n for n in ast.walk(tree) if isinstance(n, (ast.Name, ast.Attribute)) and id(n) not in inner]


def effect_at(node: ast.expr, names: Dict[str, str], called: Set[int]) -> Optional[str]:
    """The effect one reference is: a CALL of an effect's name (a reference alone -- `subprocess.CalledProcessError`
    in an `except`, `subprocess.PIPE` -- runs nothing), or any read of the environment; an imported `open` is not the
    builtin."""
    if isinstance(node, ast.Name) and node.id == "open" and "open" in names:
        return None
    name = dotted(node, names)
    kind = effect_of(name) if name else None
    return kind if kind and (id(node) in called or kind == "env") else None


def effects(tree: ast.AST, names: Dict[str, str]) -> Dict[str, int]:
    called: Set[int] = {id(n.func) for n in ast.walk(tree) if isinstance(n, ast.Call)}
    out: Dict[str, int] = {}
    for node in chain_tops(tree):
        kind = effect_at(node, names, called)
        if kind:
            out[kind] = out.get(kind, 0) + 1
    return out


def is_main_guard(node: ast.stmt) -> bool:
    return (isinstance(node, ast.If) and isinstance(node.test, ast.Compare) and isinstance(node.test.left, ast.Name)
            and node.test.left.id == "__name__")


DEFINITIONS = (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Import, ast.ImportFrom)


def import_io(tree: ast.Module, names: Dict[str, str]) -> int:
    """The effects a module's own statements run when it is loaded: none of a def, a class body's methods or the
    `if __name__ == "__main__":` block (the composition root's)."""
    count = 0
    for stmt in tree.body:
        if isinstance(stmt, DEFINITIONS) or is_main_guard(stmt):
            continue
        count += sum(n for kind, n in effects(stmt, names).items() if kind in AT_IMPORT)
    return count


def module_names(tree: ast.Module) -> Set[str]:
    out: Set[str] = set()
    for stmt in tree.body:
        targets: List[ast.expr] = list(stmt.targets) if isinstance(stmt, ast.Assign) else (
            [stmt.target] if isinstance(stmt, (ast.AnnAssign, ast.AugAssign)) else [])
        out.update(t.id for t in targets if isinstance(t, ast.Name))
    return out


def local_names(fn: ast.AST) -> Set[str]:
    """What a function binds itself (its parameters, its assignments): a module name it shadows is not the module's."""
    out: Set[str] = set()
    if isinstance(fn, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda)):
        args = fn.args
        out.update(a.arg for a in [*args.posonlyargs, *args.args, *args.kwonlyargs])
    for n in ast.walk(fn):
        if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Store):
            out.add(n.id)
    return out


def write_target(n: ast.AST) -> Optional[ast.expr]:
    """What one node writes in place: a mutator call's object, a stored or deleted subscript's or attribute's."""
    if isinstance(n, ast.Call) and isinstance(n.func, ast.Attribute) and n.func.attr in MUTATORS:
        return n.func.value
    if isinstance(n, (ast.Subscript, ast.Attribute)) and isinstance(n.ctx, (ast.Store, ast.Del)):
        return n.value
    return None


def written(fn: ast.AST, module: Set[str]) -> Set[str]:
    """The module-level names one function writes: rebinds through `global`, or mutates in place."""
    declared = {g for n in ast.walk(fn) if isinstance(n, ast.Global) for g in n.names}
    visible = module - (local_names(fn) - declared)
    targets = (write_target(n) for n in ast.walk(fn))
    return (declared & module) | {t.id for t in targets if isinstance(t, ast.Name) and t.id in visible}


def state(tree: ast.Module) -> Tuple[str, ...]:
    module = module_names(tree)
    out: Set[str] = set()
    for fn in ast.walk(tree):
        if isinstance(fn, (ast.FunctionDef, ast.AsyncFunctionDef)):
            out |= written(fn, module)
    return tuple(sorted(out))


def base_names(cls: ast.ClassDef) -> Set[str]:
    out: Set[str] = set()
    for b in [*cls.bases, *(k.value for k in cls.keywords)]:
        name = b.attr if isinstance(b, ast.Attribute) else b.id if isinstance(b, ast.Name) else ""
        out.add("ABC" if name == "ABCMeta" else name)
    return out


def is_dataclass(cls: ast.ClassDef) -> bool:
    for d in cls.decorator_list:
        target = d.func if isinstance(d, ast.Call) else d
        name = target.attr if isinstance(target, ast.Attribute) else target.id if isinstance(target, ast.Name) else ""
        if name == "dataclass":
            return True
    return bool(base_names(cls) & DATA_BASES)


def methods(cls: ast.ClassDef) -> List[ast.FunctionDef]:
    return [s for s in cls.body if isinstance(s, ast.FunctionDef)]


def non_trivial(fn: ast.FunctionDef) -> bool:
    body = [s for s in fn.body if not (isinstance(s, ast.Expr) and isinstance(s.value, ast.Constant))]
    return len(body) > 1 or decisions(fn) > 0


def decisions(node: ast.AST) -> int:
    return sum(1 for n in ast.walk(node) if isinstance(n, DECISIONS))


def field_types(cls: ast.ClassDef) -> int:
    """The capitalised names its field annotations hold: the types a data type names."""
    annotations = [s.annotation for s in cls.body if isinstance(s, ast.AnnAssign)]
    return len({n.id for a in annotations for n in ast.walk(a) if isinstance(n, ast.Name) and n.id[:1].isupper()})


def data_type(cls: ast.ClassDef) -> DataType:
    fns = methods(cls)
    return DataType(cls.name, sum(1 for fn in fns if non_trivial(fn)), sum(decisions(fn) for fn in fns), field_types(cls))


def is_stateful(cls: ast.ClassDef) -> bool:
    """Its instances are written after construction: a `self.x =` (or in-place change) outside __init__."""
    for fn in methods(cls):
        if fn.name in ("__init__", "__post_init__", "__new__"):
            continue
        for n in ast.walk(fn):
            if isinstance(n, (ast.Attribute, ast.Subscript)) and isinstance(n.ctx, ast.Store) \
                    and isinstance(n.value, ast.Name) and n.value.id == "self":
                return True
    return False


def classes(tree: ast.Module) -> List[ast.ClassDef]:
    return [s for s in tree.body if isinstance(s, ast.ClassDef)]


def names(path: str, tree: ast.Module) -> Tuple[Name, ...]:
    stem = os.path.basename(path)
    out = [Name("module", stem[:-3] if stem.endswith(".py") else stem, 0, 1)]
    for s in tree.body:
        if isinstance(s, ast.ClassDef):
            public = sum(1 for fn in methods(s) if not fn.name.startswith("_"))
            out.append(Name("type", s.name, public, s.lineno))
        elif isinstance(s, (ast.FunctionDef, ast.AsyncFunctionDef)):
            out.append(Name("function", s.name, 0, s.lineno))
    return tuple(out)


def module(path: str, tree: ast.Module, imports: FrozenSet[str]) -> Module:
    local = aliases(tree)
    types = classes(tree)
    plain = [c for c in types if not is_dataclass(c)]
    return Module(
        language="py", path=path, package=os.path.dirname(path), names=names(path, tree),
        effects=effects(tree, local), import_io=import_io(tree, local), state=state(tree),
        stateful_types=tuple(c.name for c in plain if is_stateful(c)),
        abstract_types=sum(1 for c in plain if base_names(c) & ABSTRACT_BASES),
        data_types=tuple(data_type(c) for c in types if is_dataclass(c)), imports=imports)

