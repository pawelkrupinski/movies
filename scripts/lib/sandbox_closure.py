"""THE SANDBOX CLOSURE (scripts/lib): a self-test that runs a tool from a scratch tree copies the tool AND everything it
imports -- the `scripts/lib` adapters (`tool_git`, `tool_process`, `tool_files`...) a tool takes since the design rules
injected its effects -- never the tool alone. The closure is read from the tools' own `import` lines (`ast`), through
`Files`, so a new import is copied by itself and a test lists only the tools it runs.

  closure(files, root, tools) -> [relative path]     the tools and every repository module they import, transitively
  copy(files, root, dest, tools) -> [relative path]  the same files written under `dest` at the same relative paths,
                                                     a script with a shebang executable

A module is found as `<name>.py` beside the importer or in the directories every tool imports from (SEARCH); a name
that is no file of the repository (the standard library, a third party) is no dependency. A test file is never one."""
from __future__ import annotations

import ast
import os
from typing import Callable, Iterable, List, Protocol, Set


class Tree(Protocol):
    """What the closure reads of a checkout: tool_files.Files, or an in-memory one."""

    def read_text(self, path: str) -> str: ...

    def is_file(self, path: str) -> bool: ...

    def copy_script(self, source: str, target: str) -> None: ...

SEARCH = ("scripts/lib", "scripts/ci", "scripts", "scripts/design-rules")


def imported_names(source: str) -> Set[str]:
    """The top-level module names a Python source imports (`import a.b`: a; `from c import d`: c)."""
    names: Set[str] = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Import):
            names.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and node.level == 0:
            names.add(node.module.split(".")[0])
    return names


def found(is_file: Callable[[str], bool], importer: str, name: str) -> List[str]:
    """The repository files `name` can be imported from by `importer`: beside it, else in SEARCH."""
    here = os.path.dirname(importer)
    candidates = [f"{here}/{name}.py" if here else f"{name}.py", *(f"{d}/{name}.py" for d in SEARCH)]
    return [c for c in dict.fromkeys(candidates) if is_file(c) and not c.endswith("_test.py")]


def closure(read: Callable[[str], str], is_file: Callable[[str], bool], tools: Iterable[str]) -> List[str]:
    """`tools` (paths from the repository root) and every repository module their Python imports reach."""
    order: List[str] = []
    todo = list(tools)
    while todo:
        path = todo.pop(0)
        if path in order:
            continue
        order.append(path)
        if path.endswith(".py"):
            for name in sorted(imported_names(read(path))):
                todo.extend(found(is_file, path, name)[:1])
    return order


def copy(files: Tree, root: str, dest: str, tools: Iterable[str]) -> List[str]:
    """The closure of `tools` in the checkout at `root`, each file copied under `dest` at the same relative path."""
    copied = closure(lambda p: files.read_text(f"{root}/{p}"), lambda p: files.is_file(f"{root}/{p}"), tools)
    for path in copied:
        files.copy_script(f"{root}/{path}", f"{dest}/{path}")
    return copied
