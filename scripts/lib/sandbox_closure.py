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
from typing import Iterable, List, Set

from tool_files import Files

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


def found(files: Files, root: str, importer: str, name: str) -> List[str]:
    """The repository files `name` can be imported from by `importer`: beside it, else in SEARCH."""
    here = os.path.dirname(importer)
    candidates = [f"{here}/{name}.py" if here else f"{name}.py", *(f"{d}/{name}.py" for d in SEARCH)]
    return [c for c in dict.fromkeys(candidates) if files.is_file(f"{root}/{c}") and not c.endswith("_test.py")]


def closure(files: Files, root: str, tools: Iterable[str]) -> List[str]:
    """`tools` (paths from the repository root) and every repository module their Python imports reach."""
    order: List[str] = []
    todo = list(tools)
    while todo:
        path = todo.pop(0)
        if path in order:
            continue
        order.append(path)
        if path.endswith(".py"):
            for name in sorted(imported_names(files.read_text(f"{root}/{path}"))):
                todo.extend(found(files, root, path, name)[:1])
    return order


def copy(files: Files, root: str, dest: str, tools: Iterable[str]) -> List[str]:
    copied = closure(files, root, tools)
    for path in copied:
        text = files.read_text(f"{root}/{path}")
        files.replace_text(f"{dest}/{path}", text, 0o755 if text.startswith("#!") else 0o644)
    return copied
