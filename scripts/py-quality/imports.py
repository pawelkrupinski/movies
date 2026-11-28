"""THE PYTHON MODULE GRAPH (scripts/py-quality/report.py): which of the measured files each one imports, as
run-script-tests resolves an import -- the importer's own directory first, then the shared library (scripts/lib), then
any measured file of that name -- and the standard library or a third-party package otherwise (no edge). A dotted
import names a module of a package directory below those (`from vcs import tool_github`, `from vcs.tool_github import
Gh`, `import vcs.tool_github`: scripts/lib/vcs/tool_github.py).

  fan-out  the measured modules a file imports (each once): its collaborators' modules, as the JS ratchet counts them
  cycles   the strongly connected groups of two or more files that import one another"""
from __future__ import annotations

import ast
import os
from typing import Dict, List, Optional, Set

LIBRARY = "scripts/lib"


def statement_names(node: ast.AST) -> List[str]:
    """What one statement imports, dotted as written (`import a.b`: a.b; `from c.d import e`: c.d and c.d.e, the name a
    module or a member of one); a relative import none."""
    if isinstance(node, ast.Import):
        return [alias.name for alias in node.names]
    if isinstance(node, ast.ImportFrom) and node.module and not node.level:
        return [node.module, *(f"{node.module}.{alias.name}" for alias in node.names if alias.name != "*")]
    return []


def imported_names(tree: ast.AST) -> Set[str]:
    """The module names `tree` imports (statement_names), __future__ left out."""
    return {n for node in ast.walk(tree) for n in statement_names(node) if n.split(".")[0] != "__future__"}


def resolve(importer: str, name: str, by_name: Dict[str, List[str]]) -> Optional[str]:
    """The measured file a (dotted) module name is: `a.b.c` as a/b/c.py beside the importer or in the library, else
    its package `a.b`'s; a single name also as any one measured file of that name."""
    if "." in name:
        relative = name.replace(".", "/") + ".py"
        candidates = by_name.get(os.path.basename(relative), [])
        for preferred in (os.path.join(os.path.dirname(importer), relative), os.path.join(LIBRARY, relative)):
            if preferred in candidates:
                return preferred
        return None
    own = os.path.join(os.path.dirname(importer), name + ".py")
    candidates = by_name.get(name + ".py", [])
    for preferred in (own, os.path.join(LIBRARY, name + ".py")):
        if preferred in candidates:
            return preferred
    return candidates[0] if len(candidates) == 1 else None


def graph(trees: Dict[str, ast.AST]) -> Dict[str, Set[str]]:
    """file -> the measured files it imports."""
    by_name: Dict[str, List[str]] = {}
    for path in trees:
        by_name.setdefault(os.path.basename(path), []).append(path)
    edges: Dict[str, Set[str]] = {}
    for path, tree in trees.items():
        targets = {resolve(path, name, by_name) for name in imported_names(tree)}
        edges[path] = {t for t in targets if t is not None and t != path}
    return edges


def cycles(edges: Dict[str, Set[str]]) -> List[List[str]]:
    """Tarjan's strongly connected components of two or more members, each sorted."""
    index: Dict[str, int] = {}
    low: Dict[str, int] = {}
    stack: List[str] = []
    found: List[List[str]] = []

    def visit(v: str) -> None:
        index[v] = low[v] = len(index)
        stack.append(v)
        for w in sorted(edges.get(v, ())):
            if w not in index:
                visit(w)
                low[v] = min(low[v], low[w])
            elif w in stack:
                low[v] = min(low[v], index[w])
        if low[v] == index[v]:
            members: List[str] = []
            while not members or members[-1] != v:
                members.append(stack.pop())
            if len(members) > 1:
                found.append(sorted(members))

    for v in sorted(edges):
        if v not in index:
            visit(v)
    return sorted(found)
