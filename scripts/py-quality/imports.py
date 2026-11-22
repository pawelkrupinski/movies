"""THE PYTHON MODULE GRAPH (scripts/py-quality/report.py): which of the measured files each one imports, as
run-script-tests resolves an import -- the importer's own directory first, then the shared library (scripts/lib), then
any measured file of that name -- and the standard library or a third-party package otherwise (no edge).

  fan-out  the measured modules a file imports (each once): its collaborators' modules, as the JS ratchet counts them
  cycles   the strongly connected groups of two or more files that import one another"""
from __future__ import annotations

import ast
import os
from typing import Dict, List, Optional, Set

LIBRARY = "scripts/lib"


def imported_names(tree: ast.AST) -> Set[str]:
    """The top-level module names `tree` imports (`a.b` is `a`), relative imports and __future__ left out."""
    names: Set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            names.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and not node.level:
            names.add(node.module.split(".")[0])
    names.discard("__future__")
    return names


def resolve(importer: str, name: str, by_name: Dict[str, List[str]]) -> Optional[str]:
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
