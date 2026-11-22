#!/usr/bin/env python3
# script-test-reads: .github/workflows/* .github/scripts/* scripts/*
"""EVERY SPARSE CHECKOUT HOLDS WHAT ITS JOB'S PYTHON IMPORTS. A workflow job that checks out only some paths
(`sparse-checkout: |`) and runs a repository Python tool -- named in its steps, directly or through a shell file that
execs it (`exec python3 "$(dirname "$0")/x.py"`) -- must check out every repository module that tool imports,
transitively: Identity model convergence #1390 went red when dispatch-takes.sh became an exec of dispatch_takes.py,
which imports scripts/lib/tool_dispatch, and the preflight checked out only scripts/ci and .github
(ModuleNotFoundError, no takes run). A module is looked up where the tools put it on sys.path: the importing file's own
directory, scripts/lib and scripts/ci."""
from __future__ import annotations

import ast
import os
import re
import unittest
from typing import Dict, List, Set, Tuple

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
WORKFLOWS = os.path.join(ROOT, ".github", "workflows")
SEARCHED = ("scripts/lib", "scripts/ci")
NAMED = re.compile(r"(?<![\w./-])((?:scripts|\.github/scripts)/[\w./-]*[\w])")
EXEC = re.compile(r'exec python3 (?:-\w+ )*"\$\(dirname "\$0"\)/([\w.]+\.py)"')


def jobs(workflow: str) -> Dict[str, str]:
    """Each job's text (its comment lines left out), by name: the 4-space-indented keys under `jobs:`."""
    with open(workflow) as f:
        lines = [line for line in f.read().split("\n") if not line.strip().startswith("#")]
    out: Dict[str, str] = {}
    name, body, inside = "", [], False
    for line in lines:
        if line.startswith("jobs:"):
            inside = True
        elif inside and re.match(r"^    [\w-]+:\s*$", line):
            out[name] = "\n".join(body)
            name, body = line.strip()[:-1], []
        elif inside:
            body.append(line)
    out[name] = "\n".join(body)
    out.pop("", None)
    return out


def sparse_paths(job: str) -> List[str]:
    found = re.search(r"sparse-checkout: \|\n((?: +[^\s:]+\n?)+)", job)
    return [p.strip().rstrip("/") for p in found.group(1).split("\n") if p.strip()] if found else []


def python_of(path: str) -> str:
    """The Python file `path` is or execs into, else ""."""
    full = os.path.join(ROOT, path)
    if path.endswith(".py"):
        return path
    try:
        with open(full) as f:
            text = f.read(4000)
    except (OSError, UnicodeDecodeError):
        return ""
    if text.startswith("#!") and "python" in text.split("\n", 1)[0]:
        return path
    found = EXEC.search(text)
    return os.path.join(os.path.dirname(path), found.group(1)) if found else ""


def imported(path: str) -> Set[str]:
    """The repository modules `path` imports, as repository paths."""
    with open(os.path.join(ROOT, path)) as f:
        tree = ast.parse(f.read())
    names: Set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            names |= {alias.name.split(".")[0] for alias in node.names}
        elif isinstance(node, ast.ImportFrom) and node.module and not node.level:
            names.add(node.module.split(".")[0])
    out: Set[str] = set()
    for name in names:
        for directory in (os.path.dirname(path), *SEARCHED):
            candidate = os.path.join(directory, f"{name}.py")
            if os.path.isfile(os.path.join(ROOT, candidate)):
                out.add(candidate)
                break
    return out


def closure(tool: str) -> Set[str]:
    seen, todo = {tool}, [tool]
    while todo:
        for module in imported(todo.pop()) - seen:
            seen.add(module)
            todo.append(module)
    return seen


def covered(path: str, sparse: List[str]) -> bool:
    return any(path == p or path.startswith(p + "/") for p in sparse) or "/" not in path


def uncovered() -> List[Tuple[str, str, str]]:
    """(workflow job, tool, module) for each module a sparse job's tool imports that its checkout leaves out."""
    misses: List[Tuple[str, str, str]] = []
    for workflow in sorted(os.listdir(WORKFLOWS)):
        for job, text in jobs(os.path.join(WORKFLOWS, workflow)).items():
            sparse = sparse_paths(text)
            tools = {python_of(p) for p in NAMED.findall(text) if os.path.isfile(os.path.join(ROOT, p))} - {""}
            misses += [(f"{workflow} {job}", tool, module) for tool in sorted(tools) if sparse
                       for module in sorted(closure(tool)) if not covered(module, sparse)]
    return misses


class SparseCheckoutImportsTest(unittest.TestCase):
    def test_every_sparse_job_checks_out_what_its_python_tools_import(self) -> None:
        self.assertEqual([], uncovered())

    def test_it_reads_the_jobs_it_must(self) -> None:
        convergence = jobs(os.path.join(WORKFLOWS, "identity-model-convergence.yml"))
        self.assertIn("scripts/ci", sparse_paths(convergence["preflight"]))
        self.assertEqual("scripts/ci/dispatch_takes.py", python_of("scripts/ci/dispatch-takes.sh"))
        self.assertIn("scripts/lib/tool_dispatch.py", closure("scripts/ci/dispatch_takes.py"))
        self.assertFalse(covered("scripts/lib/tool_dispatch.py", ["scripts/ci", ".github"]))


if __name__ == "__main__":
    unittest.main()
