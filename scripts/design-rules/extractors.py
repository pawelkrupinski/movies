"""EVERY LANGUAGE'S MODULES, for the general rule core: the Python tools (python_facts.py over the quality report's
own file list and import graph), the production JS/TS (scripts/js-quality-report's modules.json, facts.js) and the
production Scala sources (scala_names.py). Self-tests are left out: they are reported, never held."""
from __future__ import annotations

import ast
import dataclasses
import json
import os
import sys
import tempfile
from typing import Any, Dict, FrozenSet, List, Set

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "py-quality"))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import imports  # noqa: E402
import python_facts  # noqa: E402
import report as py_quality  # noqa: E402  (scripts/py-quality/report.py: the measured files and their parse)
import scala_names  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
from model import Function, Module, Name  # noqa: E402

JS_REPORT = os.path.join(os.path.dirname(HERE), "js-quality-report")
JS_AREAS = os.path.join(os.path.dirname(HERE), "js-quality", "ratchet-areas.tsv")
JS_ROOTS = ("web/src/main/assets/js", "infra/version-dashboard/src")  # ratchet-areas.tsv's paths
Modules = Dict[str, Module]


def held(path: str) -> bool:
    return not path.endswith(("_test.py", ".test.js", ".test.ts", ".spec.ts"))


def python_modules(root: str, added: Set[str]) -> Modules:
    sources = py_quality.read_sources(root, [p for p in py_quality.python_files(root) if held(p)])
    trees = py_quality.parse_all(sources)
    edges = imports.graph(trees)
    out: Modules = {}
    for path, tree in trees.items():
        if isinstance(tree, ast.Module):
            m = python_facts.module(path, tree, frozenset(edges.get(path, set())))
            out[path] = dataclasses.replace(m, new=path in added)
    return out


def js_module(row: Dict[str, Any], known: FrozenSet[str], added: Set[str]) -> Module:
    path = str(row["path"])
    return Module(
        language="js", path=path, package=os.path.dirname(path),
        names=tuple(Name(str(n["kind"]), str(n["name"]), int(n["publicDefs"]), int(n["line"])) for n in row["names"]),
        effects={str(k): int(v) for k, v in row["effects"].items()}, import_io=int(row["importIo"]),
        state=tuple(row["state"]), stateful_types=tuple(row["statefulTypes"]),
        abstract_types=int(row["abstractTypes"]),
        imports=frozenset(t for t in row["imports"] if t in known), new=path in added,
        functions=tuple(Function(str(f["name"]), int(f["line"]), {str(k): int(v) for k, v in f["effects"].items()},
                                 frozenset(str(c) for c in f["calls"]), int(f["cyclomatic"]))
                        for f in row.get("functions", ())),
        references=frozenset((str(m), str(n)) for m, n in row.get("references", ())))


def read_js(modules_json: str, added: Set[str]) -> Modules:
    with open(modules_json, encoding="utf-8") as f:
        rows: List[Dict[str, Any]] = json.load(f)
    rows = [r for r in rows if held(str(r["path"]))]
    known = frozenset(str(r["path"]) for r in rows)
    return {str(r["path"]): js_module(r, known, added) for r in rows}


def js_modules(git: tool_git.Git, added: Set[str], modules_json: str = "") -> Modules:
    """The production JS/TS's modules: from `modules_json` when the JS report already wrote it, else measured now
    (scripts/js-quality-report, through the root's runner)."""
    if modules_json:
        return read_js(modules_json, added)
    if not any(os.path.isdir(os.path.join(git.root, d)) for d in JS_ROOTS):
        return {}  # a checkout with no production JS/TS (a scratch repository) has nothing to measure
    with tempfile.TemporaryDirectory(prefix="design-js-") as out:
        argv = [JS_REPORT, "--root", git.root, "--areas", JS_AREAS, "--out", out]
        done = git.runner.capture(tool_process.Command(argv, git.env))
        if done.code != 0:
            raise RuntimeError(f"scripts/js-quality-report exited {done.code}: {done.stderr.strip()[-400:]}")
        return read_js(os.path.join(out, "modules.json"), added)


def scala_modules(root: str, added: Set[str]) -> Modules:
    out: Modules = {}
    for path in scala_names.sources(root):
        with open(os.path.join(root, path), encoding="utf-8", errors="replace") as f:
            out[path] = scala_names.module(path, f.read(), path in added)
    return out


LANGUAGES = ("scala", "py", "js")


def language_of(part: str) -> str:
    """The language a baseline key's file or directory (or a `<from>-><to>` edge's first) is in."""
    path = part.split("->", 1)[0].split(":", 1)[0]
    if path.endswith(".scala"):
        return "scala"
    if path.startswith(JS_ROOTS):
        return "js"
    return "py"


def by_language(git: tool_git.Git, languages: Set[str], ranges: List[str], modules_json: str = "") -> Dict[str, Modules]:
    """Each asked language's modules."""
    added = added_files(git, ranges)
    readers = {"scala": lambda: scala_modules(git.root, added), "py": lambda: python_modules(git.root, added),
               "js": lambda: js_modules(git, added, modules_json)}
    return {language: readers[language]() for language in LANGUAGES if language in languages}


def added_files(git: tool_git.Git, ranges: List[str]) -> Set[str]:
    return {p for rng in ranges for p in git.text("diff", "--name-only", "--diff-filter=A", "--no-renames", rng).split()}
