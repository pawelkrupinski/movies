#!/usr/bin/env python3
"""Every Python tool in the repository runs on the Python floor, scripts/run-script-tests' `PythonFloor` (3.9: macOS's
/usr/bin/python3, where agents and the pre-push hook run them; CI's runners are newer, so CI alone never sees a break).

Two ways a file outgrows the floor without any test on a newer Python noticing:
  - SYNTAX newer than the floor (`match`, ...): the file is parsed with `feature_version` set to the floor.
  - an ANNOTATION evaluated at runtime that uses `X | Y` (PEP 604, 3.10): a TypeError the moment the def runs on 3.9,
    unless the module has `from __future__ import annotations`, which leaves every annotation unevaluated.
Library calls new since the floor (`zip(strict=)`, ...) are not detected; the floor's own runs catch those.

usage: python_floor_lint.py [ROOT]   (default the checkout this script is in)
Exit 0 when every tracked Python file -- *.py, and extension-less scripts with a python shebang -- is within the floor,
1 listing each that is not.
"""

import ast
import os
import re
import subprocess
import sys

RUNNER = "scripts/run-script-tests"


def floor_of(root):
    """The (major, minor) the runner states as `PythonFloor="M.N"`."""
    with open(os.path.join(root, RUNNER), encoding="utf-8") as f:
        found = re.search(r'^PythonFloor="(\d+)\.(\d+)"', f.read(), re.M)
    if not found:
        raise SystemExit(f"{RUNNER} states no PythonFloor=\"M.N\"")
    return int(found.group(1)), int(found.group(2))


def git(root, *args):
    """git in `root`, whatever repository a calling hook's GIT_DIR / GIT_WORK_TREE / GIT_INDEX_FILE point at."""
    env = {k: v for k, v in os.environ.items() if k not in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE")}
    return subprocess.run(["git", "-C", root] + list(args), capture_output=True, text=True, check=True, env=env).stdout


def python_files(root):
    tracked = git(root, "ls-files", "-z")
    for path in sorted(p for p in tracked.split("\0") if p):
        name = os.path.basename(path)
        if name.endswith(".py"):
            yield path
        elif "." not in name:
            try:
                with open(os.path.join(root, path), "rb") as f:
                    first = f.readline()
            except OSError:
                continue
            if first.startswith(b"#!") and b"python" in first:
                yield path


def annotations(tree):
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            a = node.args
            for arg in a.posonlyargs + a.args + a.kwonlyargs + [a.vararg, a.kwarg]:
                if arg is not None and arg.annotation is not None:
                    yield node.lineno, arg.annotation
            if node.returns is not None:
                yield node.lineno, node.returns
        elif isinstance(node, ast.AnnAssign):
            yield node.lineno, node.annotation


def findings_in(source, floor):
    """(line, what) for each way `source` outgrows `floor`."""
    try:
        tree = ast.parse(source, feature_version=floor)
    except SyntaxError as e:
        return [(e.lineno or 0, f"syntax newer than Python {floor[0]}.{floor[1]}: {e.msg}")]
    deferred = any(isinstance(n, ast.ImportFrom) and n.module == "__future__" and any(a.name == "annotations" for a in n.names)
                   for n in tree.body)
    if deferred:
        return []
    return sorted({(line, "an `X | Y` annotation (Python 3.10) without `from __future__ import annotations`")
                   for line, ann in annotations(tree)
                   if any(isinstance(n, ast.BinOp) and isinstance(n.op, ast.BitOr) for n in ast.walk(ann))})


def main(argv):
    root = os.path.abspath(argv[0]) if argv else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
    floor = floor_of(root)
    bad = []
    for path in python_files(root):
        try:
            with open(os.path.join(root, path), encoding="utf-8") as f:
                source = f.read()
        except (OSError, UnicodeDecodeError):
            continue
        bad += [f"{path}:{line}: {what}" for line, what in findings_in(source, floor)]
    for line in bad:
        print(line)
    if bad:
        print(f"{len(bad)} finding(s): the Python floor is {floor[0]}.{floor[1]} ({RUNNER}'s PythonFloor)")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
