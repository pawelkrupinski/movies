"""STRICT PYRIGHT, PINNED (scripts/py-quality/report.py's `pyright` rows): the type checker of scripts/py-quality's
package.json and its lock, installed by `npm ci` OUTSIDE the checkout -- ~/.cache/kinowo/py-quality/<lock hash>
($KINOWO_TOOL_CACHE for ~/.cache/kinowo), shared by every worktree, as js-quality-report installs its tools (a
node_modules in the tree is read by the next self-test's `grep -r`, Order independence #99) -- and run over the files
asked with the checkout's pyrightconfig.json (strict, Python 3.9). A file's row is its count of errors."""
from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
import sys
from typing import Any, Dict, Mapping, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import pinned_npm_tools  # noqa: E402

MANIFESTS = ("package.json", "package-lock.json")


class ToolMissing(Exception):
    """What the machine lacks to run the measure (node, npm), and how to get it: a missing tool blocks the push."""


def tools_dir(env: Mapping[str, str]) -> str:
    """Where the pinned tools live, keyed by the manifest and its lock: a bump installs beside the old set."""
    digest = hashlib.sha256()
    for name in MANIFESTS:
        with open(os.path.join(HERE, name), "rb") as f:
            digest.update(f.read())
    return os.path.join(pinned_npm_tools.cache_root(env), "py-quality", digest.hexdigest()[:16])


def entry(installed: str) -> str:
    return os.path.join(installed, "node_modules", "pyright", "index.js")


def install(installed: str) -> None:
    """`npm ci` into a directory beside `installed`, renamed into place: two installs at once each finish, one wins."""
    if shutil.which("npm") is None:
        raise ToolMissing("npm is not installed (needed once, to install pyright): brew install node")
    try:
        pinned_npm_tools.npm_ci(HERE, MANIFESTS, installed, capture=True)
    except pinned_npm_tools.NpmFailed as e:
        raise ToolMissing(f"npm ci of pyright failed in {e.build}: {e.said}") from None
    if not os.path.isfile(entry(installed)):
        raise ToolMissing(f"pyright did not install in {installed}")


def ensure(env: Mapping[str, str]) -> str:
    if shutil.which("node") is None:
        raise ToolMissing("node is not installed (pyright runs on it): brew install node")
    installed = tools_dir(env)
    if not os.path.isfile(entry(installed)):
        install(installed)
    return entry(installed)


STAGED = ".extensionless.py"  # pyright reads only .py: an extension-less tool is checked as a sibling copy of this name


def errors(root: str, files: Sequence[str], env: Mapping[str, str]) -> Dict[str, int]:
    """file (relative to `root`) -> its strict pyright errors, for each of `files` (none asked: none run). A file with no
    .py extension is copied beside itself as `<file>.extensionless.py` for the run (its imports resolve as the tool's do)
    and removed after; its errors count under its own name."""
    if not files:
        return {}
    pyright = ensure(env)
    staged = {f + STAGED: f for f in files if not f.endswith(".py")}
    report = run_staged(root, pyright, files, staged)
    counts = {f: 0 for f in files}
    for diagnostic in report.get("generalDiagnostics", []):
        if diagnostic.get("severity") == "error":
            path = os.path.relpath(diagnostic["file"], root)
            path = staged.get(path, path)
            counts[path] = counts.get(path, 0) + 1
    return counts


def run_staged(root: str, pyright: str, files: Sequence[str], staged: Mapping[str, str]) -> Dict[str, Any]:
    """pyright's report over `files`, each of `staged` (copy -> original) written beside its original for the run."""
    for copy, original in staged.items():
        shutil.copyfile(os.path.join(root, original), os.path.join(root, copy))
    try:
        return run(root, pyright, [f + STAGED if f + STAGED in staged else f for f in files])
    finally:
        for copy in staged:
            os.remove(os.path.join(root, copy))


def run(root: str, pyright: str, files: Sequence[str]) -> Dict[str, Any]:
    """pyright's --outputjson report over `files`."""
    done = subprocess.run(["node", pyright, "--outputjson", "--project", os.path.join(root, "pyrightconfig.json"),
                           *files], cwd=root, capture_output=True, text=True)
    if done.returncode not in (0, 1):
        raise RuntimeError(f"pyright exited {done.returncode}: {done.stderr.strip()[-400:] or done.stdout[-400:]}")
    return json.loads(done.stdout)
