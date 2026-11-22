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
import tempfile
from typing import Dict, List, Mapping, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
MANIFESTS = ("package.json", "package-lock.json")


class ToolMissing(Exception):
    """What the machine lacks to run the measure (node, npm), and how to get it: a missing tool blocks the push."""


def tools_dir(env: Mapping[str, str]) -> str:
    """Where the pinned tools live, keyed by the manifest and its lock: a bump installs beside the old set."""
    digest = hashlib.sha256()
    for name in MANIFESTS:
        with open(os.path.join(HERE, name), "rb") as f:
            digest.update(f.read())
    cache = env.get("KINOWO_TOOL_CACHE") or os.path.join(
        env.get("XDG_CACHE_HOME") or os.path.join(os.path.expanduser("~"), ".cache"), "kinowo")
    return os.path.join(cache, "py-quality", digest.hexdigest()[:16])


def entry(installed: str) -> str:
    return os.path.join(installed, "node_modules", "pyright", "index.js")


def install(installed: str) -> None:
    """`npm ci` into a directory beside `installed`, renamed into place: two installs at once each finish, one wins."""
    if shutil.which("npm") is None:
        raise ToolMissing("npm is not installed (needed once, to install pyright): brew install node")
    os.makedirs(os.path.dirname(installed), exist_ok=True)
    build = tempfile.mkdtemp(prefix=os.path.basename(installed) + ".build.", dir=os.path.dirname(installed))
    try:
        for name in MANIFESTS:
            shutil.copy(os.path.join(HERE, name), build)
        done = subprocess.run(["npm", "ci", "--no-audit", "--no-fund", "--silent"], cwd=build,
                              stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
        if done.returncode != 0:
            raise ToolMissing(f"npm ci of pyright failed in {build}: {done.stderr.strip()[-400:]}")
        try:
            os.rename(build, installed)
        except OSError:
            pass  # another install won the rename
    finally:
        shutil.rmtree(build, ignore_errors=True)
    if not os.path.isfile(entry(installed)):
        raise ToolMissing(f"pyright did not install in {installed}")


def ensure(env: Mapping[str, str]) -> str:
    if shutil.which("node") is None:
        raise ToolMissing("node is not installed (pyright runs on it): brew install node")
    installed = tools_dir(env)
    if not os.path.isfile(entry(installed)):
        install(installed)
    return entry(installed)


def errors(root: str, files: Sequence[str], env: Mapping[str, str]) -> Dict[str, int]:
    """file (relative to `root`) -> its strict pyright errors, for each of `files` (none asked: none run)."""
    if not files:
        return {}
    pyright = ensure(env)
    done = subprocess.run(["node", pyright, "--outputjson", "--project", os.path.join(root, "pyrightconfig.json"),
                           *files], cwd=root, capture_output=True, text=True)
    if done.returncode not in (0, 1):
        raise RuntimeError(f"pyright exited {done.returncode}: {done.stderr.strip()[-400:] or done.stdout[-400:]}")
    report = json.loads(done.stdout)
    counts = {f: 0 for f in files}
    for diagnostic in report.get("generalDiagnostics", []):
        if diagnostic.get("severity") == "error":
            path = os.path.relpath(diagnostic["file"], root)
            counts[path] = counts.get(path, 0) + 1
    return counts


def checked(paths: List[str]) -> List[str]:
    """The paths pyright reads: its .py files (an extension-less Python tool is measured, but not type-checked)."""
    return [p for p in paths if p.endswith(".py")]
