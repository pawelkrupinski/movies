"""PINNED NODE TOOLS, installed OUTSIDE the checkout: a tool's package.json and its lock, `npm ci`'d once per lock into
~/.cache/kinowo/<tool>/<lock hash> ($KINOWO_TOOL_CACHE for ~/.cache/kinowo, else $XDG_CACHE_HOME/kinowo) and shared by
every worktree -- a node_modules left in the tree was read by the next self-test's `grep -r` (Order independence #99).
Used by scripts/js_quality_report.py (ESLint, jscpd, dependency-cruiser) and scripts/py-quality/pyright.py (pyright);
each keys its own directory and says its own errors.

  cache_root(env)                              ~/.cache/kinowo, or where the environment moves it
  npm_ci(sources, manifests, installed, ...)   the install, built aside and renamed into place: two installs at
                                               once each finish, and one rename wins; NpmFailed when npm ci fails"""
from __future__ import annotations

import os
import shutil
import sys
import tempfile
from typing import Mapping, Sequence

import tool_process


class NpmFailed(Exception):
    """`npm ci` exited non-zero: where it ran, and the tail of what it said when that was captured."""

    def __init__(self, build: str, said: str) -> None:
        super().__init__(f"npm ci failed in {build}")
        self.build = build
        self.said = said


def cache_root(env: Mapping[str, str]) -> str:
    return env.get("KINOWO_TOOL_CACHE") or os.path.join(
        env.get("XDG_CACHE_HOME") or os.path.join(os.path.expanduser("~"), ".cache"), "kinowo")


def npm_ci(sources: str, manifests: Sequence[str], installed: str, *, capture: bool, runner: tool_process.Runner,
           env: Mapping[str, str]) -> None:
    """`npm ci` of `sources`' `manifests` into a directory beside `installed`, renamed into place. `capture`: npm's
    output kept for NpmFailed; otherwise all of it goes to this process's stderr, as it runs. The `runner` runs npm, in
    `env` (the caller's composition root hands both)."""
    os.makedirs(os.path.dirname(installed), exist_ok=True)
    build = tempfile.mkdtemp(prefix=os.path.basename(installed) + ".build.", dir=os.path.dirname(installed))
    try:
        for name in manifests:
            shutil.copy(os.path.join(sources, name), build)
        sys.stderr.flush()
        done = runner.capture(tool_process.Command(["npm", "ci", "--no-audit", "--no-fund", "--silent"], env, cwd=build,
                                                   stderr_through=not capture))
        if done.code != 0:
            raise NpmFailed(build, (done.stderr or "").strip()[-400:])
        try:
            os.rename(build, installed)
        except OSError:
            pass  # another install won the rename
    finally:
        shutil.rmtree(build, ignore_errors=True)
