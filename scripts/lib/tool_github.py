"""GITHUB THROUGH `gh`, the one way the Python tools ask it (scripts/lib): every call bounded (a hung API read is no
answer, never a step waiting out its job's timeout), its stderr passed on unless asked to be quiet, and a read that
fails told apart from one that answered -- `read` gives None for a failed call, never an empty answer.

  GitHub(runner, env).read(["api", "repos/o/r/commits/x", "--jq", ".sha"], timeout=120) -> stdout, or None
  .succeeds([...], timeout, quiet=True)                                                  -> whether gh exited 0
  .download(release, asset, dest, timeout)  `gh release download` of one asset to `dest`: whether it is there now"""
from __future__ import annotations

import os
import sys
from typing import Mapping, Optional, Sequence

import tool_process


class GitHub:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], gh: str = "gh") -> None:
        self.runner = runner
        self.env = env
        self.gh = gh

    def call(self, args: Sequence[str], timeout: float, quiet: bool = False) -> tool_process.Captured:
        done = self.runner.capture(tool_process.Command([self.gh, *args], self.env, timeout=timeout))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def read(self, args: Sequence[str], timeout: float, quiet: bool = False) -> Optional[str]:
        done = self.call(args, timeout, quiet)
        return done.stdout if done.code == 0 else None

    def succeeds(self, args: Sequence[str], timeout: float, quiet: bool = True) -> bool:
        return self.call(args, timeout, quiet).code == 0

    def download(self, release: str, asset: str, dest: str, timeout: float) -> bool:
        """One release asset into `dest` (replaced): False when absent or the download failed, quietly."""
        os.makedirs(os.path.dirname(dest) or ".", exist_ok=True)
        return self.succeeds(["release", "download", release, "-p", asset, "-O", dest, "--clobber"], timeout) \
            and os.path.isfile(dest)
