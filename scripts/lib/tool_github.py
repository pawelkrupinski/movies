"""GITHUB THROUGH `gh`, the one way the Python tools ask it (scripts/lib): every call bounded (a hung API read is no
answer, never a step waiting out its job's timeout), its stderr passed on unless asked to be quiet, and a read that
fails told apart from one that answered -- `read` gives None for a failed call, never an empty answer.

  GitHub(runner, env).read(["api", "repos/o/r/commits/x", "--jq", ".sha"], timeout=120) -> stdout, or None
  .succeeds([...], timeout, quiet=True)                                                  -> whether gh exited 0
  .download(release, asset, dest, timeout)  `gh release download` of one asset to `dest`: whether it is there now
  .asset_names(release, timeout)            the release's asset names, or None when they could not be listed
  .fetch_gzipped(release, asset, path, timeout)  a gzipped asset unpacked into `path`: whether it is there, non-empty"""
from __future__ import annotations

import gzip
import os
import shutil
import sys
from typing import List, Mapping, Optional, Sequence

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

    def asset_names(self, release: str, timeout: float, quiet: bool = False) -> Optional[List[str]]:
        """Every asset name of `release`; None when the listing failed -- never an empty release."""
        listed = self.read(["release", "view", release, "--json", "assets", "--jq", ".assets[].name"], timeout, quiet)
        return None if listed is None else [name for name in listed.split("\n") if name]

    def fetch_gzipped(self, release: str, asset: str, path: str, timeout: float) -> bool:
        """The gzipped `asset` downloaded beside `path` (`path`.gz) and unpacked into it, quietly: False when absent,
        failed, or not a whole gzip (a truncated download is no result)."""
        packed = path + ".gz"
        if not self.download(release, asset, packed, timeout):
            return False
        try:
            with gzip.open(packed, "rb") as source, open(path, "wb") as plain:
                shutil.copyfileobj(source, plain)
        except (OSError, EOFError):
            return False
        os.remove(packed)
        return os.path.getsize(path) > 0
