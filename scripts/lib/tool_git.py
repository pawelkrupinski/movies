"""GIT, the one way the Python tools run it (scripts/lib): in one repository (`git -C <root>`), each call through the
injected runner, optionally bounded, its stderr passed on unless asked to be quiet.

  Git(runner, env, root).out("rev-parse", "HEAD") -> stdout, or None when git failed
  .ok("diff", "--quiet", a, b)                    -> whether git exited 0"""
from __future__ import annotations

import sys
from typing import Mapping, Optional, Sequence

import tool_process


class Git:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], root: str) -> None:
        self.runner = runner
        self.env = env
        self.root = root

    def call(self, *args: str, timeout: Optional[float] = None, quiet: bool = False,
             options: Sequence[str] = ()) -> tool_process.Captured:
        """`git [options] -C <root> args`: `options` (`-c key=value`) before the repository."""
        done = self.runner.capture(tool_process.Command(["git", *options, "-C", self.root, *args], self.env,
                                                        timeout=timeout))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def out(self, *args: str, timeout: Optional[float] = None, quiet: bool = False) -> Optional[str]:
        done = self.call(*args, timeout=timeout, quiet=quiet)
        return done.stdout if done.code == 0 else None

    def ok(self, *args: str, timeout: Optional[float] = None, quiet: bool = False) -> bool:
        return self.call(*args, timeout=timeout, quiet=quiet).code == 0
