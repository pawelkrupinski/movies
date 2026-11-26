"""GIT, the one way the Python tools run it (scripts/lib): in one repository (`git -C <root>`), each call through the
injected runner, optionally bounded, its stderr passed on unless asked to be quiet.

Every call acts on `root`, whatever repository a calling hook's GIT_DIR / GIT_WORK_TREE point at (a hook exports them
for ITS repository, so a bare `git -C <other>` reads the hook's), and prints a path as itself (`core.quotePath=false`:
`drift-slices/pl/...przemyśl...` is not a quoted octal escape that no prefix test matches).

  Git(runner, env, root).out("rev-parse", "HEAD") -> stdout, or None when git failed
  .line("rev-parse", "--verify", x)               -> stdout stripped, or None when git failed (its stderr unprinted)
  .lines("ls-files", "--others")                  -> its stdout lines, [] when git failed (its stderr unprinted)
  .ok("diff", "--quiet", a, b)                    -> whether git exited 0
  .text("show", "HEAD:x", ok=(0, 128))           -> stdout, or raises Failed outside `ok` (git's stderr unprinted)
  Git.here(root)                                  -> over this process's one subprocess runner and its environment"""
from __future__ import annotations

import functools
import os
import subprocess
import sys
from typing import BinaryIO, List, Mapping, Optional, Sequence, cast

import tool_process

HOOK_REPOSITORY = ("GIT_DIR", "GIT_WORK_TREE")


class Failed(subprocess.CalledProcessError):
    """A git that exited outside the codes its caller expects, said `git <args>: <its stderr, or its exit code>`."""

    def __str__(self) -> str:
        return f"{' '.join(self.cmd)}: {(self.stderr or '').strip() or self.returncode}"


@functools.lru_cache(maxsize=None)
def shared_runner() -> tool_process.SubprocessRunner:
    """One runner a process (each installs its signal handlers): git only captures, so the stdout it would tee to is
    the process's own whatever a caller swapped sys.stdout for."""
    out = sys.__stdout__
    return tool_process.SubprocessRunner(stdout=cast(BinaryIO, out.buffer) if out is not None else None)


class Git:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], root: str) -> None:
        self.runner = runner
        self.env = {k: v for k, v in env.items() if k not in HOOK_REPOSITORY}
        self.root = root

    @classmethod
    def here(cls, root: str = ".") -> "Git":
        return cls(shared_runner(), os.environ, root)

    def call(self, *args: str, timeout: Optional[float] = None, quiet: bool = False,
             options: Sequence[str] = (), input: Optional[str] = None) -> tool_process.Captured:
        """`git [options] -C <root> args`: `options` (`-c key=value`) before the repository."""
        argv = ["git", "-c", "core.quotePath=false", *options, "-C", self.root, *args]
        done = self.runner.capture(tool_process.Command(argv, self.env, timeout=timeout, input=input))
        if done.stderr and not quiet:
            sys.stderr.write(done.stderr)
            sys.stderr.flush()
        return done

    def out(self, *args: str, timeout: Optional[float] = None, quiet: bool = False) -> Optional[str]:
        done = self.call(*args, timeout=timeout, quiet=quiet)
        return done.stdout if done.code == 0 else None

    def line(self, *args: str, timeout: Optional[float] = None) -> Optional[str]:
        """The stripped stdout of a git that succeeded (`rev-parse --verify`), None when it failed; its stderr unprinted."""
        out = self.out(*args, timeout=timeout, quiet=True)
        return None if out is None else out.strip()

    def lines(self, *args: str, timeout: Optional[float] = None) -> List[str]:
        """The lines of a git that succeeded (`ls-files`, `diff --name-only`); none when it failed -- its stderr unprinted."""
        return (self.out(*args, timeout=timeout, quiet=True) or "").splitlines()

    def ok(self, *args: str, timeout: Optional[float] = None, quiet: bool = False) -> bool:
        return self.call(*args, timeout=timeout, quiet=quiet).code == 0

    def text(self, *args: str, ok: Sequence[int] = (0,), input: Optional[str] = None) -> str:
        done = self.call(*args, quiet=True, input=input)
        if done.code not in ok:
            raise Failed(done.code, ["git", *args], done.stdout, done.stderr)
        return done.stdout
