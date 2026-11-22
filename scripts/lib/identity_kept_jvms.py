"""The kept JVMs of the identity tools in Python (identity-what-if's and identity-slice's): each a JVM started once and
asked again over a unix socket, its files `<key>.pid`, `.sock`, `.tree`, `.log`, `.keeper.log` in one directory of the
shared cache -- and the file and process helpers their callers share. What starts a kept JVM and holds its JVM slot is
scripts/identity-tools.sh's (identity_kept_jvm_start, identity_kept_jvm_keep), through scripts/lib/tool_shell.py.

  KeptJvmFiles(runner, env, log, directory, pattern)  the directory's JVMs, each told by `pattern` in its command line
  read(path), remove(*paths), is_socket(path), alive(pid), tail_to_stderr(path, n)"""
from __future__ import annotations

import glob
import os
import re
import signal
import stat
import sys
from typing import Callable, Mapping, Sequence

import tool_process


def read(path: str) -> str:
    """`$(cat path 2>/dev/null)`."""
    try:
        with open(path) as f:
            return f.read().rstrip("\n")
    except OSError:
        return ""


def remove(*paths: str) -> None:
    for path in paths:
        try:
            os.remove(path)
        except OSError:
            pass


def is_socket(path: str) -> bool:
    try:
        return stat.S_ISSOCK(os.stat(path).st_mode)
    except OSError:
        return False


def alive(pid: str) -> bool:
    """`kill -0 <pid>`: a number naming a process this user may signal."""
    if not pid.isascii() or not pid.isdigit():
        return False
    try:
        os.kill(int(pid), 0)
    except (OSError, OverflowError):
        return False
    return True


def tail_to_stderr(path: str, n: int) -> None:
    """`tail -<n> <path> >&2`."""
    try:
        with open(path, errors="replace") as f:
            lines = f.read().split("\n")
    except OSError:
        return
    if lines and lines[-1] == "":
        lines.pop()
    sys.stderr.write("".join(line + "\n" for line in lines[-n:]))
    sys.stderr.flush()


class KeptJvmFiles:
    """The kept JVMs whose files lie in `directory`, a JVM told from another process by `pattern` (a regex) in its
    command line: a pid file outlives its process, and the pid comes back as someone else's."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], log: Callable[[str], None],
                 directory: str, pattern: str) -> None:
        self.runner, self.env, self.log, self.dir, self.pattern = runner, env, log, directory, pattern

    def path(self, key: str, suffix: str) -> str:
        return os.path.join(self.dir, key + suffix)

    def is_kept(self, pid: str) -> bool:
        if not pid.isascii() or not pid.isdigit():
            return False
        done = self.runner.capture(tool_process.Command(["ps", "-o", "args=", "-p", pid], self.env))
        return bool(re.search(self.pattern, done.stdout))

    def stop(self, pid: str) -> bool:
        """The kept JVM <pid> stopped (TERM) -- only while it still is one."""
        if not self.is_kept(pid):
            return False
        try:
            os.kill(int(pid), signal.SIGTERM)
        except OSError:
            return False
        return True

    def stop_all(self, forget: Sequence[str] = (".sock", ".tree")) -> None:
        """--stop-served: every kept JVM stopped, its pid file and its `forget` files removed."""
        for pidf in sorted(glob.glob(os.path.join(self.dir, "*.pid"))):
            if not os.path.isfile(pidf):
                continue
            if self.stop(read(pidf)):
                self.log(f"stopped the kept JVM {read(pidf)}")
            stem = pidf[:-len(".pid")]
            remove(pidf, *(stem + suffix for suffix in forget))

    def start(self, shell: Callable[[str, Sequence[str]], object], label: str, tree: str, key: str, keeper: str,
              java: Sequence[str]) -> str:
        """A kept JVM of <tree> started under <key> (identity-tools.sh's identity_kept_jvm_start, its JVM slot handed
        to `<keeper> --keep-served <pid>`), its tree noted beside it: the JVM's pid."""
        shell("identity_kept_jvm_start", [label, tree, self.path(key, ".log"), self.path(key, ".pid"),
                                          self.path(key, ".keeper.log"), keeper, *java])
        with open(self.path(key, ".tree"), "w") as f:
            f.write(tree + "\n")
        return read(self.path(key, ".pid"))

    def drop_older(self, key: str, tree: str, said: str = "") -> None:
        """An older code of this tree (an edit of this checkout) keeps no JVM of its own; each one stopped is `said`."""
        for treef in sorted(glob.glob(os.path.join(self.dir, "*.tree"))):
            stem = treef[:-len(".tree")]
            if os.path.isfile(treef) and os.path.basename(stem) != key and read(treef) == tree:
                if self.stop(read(stem + ".pid")) and said:
                    self.log(said)
                remove(treef, stem + ".pid", stem + ".sock")
