"""A TREE'S WORKER TEST CLASSPATH COMPILED ONCE (scripts/lib; identity-tools.sh's identity_classpath and the compile it
calls): sbt run over the tree only when a source is newer than the classpath kept in it, one sbt a tree at a time, under
a JVM slot (jvm_gate.py).

  Classpaths(ctx, locks, gate).classpath(tree) -> the tree's worker test classpath (its kept file's text)
  classpath_line(log)                          the last line of an sbt log that IS a classpath (sbt prints more than
                                               `export` asked)"""
from __future__ import annotations

import os
import re
import subprocess
from typing import BinaryIO, Callable

from processes import machine
from identity_tooling.base.identity_context import Context, Failed
from identity_tooling.base.identity_lock import Locks
from identity_tooling.base.identity_tree_seeds import classpath_lock
from governor.jvm_gate import SLOT_FD, JvmGate, descriptor_open

SOURCES = ("geodata/src", "common/src", "identity/src", "scrapers/src", "worker/src", "testkit/src", "build.sbt")
KEPT = os.path.join("target", "identity-tools", "classpath")
LOG = os.path.join("target", "identity-tools", "sbt.log")
SBT_COMMANDS = ("-batch", "worker/Test/compile", "export worker/Test/fullClasspath")
TAIL = 40
CLASSPATH_LINE = re.compile(r"^/[^ ]*worker/target/[^ ]*test-classes")


def classpath_line(text: str) -> str:
    """The last line of it that IS a classpath: an absolute path through worker/target to its test-classes."""
    found = [line for line in text.splitlines() if CLASSPATH_LINE.match(line)]
    return found[-1] if found else ""


def newer_source(tree: str, kept: str) -> bool:
    """Whether a source of the tree is newer than the kept classpath (`find -newer`: a file, not a directory)."""
    reference = os.stat(kept).st_mtime_ns
    for name in SOURCES:
        root = os.path.join(tree, name)
        paths = [root] if os.path.isfile(root) else [os.path.join(d, f) for d, _, fs in os.walk(root) for f in fs]
        if any(is_newer(path, reference) for path in paths):
            return True
    return False


def is_newer(path: str, reference: int) -> bool:
    try:
        return os.stat(path).st_mtime_ns > reference
    except OSError:
        return False


class Classpaths:
    def __init__(self, ctx: Context, locks: Locks, gate: Callable[[], JvmGate]) -> None:
        self.ctx, self.locks, self.gate = ctx, locks, gate

    def fresh(self, tree: str) -> bool:
        kept = os.path.join(tree, KEPT)
        return os.path.isfile(kept) and os.path.getsize(kept) > 0 and not newer_source(tree, kept)

    def classpath(self, tree: str) -> str:
        if not self.fresh(tree):
            with self.locks.held(classpath_lock(tree)):
                self.compile(tree)
        return self.ctx.files.read_text(os.path.join(tree, KEPT))

    def compile(self, tree: str) -> None:
        """sbt over the tree, its classpath kept -- one sbt a tree at a time."""
        kept, log = os.path.join(tree, KEPT), os.path.join(tree, LOG)
        if self.fresh(tree):
            return
        self.ctx.files.make_directories(os.path.dirname(kept))
        self.seed_checkout(tree)
        self.ctx.log(f"compiling {tree} (sbt; then cached until a source changes)")
        if not self.sbt(tree, log):
            self.tail(log)
            self.ctx.log(f"compile failed in {tree} (log: {log})")
            raise Failed(1)
        self.ctx.put(kept, self.keeping(log))
        if not (os.path.isfile(kept) and os.path.getsize(kept) > 0):
            self.ctx.files.remove(kept)
            self.tail(log)
            self.ctx.log(f"no classpath in sbt's output ({log})")
            raise Failed(1)

    def seed_checkout(self, tree: str) -> None:
        """A checkout never compiled (a fresh worktree) gets sbt-warm's seed first -- the nearest local build of main, APFS
        clones: its first compile is incremental (2026-11-15: 72 s cold, 2 s seed + 10 s compile seeded)."""
        warm = os.path.join(tree, "scripts", "sbt-warm")
        if tree == self.ctx.repo and os.access(warm, os.X_OK):
            machine.status([warm, "--seed"], cwd=tree, out=2)

    def sbt(self, tree: str, log: str) -> bool:
        gate = self.gate()
        gate.slot_take(self.ctx.tool)
        try:
            options = gate.cpu_options()
            argv = ["sbt", *([f"-J{options}"] if options else []), *SBT_COMMANDS]
            held = tuple(fd for fd in (SLOT_FD,) if descriptor_open(fd))
            with open(log, "wb") as out:
                # PWD named as a shell's `cd` names it: the classpath sbt prints is read back as paths of this tree
                return machine.status(argv, cwd=tree, env={**self.ctx.env, "PWD": tree}, out=out, err=subprocess.STDOUT,
                                      pass_fds=held) == 0
        finally:
            gate.release()

    def tail(self, log: str) -> None:
        text = self.ctx.files.read_text_or(log, "")
        self.ctx.say("\n".join(text.splitlines()[-TAIL:]))

    def keeping(self, log: str) -> Callable[[BinaryIO], bool]:
        line = classpath_line(self.ctx.files.read_text_or(log, ""))

        def fill(out: BinaryIO) -> bool:
            if line:
                out.write((line + "\n").encode())
            return True
        return fill

