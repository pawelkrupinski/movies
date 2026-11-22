"""RESOLVING A SIDE (scripts/identity_what_if_cli.py): a ref's tree laid out once (identity-tools.sh's identity_tree,
this checkout's tool files dropped in) and compiled once (identity_classpath), and services.identity.WhatIfResolve run
over it -- through the tree's KEPT JVM (one per tree and code, started when none is, its JVM slot held by its keeper:
`--keep-served`), or a JVM of its own under a slot (--no-serve, and every whole side). Every JVM sequence that takes a
slot is identity-tools.sh's (identity_java_slotted, identity_kept_jvm_start, identity_kept_jvm_request): a slot is a
lock its process holds, never this one's."""
from __future__ import annotations

import glob
import hashlib
import os
import re
import signal
import stat
import sys
from typing import List, Optional, Sequence, Tuple

from identity_what_if_cli_context import TOOL, TOOL_FILES, Context, alive
import tool_process  # (scripts/lib, on the path the context put it on)

WHAT_IF_RESOLVE = "services.identity.WhatIfResolve"
KEPT_PATTERN = r"services\.identity\.WhatIfResolve"
START_LIMIT = 300  # seconds a kept JVM has to listen


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


def tail_to_stderr(path: str, n: int) -> None:
    try:
        with open(path, errors="replace") as f:
            lines = f.read().split("\n")
    except OSError:
        return
    if lines and lines[-1] == "":
        lines.pop()
    sys.stderr.write("".join(line + "\n" for line in lines[-n:]))
    sys.stderr.flush()


class KeptJvms:
    """The kept JVMs under <cache>/what-if-serve: `<key>.pid`, `.sock`, `.tree`, `.log`, `.keeper.log` a tree and code."""

    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx
        self.dir = os.path.join(ctx.cache, "what-if-serve")
        self.idle_minutes = ctx.knob("KINOWO_IDENTITY_WHAT_IF_IDLE_MINUTES", "10")

    def path(self, key: str, suffix: str) -> str:
        return os.path.join(self.dir, key + suffix)

    def is_kept(self, pid: str) -> bool:
        """Whether <pid> is still a what-if JVM: a pid file outlives its process, and the pid comes back as another's."""
        if not pid.isascii() or not pid.isdigit():
            return False
        done = self.ctx.runner.capture(tool_process.Command(["ps", "-o", "args=", "-p", pid], self.ctx.env))
        return bool(re.search(KEPT_PATTERN, done.stdout))

    def stop(self, pid: str) -> bool:
        if not self.is_kept(pid):
            return False
        try:
            os.kill(int(pid), signal.SIGTERM)
        except OSError:
            return False
        return True

    def stop_all(self) -> None:
        """--stop-served: every kept JVM stopped, its files removed."""
        for pidf in sorted(glob.glob(os.path.join(self.dir, "*.pid"))):
            if not os.path.isfile(pidf):
                continue
            if self.stop(read(pidf)):
                self.ctx.log(f"stopped the kept JVM {read(pidf)}")
            stem = pidf[:-len(".pid")]
            remove(pidf, stem + ".sock", stem + ".tree")

    def drop_older(self, key: str, tree: str) -> None:
        """An older code of this tree (an edit of this checkout) keeps no JVM of its own."""
        for treef in sorted(glob.glob(os.path.join(self.dir, "*.tree"))):
            stem = treef[:-len(".tree")]
            if os.path.isfile(treef) and os.path.basename(stem) != key and read(treef) == tree:
                self.stop(read(stem + ".pid"))
                remove(treef, stem + ".pid", stem + ".sock")


class Resolver:
    def __init__(self, ctx: Context, kept: KeptJvms, keeper: str) -> None:
        self.ctx, self.kept, self.keeper = ctx, kept, keeper

    def tree_of(self, ref: str) -> Tuple[int, str]:
        """(status, tree): this checkout for `.`, else the ref's laid-out tree."""
        if ref == ".":
            return 0, self.ctx.repo
        done = self.ctx.shell("identity_tree", [ref, *TOOL_FILES])
        return done.code, done.stdout.rstrip("\n")

    def classpath(self, tree: str) -> Optional[str]:
        done = self.ctx.shell("identity_classpath", [tree])
        return done.stdout.rstrip("\n") if done.code == 0 else None

    def classpath_fresh(self, tree: str) -> bool:
        return self.ctx.shell("identity_classpath_fresh", [tree]).code == 0

    def code_stamp(self, tree: str) -> str:
        """The code a tree runs: a ref's tree is its code; this checkout's, its commit and its uncommitted edits."""
        if tree != self.ctx.repo:
            return os.path.basename(tree)
        text = self.ctx.git("rev-parse", "HEAD").stdout + self.ctx.git("diff", "HEAD").stdout
        others = self.ctx.git("ls-files", "--others", "--exclude-standard", "--", "*.scala", "*.tsv").stdout
        for name in others.split("\n")[:-1] if others.endswith("\n") else others.split("\n"):
            path = os.path.join(self.ctx.repo, name)
            with open(path, "rb") as f:
                text += f"{hashlib.sha1(f.read()).hexdigest()}  {path}\n"
        return hashlib.sha1(text.encode()).hexdigest()[:16]

    def resolve(self, tree: str, cc: str, out: str, args: Sequence[str], serve: bool) -> bool:
        """That tree's WhatIfResolve of `cc` into `out`: through its kept JVM, else (or with `serve` off) a JVM of its
        own. False: failed, said."""
        os.makedirs(out, exist_ok=True)
        if serve:
            code = self.served(tree, cc, out, args)
            if code != 3:
                return code == 0
        cp = self.classpath(tree)
        if cp is None:
            return False
        log = os.path.join(out, "run.log")
        if self.ctx.shell("identity_java_slotted", [TOOL, tree, log, "-cp", cp, WHAT_IF_RESOLVE, cc, "--out", out,
                                                    *args]).code != 0:
            return self.failed(cc, tree, log)
        return True

    def failed(self, cc: str, tree: str, log: str) -> bool:
        tail_to_stderr(log, 30)
        self.ctx.log(f"{cc}: WhatIfResolve failed in {tree} (log {log})")
        return False

    def served(self, tree: str, cc: str, out: str, args: Sequence[str]) -> int:
        """The request through the tree's kept JVM: 0, 2 failed, 3 when no kept JVM could start."""
        key = hashlib.sha1(f"{tree}\n{self.code_stamp(tree)}\n".encode()).hexdigest()[:16]
        sock = self.kept.path(key, ".sock")
        os.makedirs(self.kept.dir, exist_ok=True)
        for _ in range(2):
            if not is_socket(sock) or not self.kept.is_kept(read(self.kept.path(key, ".pid"))):
                code = self.start_kept(tree, key, sock)
                if code:
                    return code
            code = self.request(tree, cc, out, sock, args)
            if code != 3:
                return code
        return 3

    def request(self, tree: str, cc: str, out: str, sock: str, args: Sequence[str]) -> int:
        """One request of the kept JVM at `sock` (its cores its own: identity_kept_jvm_request): 0, 2 failed, 3 when
        the JVM went away (said; its socket removed, for another to start)."""
        log = os.path.join(out, "run.log")
        rc = self.ctx.shell("identity_kept_jvm_request", [TOOL, "what-if", log, sock, cc, "--out", out, *args]).code
        if rc == 0:
            return 0
        if rc not in (111, 112):
            self.failed(cc, tree, log)
            return 2
        self.ctx.log(f"the kept JVM went away ({rc}): starting another")
        remove(sock)
        return 3

    def start_kept(self, tree: str, key: str, sock: str) -> int:
        """A kept JVM of the tree started and listening at `sock`: 0, 2 when the tree does not compile, 3 when it
        did not start or listen."""
        cp = self.classpath(tree)
        if cp is None:
            return 2
        remove(sock)
        self.kept.drop_older(key, tree)
        log, pidf = self.kept.path(key, ".log"), self.kept.path(key, ".pid")
        self.ctx.log(f"starting a kept JVM for {tree} (log {log}; stops after {self.kept.idle_minutes} idle minutes)")
        java: List[str] = ["-cp", cp, WHAT_IF_RESOLVE, "--serve", sock, "--idle-minutes", self.kept.idle_minutes]
        self.ctx.shell("identity_kept_jvm_start", [TOOL, tree, log, pidf, self.kept.path(key, ".keeper.log"),
                                                   self.keeper, *java])
        with open(self.kept.path(key, ".tree"), "w") as f:
            f.write(tree + "\n")
        return self.listening(read(pidf), sock, log)

    def listening(self, pid: str, sock: str, log: str) -> int:
        started = self.ctx.seconds()
        while not is_socket(sock):
            if not alive(pid):
                tail_to_stderr(log, 20)
                self.ctx.log("the kept JVM did not start")
                return 3
            if self.ctx.seconds() - started >= START_LIMIT:
                self.kept.stop(pid)
                self.ctx.log(f"the kept JVM did not listen in {START_LIMIT} s")
                return 3
            self.ctx.sleep(1)
        return 0
