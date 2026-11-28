"""THE KEPT WHOLE SIDES (scripts/identity_what_if_cli.py): a commit's every family resolved at a snapshot -- takes,
families, prod, final -- kept under `<cache>/what-if/<cc>/<sha>-<tools hash>-<live|offline>-mirror<generation>/<snapshot>/`
and resolved only when it is not. ONE PRODUCER: a run takes `<dir>.lock` (mkdir, a pid file in it; a dead holder's
lock broken) before it resolves into a `.part` of its own, renamed whole into <dir>; a run finding the lock held waits
and then reads the side the holder kept. A side whose live asks failed past KINOWO_WHAT_IF_MAX_FAILED_ASKS is a HARNESS
ERROR and is not kept."""
from __future__ import annotations

import datetime
import os
import random
import re
import shutil
from typing import List, Optional

from identity_what_if_cli_context import Context, Exit, alive
from processes import machine  # scripts/lib, on the path the context put it on
from identity_what_if_cli_resolve import Resolver, read


class SideKey:
    """What a kept side is keyed by besides the commit: the tool files' hash, the answers and the mirror generation."""

    def __init__(self, tools_hash: str, side_answers: str) -> None:
        self.tools_hash, self.side_answers = tools_hash, side_answers
        self.mirror_gen = ""


class Sides:
    def __init__(self, ctx: Context, resolver: Resolver, key: SideKey, answer_args: List[str]) -> None:
        self.ctx, self.resolver, self.key, self.answer_args = ctx, resolver, key, answer_args
        self.final = ctx.o.final
        self.max_failed = int(ctx.knob("KINOWO_WHAT_IF_MAX_FAILED_ASKS", "25"))
        self.lock_wait = int(ctx.knob("KINOWO_WHAT_IF_SIDE_LOCK_WAIT_SECONDS", "3600"))
        self.lock_poll = float(ctx.knob("KINOWO_WHAT_IF_SIDE_LOCK_POLL_SECONDS", "2"))

    def root(self, cc: str, sha: str) -> str:
        k = self.key
        return os.path.join(self.ctx.cache, "what-if", cc, f"{sha}-{k.tools_hash}-{k.side_answers}-mirror{k.mirror_gen}")

    def cached_at(self, cc: str, sha: str) -> str:
        """The newest snapshot moment `sha` was already resolved at for `cc` within --max-age hours, else ""."""
        root = self.root(cc, sha)
        if not os.path.isdir(root):
            return ""
        now = datetime.datetime.fromtimestamp(machine.wall(), datetime.timezone.utc)
        want = "final.tsv" if self.final else "prod.tsv"
        window = datetime.timedelta(hours=float(self.ctx.o.max_age))
        fresh = [d for d in os.listdir(root) if os.path.exists(os.path.join(root, d, "prod.tsv"))
                 and os.path.exists(os.path.join(root, d, want)) and within(d, now, window)]
        return max(fresh) if fresh else ""

    def complete(self, directory: str) -> bool:
        return os.path.isfile(os.path.join(directory, "prod.tsv")) and (
            not self.final or os.path.isfile(os.path.join(directory, "final.tsv")))

    def whole(self, sha: str, cc: str, snapshot: str) -> str:
        """The directory of `sha`'s every family resolved at `snapshot`, resolved here only when it is not kept;
        exit 2 when it cannot be."""
        directory = os.path.join(self.root(cc, sha), snapshot)
        if not self.complete(directory):
            self.lock(directory)
            try:
                if self.complete(directory):
                    self.ctx.log(f"{cc}: reading {sha[:12]}'s side at {snapshot} another what-if just resolved")
                else:
                    self.produce(sha, cc, snapshot, directory)
            finally:
                shutil.rmtree(directory + ".lock", ignore_errors=True)
        if not self.answers_ok(directory, sha[:12]):
            shutil.rmtree(directory, ignore_errors=True)
            raise Exit(2)
        return directory

    def produce(self, sha: str, cc: str, snapshot: str, directory: str) -> None:
        """The side resolved into a `.part` of this run's and renamed whole into `directory`, the lock held."""
        part = f"{directory}.part-{self.ctx.pid}-{random.randint(0, 32767)}"
        code, tree = self.resolver.tree_of(sha)
        if code != 0:
            self.ctx.harness_error(f"{cc}: laying out {sha[:12]} for its whole side failed (exit {code})")
            raise Exit(2)
        self.ctx.log(f"{cc}: resolving {sha[:12]} whole at {snapshot}")
        args = ["--at", snapshot, *self.answer_args, *(["--final"] if self.final else []), "--mirror-generation",
                self.key.mirror_gen]
        # a JVM of its own, never a kept one: the side is kept on disk and never asked of that JVM again, which would
        # only have held a JVM slot idle for its 10 minutes (2026-11-15: a what-if queued 390 s behind two such)
        if not self.resolver.resolve(tree, cc, part, args, serve=False) or not self.answers_ok(part, sha[:12]):
            shutil.rmtree(part, ignore_errors=True)
            raise Exit(2)
        shutil.rmtree(directory, ignore_errors=True)
        os.rename(part, directory)

    def lock(self, directory: str) -> None:
        """`<directory>.lock` held by this run; exit 2 when another kept it past the wait."""
        lock, started, said = directory + ".lock", self.ctx.seconds(), False
        os.makedirs(os.path.dirname(directory), exist_ok=True)
        while not made(lock):
            holder = read(os.path.join(lock, "pid"))
            if holder and not alive(holder):
                self.ctx.log(f"breaking the lock of {os.path.basename(directory)} left by {holder} (no longer running)")
                shutil.rmtree(lock, ignore_errors=True)
                continue
            if self.ctx.seconds() - started >= self.lock_wait:
                self.ctx.harness_error(f"{self.ctx.cc}: another what-if (pid {holder or '?'}) has been resolving this "
                                       f"side for {self.lock_wait}s ({lock})")
                raise Exit(2)
            if not said:
                self.ctx.log(f"{self.ctx.cc}: another what-if (pid {holder or '?'}) is resolving this side: waiting to "
                             "read it")
                said = True
            self.ctx.sleep(self.lock_poll)
        with open(os.path.join(lock, "pid"), "w") as f:
            f.write(f"{self.ctx.pid}\n")

    def answers_ok(self, directory: str, label: str) -> bool:
        """Whether the side's live asks failed within the bound (answers.txt: WhatIfAnswers' `N live request(s) (...;
        F failed) ...`); past it, said."""
        line = read(os.path.join(directory, "answers.txt"))
        failed, asked = matched(r".*; ([0-9]+) failed\).*", line), matched(r"^([0-9]+) live request.*", line)
        if failed <= self.max_failed:
            return True
        self.ctx.harness_error(
            f"{self.ctx.cc}: answers failed: {failed} of {asked} live ask(s) of the {label} side failed (more than "
            f"{self.max_failed}, KINOWO_WHAT_IF_MAX_FAILED_ASKS): their questions are gaps the side took as pending, "
            "not moves -- the mirror not ready, or a service refusing; the side is not kept")
        return False


def matched(pattern: str, text: str) -> int:
    """sed -nE 's/<pattern>/\\1/p' over the first line it matches, 0 when none does."""
    for line in text.split("\n"):
        m = re.match(pattern, line)
        if m:
            return int(m.group(1))
    return 0


def made(directory: str) -> bool:
    try:
        os.mkdir(directory)
    except OSError:
        return False
    return True


def within(name: str, now: datetime.datetime, window: datetime.timedelta) -> bool:
    """Whether the snapshot directory `name` is a moment within `window` of now (a `.part` or `.lock` is none)."""
    try:
        moment: Optional[datetime.datetime] = datetime.datetime.fromisoformat(name.replace("Z", "+00:00"))
    except ValueError:
        return False
    return moment.tzinfo is not None and now - moment <= window
