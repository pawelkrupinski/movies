"""THE MACHINE-WIDE LOCK OF A DOWNLOAD, A LAYOUT OR A COMPILE (scripts/lib; identity-tools.sh's identity_locked): one
agent at a time runs under a name in the shared cache, on jvm-gate.sh's kernel lock (jvm_gate.FileLock), so it is free
the moment its holder has gone, killed or not. A live holder is waited for at most KINOWO_IDENTITY_LOCK_WAIT seconds
(3600), then this fails naming the lock and its holder.

  with Locks(ctx).held("tree-<sha>"):   the lock file is <cache>/locks/<name>.flock, naming this process while it is held"""
from __future__ import annotations

import contextlib
import os
from typing import Callable, Generator

from processes import machine
from identity_tooling.base.identity_context import Context, Failed
from governor.jvm_gate import FileLock

POLL = 2          # seconds between two tries
REMINDER = 60     # seconds between two "waiting" lines


class Locks:
    def __init__(self, ctx: Context, sleep: Callable[[float], None] = machine.sleep) -> None:
        self.ctx = ctx
        self.sleep = sleep

    @contextlib.contextmanager
    def held(self, name: str) -> Generator[None, None, None]:
        directory = os.path.join(self.ctx.cache, "locks")
        self.ctx.files.make_directories(directory)
        path = os.path.join(directory, name + ".flock")
        lock = FileLock(path)
        self.wait_for(lock, name)
        try:
            yield
        finally:
            lock.give()

    def wait_for(self, lock: FileLock, name: str) -> None:
        waited = 0
        while not lock.take(machine.own_pid()):
            holder = self.ctx.files.read_text_or(lock.path, "").strip() or "?"
            if waited >= self.ctx.lock_wait:
                self.ctx.log(f"gave up after {waited}s waiting for {name} (held by pid {holder}; {lock.path})")
                raise Failed(1)
            if waited % REMINDER == 0:
                self.ctx.log(f"waiting for another agent's {name} (pid {holder})")
            self.sleep(POLL)
            waited += POLL
