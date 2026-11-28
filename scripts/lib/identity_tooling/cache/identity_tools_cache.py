"""THE CACHE COMMANDS OF scripts/lib/identity_tooling/identity_tools.py (captures, trees, classpaths, recordings), and the composition root
every command reads through: `Tools` holds the collaborators of one call and the stream its answer is written to."""
from __future__ import annotations

import os
import platform
import time
from typing import List, TextIO

from identity_tooling.cache.identity_captures import Captures
from identity_tooling.cache.identity_classpath import Classpaths, classpath_line
from identity_tooling.base.identity_context import Context
from identity_tooling.base.identity_lock import Locks
from identity_tooling.cache.identity_recordings import Recordings
from identity_tooling.base.identity_tree_seeds import Seeds
from identity_tooling.cache.identity_trees import Trees, tools_hash
from governor.jvm_gate import Governor, Host, JvmGate, SubprocessCommands
from governor.jvm_memory import Clock, MemoryGate, SlotWait


def compose_gate(ctx: Context) -> JvmGate:
    """THE GOVERNOR's ticket and the JVM slots (jvm_gate.py): the composition root's one gate."""
    home = os.path.expanduser("~")
    say = ctx.say
    host = Host(ctx.env, os.getpid(), platform.system(), home, lambda: ctx.repo, say)
    governor = Governor(host, SubprocessCommands())
    memory = MemoryGate(ctx.env, Clock(time.sleep), say, lambda: governor.bin() is not None, platform.system())
    slots = os.path.join(ctx.cache, "jvm-slots")
    return JvmGate(host, governor, memory, SlotWait(memory, slots), slots)


class Tools:
    """THE COMPOSITION ROOT: the collaborators every command reads through, and where its answer is written."""

    def __init__(self, ctx: Context, answer: TextIO) -> None:
        self.ctx, self.answer = ctx, answer
        self.locks = Locks(ctx)
        self.seeds = Seeds(ctx)
        self.captures = Captures(ctx, self.locks)
        self.trees = Trees(ctx, self.locks, self.seeds)
        self.classpaths = Classpaths(ctx, self.locks, lambda: compose_gate(ctx))
        self.recordings = Recordings(ctx, self.locks)

    def say(self, text: str) -> int:
        self.answer.write(text + "\n")
        self.answer.flush()
        return 0


def capture(t: Tools, a: List[str]) -> int:
    return t.say(t.captures.capture(a[0], a[1:]))


def judgements(t: Tools, a: List[str]) -> int:
    return t.say(t.captures.judgements(a[0]))


def tree(t: Tools, a: List[str]) -> int:
    return t.say(t.trees.tree(a[0], a[1:]))


def tree_judgements(t: Tools, a: List[str]) -> int:
    t.trees.tree_judgements(a[0], a[1])
    return 0


def hash_of(t: Tools, a: List[str]) -> int:
    return t.say(tools_hash(t.ctx.repo, a))


def classpath(t: Tools, a: List[str]) -> int:
    t.answer.write(t.classpaths.classpath(a[0]))
    t.answer.flush()
    return 0


def classpath_fresh(t: Tools, a: List[str]) -> int:
    return 0 if t.classpaths.fresh(a[0]) else 1


def line_of(t: Tools, a: List[str]) -> int:
    line = classpath_line(t.ctx.files.read_text_or(a[0], ""))
    return t.say(line) if line else 1


def recording(t: Tools, a: List[str]) -> int:
    return t.say(t.recordings.recording(a[0], a[1], a[2] if len(a) > 2 else ""))


def recording_cached(t: Tools, a: List[str]) -> int:
    return t.say(t.recordings.cached(a[0], a[1]))


def recording_run(t: Tools, a: List[str]) -> int:
    return t.say(t.recordings.run_named(a[0], a[1]))


def recorded(t: Tools, a: List[str]) -> int:
    return 0 if t.recordings.recorded(a[0], a[1], a[2] if len(a) > 2 else "") else 1


def newest_recording(t: Tools, a: List[str]) -> int:
    return t.say(t.recordings.newest(a[0] if a else ""))
