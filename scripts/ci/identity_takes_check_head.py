"""THE HEAD'S SIDE of the identity takes check (identity_takes_check.py). A head is a base's commit as often as not, so
each of its results is fetched when stored under its own key, exactly as a base's is, and only the rest are made: none
left, and the head runs no sbt at all. Its final takes replay in a checkout of their own BESIDE its resolve; its resolve
and corpus shape run in ONE sbt session (each its own sbt was a JVM start, a project load and a cold JIT apiece), each
command's "[success]" saying it ran -- the resolve first, the corpus shape last (its specs FAIL on a finding)."""
from __future__ import annotations

import os
import re
import shutil
from typing import List, Optional

import identity_takes_check_context as ctx_module
import identity_takes_check_runs as runs
import identity_takes_check_sides as sides
import tool_process

Context = ctx_module.Context
ANSI = re.compile(r"\x1b\[[0-9;]*m")
SESSION_SHOWN = re.compile(r"\[full-|\[ratchet\]|error|Total time")
TOOK = re.compile(r"^\[success\] Total time: ([0-9]*) s")
POPULATION = re.compile(r"^kind=identity\.population_flips ")


class Head:
    def __init__(self, ctx: Context, state: sides.State, base: sides.Base) -> None:
        self.ctx = ctx
        self.s = state
        self.base = base
        self.final_reused = self.resolve_reused = self.shape_reused = False
        self.replay: Optional[runs.Replay] = None
        self.final_tree = os.path.join(ctx.temp, "identity-takes-head-final")
        self.names: List[str] = []
        self.took: List[str] = []
        self.session = 0

    def asset(self, kind: str) -> str:
        keys, head = self.s.keys, self.ctx.head
        key = {"final": keys.capture, "decisions": keys.decisions, "shape": keys.shape}[kind](head)
        return keys.asset(kind, key)

    def stored(self) -> None:
        """THE HEAD'S OWN RESULTS, fetched when stored."""
        os.makedirs(self.ctx.path("head"), exist_ok=True)
        store = self.s.store
        if self.s.takes_moves:
            if store.reused(self.asset("final"), self.ctx.path("head", "ratchet-takes.tsv"), "the head final takes"):
                self.final_reused = True
                store.final_gaps("head", self.ctx.head)
                self.ctx.say(f"{self.ctx.label}: head final takes reused")
            decisions = self.asset("decisions")
            if store.reused(decisions, self.ctx.path("head", f"decisions-{self.ctx.cc}.jsonl"), "the head resolve"):
                self.resolve_reused = True
                self.ctx.say(f"{self.ctx.label}: head resolve reused from {decisions}")
        if self.s.shape_moves and store.reused(self.asset("shape"), self.ctx.path("head", "shape.tar"),
                                               "the head corpus-shape findings"):
            shutil.rmtree(self.ctx.path("head", "shape"), ignore_errors=True)
            sides.unpack(self.ctx.path("head", "shape.tar"), self.ctx.path("head", "shape"))
            self.shape_reused = True
            self.ctx.say(f"{self.ctx.label}: head corpus-shape findings reused")

    def final_start(self) -> None:
        """The head's final takes in a checkout of its own, beside the session (they read only code and capture)."""
        if not self.s.takes_moves or self.final_reused:
            return
        shutil.rmtree(self.final_tree, ignore_errors=True)
        self.ctx.git.ok("worktree", "prune")
        if not runs.checkout(self.ctx, self.final_tree, self.ctx.head):
            self.ctx.say(f"::warning::{self.ctx.label}: could not check the head out for its final takes")
            self.s.final = 2
            return
        runs.copy_captures(self.ctx, self.final_tree)
        self.replay = runs.Replay(self.ctx, self.final_tree, self.ctx.path("head", "ratchet-takes.tsv"),
                                  os.path.join(self.s.traces, "head-final.jsonl.gz"))

    def commands(self) -> List[str]:
        out: List[str] = []
        if self.s.takes_moves and not self.resolve_reused:
            self.names.append("resolve")
            # the head's resolve also walks its families along arrival chains (report only)
            out.append(self.resolve_command())
        if self.s.shape_moves and not self.shape_reused:
            self.names.append("shape")
            out.append(self.ctx.env.get("SHAPE_COMMAND", ""))
        return out

    def resolve_command(self) -> str:
        """The resolve's command line, which also walks its families along arrival chains (report only)."""
        resolve = runs.RESOLVE.format(out=self.ctx.path("head"), cc=self.ctx.cc, corpus=runs.corpus(self.ctx),
                                      fixtures=self.ctx.fixtures())
        population = (f" --population {self.ctx.setting('TAKES_POPULATION_CHAINS', '4')} --population-seconds "
                      f"{self.ctx.setting('TAKES_POPULATION_SECONDS', '300')}")
        return resolve + population + runs.traced(os.path.join(self.ctx.here, runs.RESOLVE_TOOL),
                                                  os.path.join(self.s.traces, "head-model.jsonl.gz"))

    def session_heap(self) -> str:
        """Beside the head's ratchet and the row's Mongo, on a 16 GB runner: at most TAKES_SESSION_HEAP_BESIDE (8g)."""
        heap, beside = self.ctx.setting("HEAP", "6g"), self.ctx.setting("TAKES_SESSION_HEAP_BESIDE", "8g")
        try:
            if self.replay is not None and int(heap[:-1] if heap.endswith("g") else heap) > int(
                    beside[:-1] if beside.endswith("g") else beside):
                return beside
        except ValueError:
            pass
        return heap

    def run_session(self) -> None:
        commands = self.commands()
        if not commands:
            if self.s.takes_moves or self.s.shape_moves:
                self.ctx.say(f"{self.ctx.label}: every head result was stored -- no head session")
            return
        self.clear_here()
        self.ctx.restart()
        log = self.ctx.path("head.log")
        self.session = self.ctx.runner.run(tool_process.Command(
            self.ctx.sbt(f"-J-Xmx{self.session_heap()}", *commands),
            self.ctx.with_env({"KINOWO_IDENTITY_CORPUS_DIR": runs.corpus(self.ctx)}), log=log, cwd=self.ctx.here))
        runs.show(self.ctx, log, SESSION_SHOWN)
        plain = [ANSI.sub("", line) for line in ctx_module.read(log).splitlines()]
        self.took = [m.group(1) for m in map(TOOK.match, plain) if m]
        self.ctx.say(f"{self.ctx.label}: the head's session ran {' '.join(self.names)} in {self.ctx.elapsed()}s "
                     f"(sbt exit {self.session})")
        for line in filter(POPULATION.match, plain):  # report only: families whose take returns to a value it left
            self.ctx.verdict(line)

    def clear_here(self) -> None:
        """What an earlier session left in the checkout, gone: its outputs say this one ran."""
        for name in ("ratchet-takes.tsv", "capture-gaps.tsv"):
            runs.remove(os.path.join(self.ctx.here, "target/identity-unmatched", name))
        for path in runs.findings(self.ctx, self.ctx.here):
            os.remove(path)

    def took_by(self, name: str) -> Optional[int]:
        """The seconds the session's <name> command took; None when it did not run (or not to success)."""
        if name in self.names and self.names.index(name) < len(self.took):
            return int(self.took[self.names.index(name)] or 0)
        return None

    def final_finish(self) -> None:
        if self.final_reused:
            self.ctx.timing("head-final", "reused", 0)
        elif self.replay is not None:
            replay, self.replay = self.replay, None
            ok = replay.finish(self.ctx)
            if ok and ctx_module.nonempty(self.ctx.path("head", "ratchet-takes.tsv")):
                self.ctx.timing("head-final", "replayed", int(self.ctx.clock() - replay.started))
                self.s.store.keep_final("head", self.ctx.head)
            else:
                self.s.final = 2  # the ratchet said why (its log: ratchet-takes.tsv.log)

    def resolve_finish(self) -> None:
        secs = self.took_by("resolve")
        if self.resolve_reused:
            self.ctx.timing("head-resolve", "reused", 0)
        elif secs is not None and ctx_module.nonempty(self.ctx.path("head", f"decisions-{self.ctx.cc}.jsonl")):
            self.ctx.timing("head-resolve", "resolved", secs)
            self.s.store.keep(self.ctx.path("head", f"decisions-{self.ctx.cc}.jsonl"), self.asset("decisions"))
            self.s.store.keep_trace(self.s.traces, "head", self.ctx.head)
        else:
            self.ctx.say(f"::warning::{self.ctx.label}: the resolve exited {self.session} (its log: {self.ctx.path('head.log')})")
            raise ctx_module.Stop(2)

    def takes(self) -> None:
        """After the session: the head's final takes and resolve, then the base's (awaited beside a base row)."""
        self.final_finish()
        self.resolve_finish()
        if self.s.side == "head":
            self.ctx.restart()
            if self.s.store.awaited(self.base.final_asset(), self.ctx.path("base", "ratchet-takes.tsv"), "the base final takes"):
                self.ctx.timing("base-final", self.s.store.how)
                self.s.store.final_gaps("base", self.s.base)
            else:
                self.base.final_start()  # beside the base's resolve below
        else:
            self.base.final_finish()
        self.base.resolve_had(self.s.store.awaited)
        self.base.final_finish()
        self.s.store.keep_trace(self.s.traces, "base", self.s.base, base=True)
        for side in ("head", "base"):
            gaps_of(self.ctx, side)

    def shape(self) -> None:
        """The corpus-shape findings of both sides, the head's from its session or the store."""
        shaped = 0
        if not self.shape_reused:
            runs.shape_found(self.ctx, self.ctx.here, self.ctx.path("head", "shape"))
        if not sides.listing(self.ctx.path("head", "shape")):
            self.s.shape = 2
        else:
            if self.shape_reused:
                self.ctx.timing("head-shape", "reused", 0)
            else:
                shaped = self.took_by("shape") or 0
                self.ctx.timing("head-shape", "ran", shaped)
                sides.pack(self.ctx.path("head", "shape"), self.ctx.path("head", "shape.tar"))
                self.s.store.keep(self.ctx.path("head", "shape.tar"), self.asset("shape"))
            self.ctx.restart()
            if self.base.shape_stored(self.s.store.awaited):
                self.ctx.say(f"{self.ctx.label}: base corpus-shape findings {self.s.store.how}")
                self.ctx.timing("base-shape", self.s.store.how)
            else:
                self.base.shape_made()
        self.ctx.say(f"{self.ctx.label}: corpus-shape specs ran in {shaped + self.ctx.elapsed()}s")


def gaps_of(ctx: Context, side: str) -> None:
    """This country's gaps the side's final takes read as unknown, said with what they were (up to 400 characters)."""
    rows = [line.split("\t") for line in ctx_module.read(ctx.path(side, "capture-gaps.tsv")).splitlines()[1:]]
    mine = [r for r in rows if r and r[0] == ctx.cc]
    if not mine:
        return
    named = "; ".join(f"{(r + ['', ''])[1]} {(r + ['', '', ''])[2]}" for r in mine)[:400]
    ctx.verdict(f"{ctx.label}: the {side}'s final takes read {len(mine)} lookup(s) the capture lacks as UNKNOWN -- a blind "
                f"spot of this measurement, not a failure (re-capture to close it): {named}", summary=True)
