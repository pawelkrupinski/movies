"""THE TWO SIDES of the identity takes check (identity_takes_check.py): each of a side's results -- its final takes, its
resolve, its corpus-shape findings -- fetched when stored under its own key, else made (identity_takes_check_runs.py)
and stored. The base's final takes replay BESIDE the head's session (they read nothing the head writes); its resolve and
corpus shape follow the session, whose heap they could not share. THE BASE ROW (TAKES_SIDE=base) makes the base's side
alone, for the takes row (TAKES_SIDE=head) that waits for it."""
from __future__ import annotations

import os
import shutil
import tarfile
from dataclasses import dataclass
from typing import Callable, List, Optional

import identity_takes_check_context as ctx_module
import identity_takes_check_runs as runs
import identity_takes_check_store as store_module

Context = ctx_module.Context


@dataclass
class State:
    base: str
    side: str                 # both, head or base
    aa: bool
    takes_moves: bool
    shape_moves: bool
    store: store_module.Store
    keys: store_module.Keys
    traces: str
    tree: str                 # the base's checkout
    final_tree: str           # the base's final takes' (the base's own, beside a session that is not this row's)
    model: int = 0
    final: int = 0
    shape: int = 0


class Base:
    """The base's side: each result reused or made in the base's checkout."""

    def __init__(self, ctx: Context, state: State) -> None:
        self.ctx = ctx
        self.s = state
        self.replay: Optional[runs.Replay] = None

    def checkout(self, directory: str) -> None:
        if not runs.checkout(self.ctx, directory, self.s.base):
            raise ctx_module.Stop(2)

    def final_asset(self) -> str:
        return self.s.keys.asset("final", self.s.keys.capture(self.s.base))

    def decisions_asset(self) -> str:
        return self.s.keys.asset("decisions", self.s.keys.decisions(self.s.base))

    def final_reused_or_started(self) -> None:
        self.ctx.restart()
        if self.s.store.reused(self.final_asset(), self.ctx.path("base", "ratchet-takes.tsv"), "the base final takes"):
            self.ctx.say(f"{self.ctx.label}: base final takes reused")
            self.ctx.timing("base-final", "reused")
            self.s.store.final_gaps("base", self.s.base)
        else:
            self.final_start()

    def final_start(self) -> None:
        """The base's final takes, replayed in the background (final_finish waits for them)."""
        self.checkout(self.s.final_tree)
        runs.copy_captures(self.ctx, self.s.final_tree)
        self.replay = runs.Replay(self.ctx, self.s.final_tree, self.ctx.path("base", "ratchet-takes.tsv"),
                                  os.path.join(self.s.traces, "base-final.jsonl.gz"))

    def final_finish(self) -> None:
        if self.replay is None:
            return
        replay, self.replay = self.replay, None
        if not replay.finish(self.ctx):
            self.s.final = 2
        self.ctx.timing("base-final", "replayed", int(self.ctx.clock() - replay.started))
        if self.s.final == 0:
            self.s.store.keep_final("base", self.s.base, base=True)

    def resolve(self) -> None:
        self.checkout(self.s.tree)
        if not runs.dump(self.ctx, self.s.tree, self.ctx.path("base"), os.path.join(self.s.traces, "base-model.jsonl.gz")):
            self.ctx.say(f"::warning::{self.ctx.label}: the base resolved nothing")
            raise ctx_module.Stop(2)
        self.ctx.say(f"{self.ctx.label}: base compiled and resolved in {self.ctx.elapsed()}s")
        self.ctx.timing("base-resolve", "resolved")
        self.s.store.keep(self.ctx.path("base", f"decisions-{self.ctx.cc}.jsonl"), self.decisions_asset(), base=True)

    def resolve_had(self, fetch: Callable[[str, str, str], bool]) -> None:
        """The base's decisions, fetched (`reused` or `awaited`) or resolved."""
        self.ctx.restart()
        asset = self.decisions_asset()
        if fetch(asset, self.ctx.path("base", f"decisions-{self.ctx.cc}.jsonl"), "the base resolve"):
            if fetch == self.s.store.awaited:
                self.ctx.say(f"{self.ctx.label}: base resolve {self.s.store.how} from {asset} in {self.ctx.elapsed()}s")
            self.ctx.timing("base-resolve", self.s.store.how)
        else:
            self.resolve()

    def shape_asset(self) -> str:
        return self.s.keys.asset("shape", self.s.keys.shape(self.s.base))

    def shape_made(self) -> None:
        """The base's corpus-shape findings into <out>/base/shape (copies of the head's when the base wrote none)."""
        self.checkout(self.s.tree)
        spec = os.path.join(self.s.tree, "e2e/src/test/scala/services/movies/CorpusShapeSpec.scala")
        found = self.ctx.path("base", "shape")
        if "corpus-shape-" not in ctx_module.read(spec):
            self.ctx.verdict(f"{self.ctx.label}: the base predates the corpus-shape findings files -- the head's are "
                             "reported, not judged")
            os.makedirs(found, exist_ok=True)
            for name in listing(self.ctx.path("head", "shape")):
                if name.endswith(".txt"):
                    shutil.copy(self.ctx.path("head", "shape", name), found)
            return
        if not runs.shape_run(self.ctx, self.s.tree, found):
            self.s.shape = 2
        self.ctx.timing("base-shape", "ran")
        if self.s.shape == 0:
            pack(found, self.ctx.path("base", "shape.tar"))
            self.s.store.keep(self.ctx.path("base", "shape.tar"), self.shape_asset(), base=True)

    def shape_stored(self, fetch: Callable[[str, str, str], bool]) -> bool:
        if not fetch(self.shape_asset(), self.ctx.path("base", "shape.tar"), "the base corpus-shape findings"):
            return False
        unpack(self.ctx.path("base", "shape.tar"), self.ctx.path("base", "shape"))
        return True

    def row(self) -> int:
        """THE BASE ROW (TAKES_SIDE=base): the base's side alone, stored for this run's takes row -- no head, no verdict."""
        if not self.s.store.store:
            self.ctx.say(f"{self.ctx.label}: no store (TAKES_STORE empty, or a fill laid unpublished) -- the takes row "
                         "resolves the base itself")
            return 0
        os.makedirs(self.ctx.path("base"), exist_ok=True)
        if self.s.takes_moves:
            self.final_reused_or_started()
            self.resolve_had(self.s.store.reused)
            self.final_finish()
            self.s.store.keep_trace(self.s.traces, "base", self.s.base, base=True)
        if self.s.shape_moves:
            self.ctx.restart()
            if self.shape_stored(self.s.store.reused):
                self.ctx.timing("base-shape", "reused")
            else:
                self.shape_made()
        remove_worktrees(self.ctx, self.s.tree, self.s.final_tree)
        self.ctx.verdict(f"{self.ctx.label}: the base's side of {self.s.base[:9]} is stored for the takes row")
        return 0 if self.s.final == 0 and self.s.shape == 0 else 2


def listing(directory: str) -> List[str]:
    return sorted(os.listdir(directory)) if os.path.isdir(directory) else []


def pack(directory: str, tar: str) -> None:
    with tarfile.open(tar, "w") as out:
        out.add(directory, arcname=".")


def unpack(tar: str, directory: str) -> None:
    os.makedirs(directory, exist_ok=True)
    with tarfile.open(tar) as packed:
        packed.extractall(directory)


def remove_worktrees(ctx: Context, *directories: str) -> None:
    for directory in directories:
        if os.path.isdir(directory):
            ctx.git.ok("worktree", "remove", "--force", directory)
