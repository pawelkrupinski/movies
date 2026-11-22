"""ONE COUNTRY'S WHAT-IF (scripts/identity_what_if_cli.py): its mirror fenced and its generation pinned, the base's
whole side read (kept) or resolved -- the head compiling beside it --, the head re-resolving the families selected (or
none: no head JVM), the mirror checked unchanged after each side, the fence lingering, the takes diffed
(identity_what_if.py diff), a head of the base's very code moving nothing (a control), the verdict recorded."""
from __future__ import annotations

import os
import shutil
import subprocess
from dataclasses import dataclass
from typing import Callable, List, Optional

from identity_what_if_cli_bisect import minute_now
from identity_what_if_cli_context import Context, Exit, lines_of, write
from identity_what_if_cli_mirror import Mirror
from identity_what_if_cli_resolve import Resolver, read
from identity_what_if_cli_sides import SideKey, Sides
from identity_what_if_cli_verdict import Verdicts, rows_moved
import tool_process  # (scripts/lib, on the path the context put it on)

Starter = Callable[[tool_process.Command], "subprocess.Popen[bytes]"]


@dataclass
class Change:
    """The change measured: the base and head, the head's tree, where its diffs go, and how the head resolves."""
    base: str
    head: str
    head_label: str
    head_tree: str
    out: str
    judgements: str
    all: bool
    same_code: bool
    head_resolves: bool
    answers: str
    answer_args: List[str]


class HeadCompile:
    """The head's compile, started beside the base's resolve when the base side is not kept (its own JVM slot); waited
    for before the head resolves -- and on every exit before that: left running, it outlived the tool, holding its JVM
    slot, and wrote into the head's tree while the next run of that ref laid it out again (Order independence #90)."""

    def __init__(self, ctx: Context, resolver: Resolver, start: Starter) -> None:
        self.ctx, self.resolver, self.start = ctx, resolver, start
        self.proc: "Optional[subprocess.Popen[bytes]]" = None

    def beside(self, change: Change) -> None:
        if not change.head_resolves or self.proc is not None or self.resolver.classpath_fresh(change.head_tree):
            return
        self.ctx.log("compiling the head beside the base's resolve")
        self.proc = self.start(self.ctx.shell_command("identity_classpath", [change.head_tree]))

    def done(self) -> None:
        if self.proc is not None:
            self.proc.wait()
            self.proc = None


def families(takes: str) -> List[str]:
    """The families of a side's takes.tsv: its first column, its header aside."""
    return [line.split("\t")[0] for line in lines_of(takes)[1:]]


class Country:
    def __init__(self, ctx: Context, change: Change, sides: Sides, key: SideKey, mirror: Mirror,
                 compile_: HeadCompile, verdicts: Verdicts) -> None:
        self.ctx, self.change, self.sides, self.key, self.mirror = ctx, change, sides, key, mirror
        self.compile, self.verdicts = compile_, verdicts
        self.cc = ""

    def clock(self) -> float:
        return float(f"{self.ctx.clock():.1f}")

    def since(self, started: float) -> str:
        return f"{self.ctx.clock() - started:.0f}"

    def run(self, cc: str) -> int:
        """`cc`'s what-if: the diff's status."""
        self.cc = self.ctx.cc = cc
        self.mirror.fence(cc)
        gen = self.key.mirror_gen = self.mirror.ready(cc)
        started = self.clock()
        snapshot = self.ctx.o.at or self.sides.cached_at(cc, self.change.base) or minute_now()
        kept = self.base_kept(snapshot)
        if not kept:
            self.compile.beside(self.change)
        base_dir = self.sides.whole(self.change.base, cc, snapshot)
        self.unchanged(gen, "after the base", base_dir, kept)
        base_seconds = self.since(started)
        head_dir = os.path.join(self.change.out, cc)
        shutil.rmtree(head_dir, ignore_errors=True)
        os.makedirs(head_dir)
        selected, whole = self.select(base_dir, head_dir)
        head_started = self.clock()
        self.head_side(snapshot, base_dir, head_dir)
        self.unchanged(gen, "after the head", base_dir, kept)
        # both sides read: this country's fence lingers now, not once every later country is done too
        self.mirror.linger()
        head_seconds = self.since(head_started)
        rc = self.diff(base_dir, head_dir)
        self.verdicts.recorded(cc, rc, whole)
        self.summary(selected, base_dir, head_dir)
        print(f"  snapshot {snapshot}; answers {self.change.answers}; wall: base {base_seconds}s "
              f"({'kept' if kept else 'resolved'}), head {head_seconds}s, total {self.since(started)}s; rows "
              f"{self.change.out}/takes-diff-{cc}.tsv")
        return rc

    def base_kept(self, snapshot: str) -> bool:
        side = os.path.join(self.sides.root(self.cc, self.change.base), snapshot)
        return os.path.isfile(os.path.join(side, "final.tsv")) or (
            not self.ctx.o.final and os.path.isfile(os.path.join(side, "prod.tsv")))

    def unchanged(self, gen: str, when: str, base_dir: str, kept: bool) -> None:
        """Exit 2 when the mirror changed: a base resolved here off a copy that changed under it is not kept."""
        if not self.mirror.unchanged(self.cc, gen, when):
            if not kept:
                shutil.rmtree(base_dir, ignore_errors=True)
            raise Exit(2)

    def select(self, base_dir: str, head_dir: str) -> "tuple[str, bool]":
        """The families the head re-resolves (select.txt), said; and whether that is every family (a whole verdict)."""
        select, takes = os.path.join(head_dir, "select.txt"), os.path.join(base_dir, "takes.tsv")
        o, change = self.ctx.o, self.change
        if change.all:
            write(select, "".join(f + "\n" for f in families(takes)))
            selected = "selected: ALL (--all)"
        else:
            done = self.ctx.capture(self.ctx.what_if_py(["select", "--repo", self.ctx.repo, "--base", change.base,
                                                         "--head", change.head, "--takes", takes, "--out", select]))
            if done.code != 0:
                raise Exit(2)
            selected = done.stdout.rstrip("\n")
        self.ctx.log(f"{self.cc}: {selected}")
        # a selection of every family is a whole resolve too: what the verdict record says, and the stamp carry reads
        whole = change.all or o.verify or set(lines_of(select)) == set(families(takes))
        return selected, whole

    def head_side(self, snapshot: str, base_dir: str, head_dir: str) -> None:
        select = os.path.join(head_dir, "select.txt")
        if os.path.getsize(select) == 0 and not self.ctx.o.verify:
            # nothing to re-resolve: what WhatIfResolve writes for an empty selection, without a compile or a JVM
            self.ctx.log(f"{self.cc}: no family to re-resolve: the head's takes are the base's (no head JVM)")
            write(os.path.join(head_dir, "takes.tsv"), "key\tvenue\trawTitle\tfamily\ttake\trules\n")
            write(os.path.join(head_dir, "region.tsv"), "selected\tre-resolved\tgroups\trounds\n0\t0\t0\t1\n")
            if self.ctx.o.final:
                write(os.path.join(head_dir, "final.tsv"), "key\tfinal\tbasis\n")
            write(os.path.join(head_dir, "answers.txt"), "\n")
            return
        self.compile.done()
        o = self.ctx.o
        args = ["--at", snapshot, "--select", select, "--families", os.path.join(base_dir, "families.tsv"),
                "--base-takes", os.path.join(base_dir, "takes.tsv"), "--threads", o.threads, *self.change.answer_args,
                *(["--final"] if o.final else []), *(["--whole"] if o.verify else []), "--mirror-generation",
                self.key.mirror_gen]
        if not self.sides.resolver.resolve(self.change.head_tree, self.cc, head_dir, args, o.serve):
            raise Exit(2)
        if not self.sides.answers_ok(head_dir, "head"):
            raise Exit(2)

    def diff(self, base_dir: str, head_dir: str) -> int:
        o, cc, out = self.ctx.o, self.cc, self.change.out
        args = ["diff", "--cc", cc, "--base", base_dir, "--head", head_dir, "--judgements", self.change.judgements,
                "--tsv", os.path.join(out, f"takes-diff-{cc}.tsv"), *(["--verify"] if o.verify else []),
                *(["--json"] if o.json else [])]
        rc = self.ctx.runner.run(tool_process.Command(self.ctx.what_if_py(args), self.ctx.env))
        moved = rows_moved([os.path.join(out, f"takes-diff-{cc}.tsv"), os.path.join(out, f"final-diff-{cc}.tsv")])
        if self.change.same_code and moved > 0:
            # the base side is dropped whether kept or not: one of the two read other inputs, and a kept one is the suspect
            shutil.rmtree(base_dir, ignore_errors=True)
            self.ctx.harness_error(
                f"{cc}: the same code moved {moved} take(s) (head {self.change.head_label} = base "
                f"{self.change.base[:12]}): the sides read different inputs -- a mirror changed under a kept base side "
                "or its fence, or live answers one side had and the other did not (answers above); nothing recorded, "
                "the base side dropped -- run again")
            raise Exit(2)
        return rc

    def summary(self, selected: str, base_dir: str, head_dir: str) -> None:
        region = lines_of(os.path.join(head_dir, "region.tsv"))
        f = (region[-1].split("\t") if region else []) + ["", "", "", ""]
        print(f"  {selected}; re-resolved {f[1]} listings in {f[2]} group(s), {f[3]} round(s)")
        for side, name in ((base_dir, "base"), (head_dir, "head")):
            answers = read(os.path.join(side, "answers.txt"))
            if answers:
                print(f"  answers asked live ({name}): {answers}")
