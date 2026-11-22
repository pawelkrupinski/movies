"""The sbt runs of the identity takes check (identity_takes_check.py), each in a checkout: the RESOLVE (its decisions),
the FINAL TAKES (the unmatched-cluster ratchet replaying the model and the agreement stage over the checked-in capture,
its lookups the capture lacks read as UNKNOWN and listed: --measure), and THE CORPUS SHAPE (the specs' findings files,
their exit no verdict: a finding fails the specs themselves). A checkout from before a resolve or ratchet was a tool (a
base, often) runs it as the spec it was; one from before --countries replays every capture.

The base is resolved in a worktree of its own beside the checkout (the row overwrote tracked corpus fixtures with the
recording), seeded with the head's compiled classes so zinc compiles only what differs. A final-takes replay runs in the
BACKGROUND, in a checkout of its own (two sbt runs in one checkout compile into one target), beside the session."""
from __future__ import annotations

import glob
import os
import re
import shutil
import threading
from typing import Callable, List, Optional

import identity_takes_check_context as ctx_module
import tool_process

Context = ctx_module.Context
RESOLVE_TOOL = "worker/src/it/scala/IdentityResolveDump.scala"
RATCHET_TOOL = "worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala"
# One line, no quote inside: IntegrationToolCommandLinesSpec parses every runMain the scripts type against the tool's own
RESOLVE = "worker/IntegrationTest/runMain integration.IdentityResolveDump --out {out} --full {cc} --corpus-dir {corpus} --fixtures {fixtures}"
SHOWN = re.compile(r"\[full-|error|Total time")


def traced(tool_source: str, trace: str) -> str:
    """` --trace <file>` when the tool takes one (a base from before it does not) and a file is named."""
    return f" --trace {trace}" if trace and '"trace"' in ctx_module.read(tool_source) else ""


def corpus(ctx: Context) -> str:
    return ctx.fixtures("corpus")


def dump(ctx: Context, checkout: str, out: str, trace: str = "") -> bool:
    """The resolve of one checkout over the recording: its exit and its decisions both say it ran."""
    heap = ctx.setting("HEAP", "6g")
    log = out + ".log"
    if os.path.isfile(os.path.join(checkout, RESOLVE_TOOL)):
        command = RESOLVE.format(out=out, cc=ctx.cc, corpus=corpus(ctx), fixtures=ctx.fixtures()) + traced(
            os.path.join(checkout, RESOLVE_TOOL), trace)
        env = dict(ctx.env)
    else:
        command = "worker/IntegrationTest/testOnly integration.IdentityResolveDumpIntegrationSpec"
        env = ctx.with_env({"KINOWO_IDENTITY_DUMP": out, "KINOWO_IDENTITY_FULL": ctx.cc,
                            "KINOWO_IDENTITY_CORPUS_DIR": corpus(ctx), "KINOWO_FIXTURE_ROOT": ctx.fixtures()})
    status = ctx.runner.run(tool_process.Command(ctx.sbt(f"-J-Xmx{heap}", command), env, log=log, cwd=checkout))
    show(ctx, log, SHOWN)
    if status != 0:
        ctx.say(f"::warning::{ctx.label}: the resolve exited {status} (its log: {log})")
        return False
    return ctx_module.nonempty(os.path.join(out, f"decisions-{ctx.cc}.jsonl"))


def show(ctx: Context, log: str, pattern: "re.Pattern[str]") -> None:
    for line in ctx_module.read(log).splitlines():
        if pattern.search(line):
            ctx.say(line)


def ratchet(ctx: Context, checkout: str, out_file: str, trace: str, say: Callable[[str], None]) -> bool:
    """The final takes replayed in <checkout>, copied to <out_file> (its capture gaps beside it): whether they were."""
    target = os.path.join(checkout, "target/identity-unmatched")
    for name in ("ratchet-takes.tsv", "capture-gaps.tsv"):
        remove(os.path.join(target, name))
    os.makedirs(os.path.dirname(out_file), exist_ok=True)
    tool = os.path.join(checkout, RATCHET_TOOL)
    heap = ctx.setting("TAKES_RATCHET_HEAP", "3g")
    log = out_file + ".log"
    if os.path.isfile(tool):
        scope = (f" --countries {ctx.cc}" if '"countries"' in ctx_module.read(tool) else "") + traced(tool, trace)
        # unforked, whatever the base's build says: a forked tool in worker/ misses the repo-relative judgements (run 195)
        status = ctx.runner.run(tool_process.Command(
            ["sbt", f"-J-Xmx{heap}", "set worker / Test / run / fork := false", "set worker / Test / runMain / fork := false",
             f"worker/Test/runMain scripts.UnmatchedClustersRatchet --measure --out target/identity-unmatched{scope}"],
            ctx.env, log=log, cwd=checkout))
        if status != 0:
            say(f"::warning::{ctx.label}: the ratchet exited {status} ({log})")
            return False
    else:
        ctx.runner.run(tool_process.Command(["sbt", f"-J-Xmx{heap}", "worker/Test/testOnly services.identity.UnmatchedClustersRatchetSpec"],
                                            ctx.with_env({"KINOWO_RATCHET_CAPTURE_GAPS_UNKNOWN": "1"}), log=log, cwd=checkout))
    copy(os.path.join(target, "capture-gaps.tsv"), os.path.join(os.path.dirname(out_file), "capture-gaps.tsv"))
    return copy(os.path.join(target, "ratchet-takes.tsv"), out_file)


def shape_found(ctx: Context, checkout: str, out: str) -> bool:
    """The findings a shape run in <checkout> wrote, copied into <out>: whether there were any."""
    os.makedirs(out, exist_ok=True)
    found = findings(ctx, checkout)
    for path in found:
        shutil.copy(path, out)
    return bool(found)


def findings(ctx: Context, checkout: str) -> List[str]:
    return sorted(glob.glob(os.path.join(checkout, f"target/convergence-diagnostics/corpus-shape-recorded-{ctx.cc}-*.txt")))


def shape_run(ctx: Context, checkout: str, out: str) -> bool:
    for path in findings(ctx, checkout):
        os.remove(path)
    os.makedirs(out, exist_ok=True)
    ctx.runner.run(tool_process.Command(
        ctx.sbt(f"-J-Xmx{ctx.setting('HEAP', '6g')}", ctx.env.get("SHAPE_COMMAND", "")),
        ctx.with_env({"KINOWO_IDENTITY_CORPUS_DIR": corpus(ctx)}), log=out + ".log", cwd=checkout))
    return shape_found(ctx, checkout, out)


def checkout(ctx: Context, directory: str, sha: str) -> bool:
    """<sha> checked out in a worktree at <directory>, once, seeded with the head's compiled classes and zinc state
    (the sbt-target-cache's paths: classes and analysis, no jars)."""
    if os.path.isdir(directory):
        return True
    if not ctx.git.ok("worktree", "add", "-q", "--detach", directory, sha):
        return False
    targets = ["project/target", "project/project/target"] + [
        os.path.relpath(p, ctx.here) for p in sorted(glob.glob(os.path.join(ctx.here, "*/target/scala-*")))]
    for relative in targets:
        source = os.path.join(ctx.here, relative)
        if os.path.isdir(source):
            shutil.copytree(source, os.path.join(directory, relative), symlinks=True, dirs_exist_ok=True)
    return True


def copy_captures(ctx: Context, directory: str) -> None:
    """The head's capture copied into another checkout: a base's final takes read the head's capture, one input."""
    target = os.path.join(directory, "test/resources/fixtures/identity-unmatched")
    for path in glob.glob(ctx.fixtures("identity-unmatched", "*.json.gz")) + glob.glob(ctx.fixtures("identity-unmatched",
                                                                                                   "*.inputs")):
        if os.path.isdir(target):
            shutil.copy(path, target)


def copy(source: str, dest: str) -> bool:
    try:
        shutil.copy(source, dest)
    except OSError:
        return False
    return True


def remove(path: str) -> None:
    if os.path.exists(path):
        os.remove(path)


class Replay:
    """A final-takes replay in the background: its lines kept to be said when it is waited for."""

    def __init__(self, ctx: Context, directory: str, out_file: str, trace: str) -> None:
        self.lines: List[str] = []
        self.ok: Optional[bool] = None
        self.started = ctx.clock()
        self.thread = threading.Thread(target=self._run, args=(ctx, directory, out_file, trace), daemon=True)
        self.thread.start()

    def _run(self, ctx: Context, directory: str, out_file: str, trace: str) -> None:
        self.ok = ratchet(ctx, directory, out_file, trace, self.lines.append)

    def finish(self, ctx: Context) -> bool:
        self.thread.join()
        for line in self.lines:
            ctx.say(line)
        return bool(self.ok)
