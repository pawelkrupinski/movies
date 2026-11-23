"""identity-slice's drift modes (scripts/identity_slice_cli.py): --drift (the WHOLE recording through production's
incremental sequence, scripts.IdentityDrift) and --emit-drift-slice (each slice emitted, its cut CLOSED by the
incremental sequence's own reads, replayed, then its ARRIVAL WALK filed as population rows of identity-flip-backlog.tsv).
A replay's verdict is its own explicit `## DRIFT VERDICT:` line, never its exit status alone: a JVM that dies (an OOM, a
kill, a timeout) exits non-zero too, and was once read as RED."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
from typing import List, Tuple

from identity_slice_cli_context import Context, Exit, cat, say
import tool_python  # (scripts/lib, on the path the context put it on)
import tool_shell  # (scripts/lib, on the path the context put it on)

DRIFT = "scripts.IdentityDrift"
WALK = "services.identity.PopulationMonotonicitySpec"


def verdict_of(path: str) -> str:
    """The replay's last `## DRIFT VERDICT: ` line's verdict."""
    with open(path, errors="replace") as f:
        found = [line[len("## DRIFT VERDICT: "):].rstrip("\n") for line in f if line.startswith("## DRIFT VERDICT: ")]
    return found[-1] if found else ""


def mentions(text: str, *paths: str) -> bool:
    for path in paths:
        try:
            with open(path, errors="replace") as f:
                if text in f.read():
                    return True
        except OSError:
            pass
    return False


class Drift:
    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx
        self.allow = f"{ctx.repo}/test/resources/fixtures/identity-drift-allow.tsv"

    def whole(self) -> int:
        o, ctx = self.ctx.o, self.ctx
        tree = ctx.tree_of(o.refs[0] if o.refs else ".")
        cp = ctx.classpath(tree)
        if cp is None:
            return 1
        rc = ctx.java(tree, "-", "-", ["-cp", cp, DRIFT, o.cc, "--recording", ctx.recording, "--families", o.families,
                                       "--labels", o.labels, "--allow", self.allow, "--out", ctx.path("drift")])
        kept = ctx.path(f"drift/drift-{o.cc}.tsv")
        if o.keep and os.path.isfile(kept):
            shutil.copyfile(kept, o.keep)
        return rc

    def emit(self) -> int:
        """Each slice the first stage emitted (its `--emit-slice: <dir>` lines), closed, replayed and walked."""
        o, ctx = self.ctx.o, self.ctx
        if not (len(o.refs) == 1 and o.refs[0] == "."):
            say("--emit-drift-slice cuts with this checkout's code: no --ref")
            return 2
        cat(ctx.path("0.txt"))
        cp = ctx.classpath(ctx.repo)
        if cp is None:
            return 1
        with open(ctx.path("0.txt"), errors="replace") as f:
            dirs = [line[len("--emit-slice: "):].rstrip("\n") for line in f if line.startswith("--emit-slice: ")]
        for slice_dir in dirs:
            self.closed(cp, slice_dir)
            self.walked(cp, slice_dir)
        if not dirs:
            ctx.log("no drift slice emitted (no listing matched?)")
            return 1
        return 0

    def closed(self, cp: str, slice_dir: str) -> None:
        """The cut CLOSED (IdentityDrift.cut): widened by the incremental sequence's reads and replayed from its answers
        alone, round after round, until it lacks no request and finds what the recording behind it finds -- else exit 4."""
        o, ctx = self.ctx.o, self.ctx
        seeds = ctx.knob("KINOWO_DRIFT_SLICE_SEEDS", "12")
        rc = self.teed(["-cp", cp, DRIFT, o.cc, "--slice", slice_dir, "--seeds", seeds, "--allow", self.allow,
                        "--flips-allow", f"{ctx.repo}/test/resources/fixtures/identity-flips-allow.tsv",
                        "--out", ctx.path("drift"), "--widen", "--recording", ctx.recording, "--families", o.families])
        verdict = verdict_of(ctx.path("drift.out"))
        if rc == 4:
            ctx.log(f"{slice_dir}: CUT NOT CLOSED (above) -- no faithful cut: do not commit it (git checkout/clean it)")
            raise Exit(1)
        if (rc, verdict) not in ((0, "CLEAN"), (1, "RED")):
            ctx.tail(20, ctx.path("drift.log"))
            ctx.log(f"{slice_dir}: NO VERDICT: {self.cause(rc, seeds)}")
            raise Exit(5)
        said = "replays clean" if verdict == "CLEAN" else \
            "REPLAYS RED (above): the regression it guards is open -- commit it with the fix"
        ctx.log(f"drift slice {slice_dir}: closed, {said}")

    def cause(self, rc: int, seeds: str) -> str:
        if mentions("OutOfMemoryError", self.ctx.path("drift.log"), self.ctx.path("drift.out")):
            return f"the replay ran out of memory (KINOWO_DRIFT_SLICE_SEEDS={seeds}: fewer seeds or a bigger heap)"
        if rc >= 128:
            return f"the replay was killed or timed out (signal {rc - 128})"
        if rc == 3:
            return "the slice lacks answers (exit 3)"
        return f"the replay exited {rc} with no verdict line"

    def teed(self, java: List[str]) -> int:
        """The replay in this checkout, its stdout onto this tool's and into drift.out (`| tee`), its stderr into
        drift.log."""
        ctx = self.ctx
        command = tool_shell.shell_command(ctx.env, "scripts/identity-tools.sh", "identity_java_slotted_to",
                                           ["identity-slice", ctx.repo, "-", ctx.path("drift.log"), *java],
                                           prelude="IDENTITY_TOOL=identity-slice")
        sys.stdout.flush()
        with open(ctx.path("drift.out"), "wb") as out:
            proc = subprocess.Popen(list(command.argv), env=dict(command.env), stdout=subprocess.PIPE)
            assert proc.stdout is not None
            chunk = os.read(proc.stdout.fileno(), 65536)
            while chunk:
                sys.stdout.buffer.write(chunk)
                sys.stdout.buffer.flush()
                out.write(chunk)
                chunk = os.read(proc.stdout.fileno(), 65536)
            proc.stdout.close()
            return proc.wait()

    def walked(self, cp: str, slice_dir: str) -> None:
        """THE BACKLOG ROWS THE SLICE NEEDS, filed in the same step (Main #2515: a slice committed with its drift row
        alone failed PopulationMonotonicitySpec on main): the slice walked with this checkout's code, every flip no
        `population` row names appended to identity-flip-backlog.tsv. A walk that fails, or a rule contract breach,
        files nothing: exit 6. The walk names slices by their path under the checkout (Fixtures.relativize)."""
        ctx = self.ctx
        relative = slice_dir[len(ctx.repo) + 1:] if slice_dir.startswith(ctx.repo + "/") else slice_dir
        rc = ctx.java(ctx.repo, ctx.path("population.out"), ctx.path("population.log"), ["-cp", cp, WALK, relative])
        filed, ok = self.filed() if rc == 0 else ("", False)
        if not ok:
            ctx.tail(20, ctx.path("population.log"), ctx.path("population.out"))
            ctx.log(f"{slice_dir}: the population walk failed or breached the rule contract (above): no backlog row "
                    "filed -- PopulationMonotonicitySpec fails the slice on main")
            raise Exit(6)
        if filed:
            print(filed, flush=True)
            rows = sum(1 for line in filed.split("\n") if line)
            ctx.log(f"drift slice {slice_dir}: {rows} population row(s) filed in identity-flip-backlog.tsv (above): "
                    "commit them with the slice under the owner's named decision (Owner-Approved:), or fix the flips")
        else:
            ctx.log(f"drift slice {slice_dir}: its arrival walk finds no flip no population row names")

    def filed(self) -> Tuple[str, bool]:
        """scripts/ci/population_walk_diff.py's rows the walk's flips added to the backlog, and whether it ran clean."""
        ctx = self.ctx
        backlog = f"{ctx.repo}/test/resources/fixtures/identity-flip-backlog.tsv"
        done = ctx.capture(["sh", "-c", 'f=$1; shift; exec "$@" < "$f"', "sh", ctx.path("population.out"),
                            *tool_python.isolated(f"{ctx.repo}/scripts/ci/population_walk_diff.py", "rows", "--into",
                                                  backlog)])
        return done.stdout.rstrip("\n"), done.code == 0
