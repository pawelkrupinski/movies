"""THE VERDICT RECORD (scripts/identity_what_if_cli.py): a line a country finished, under (head, base), written by
identity-tools.sh's what_if_record_verdict (scripts/identity-measure's what-if gate reads it, scripts/identity_stamp_carry.py
carries a stamp by it): the verdict, the takes moved (the model's and the final diff's rows), the mode (whole or
selected, final or model-only) and the run's PUBLISHED asset -- after every country the run uploads its lines so far
and the takes diffs behind them to the identity-what-if-verdicts release (identity_stamp_carry.py publish), best effort.
A `.` head counts as its commit only with no uncommitted edits to tracked files. And A PURE MOVE's `pure-move`
verdict a country (scripts/pure-move-check)."""
from __future__ import annotations

import datetime
import os
import sys
from typing import List, Sequence

from identity_what_if_cli_context import TOOL_FILES, Context, Exit, lines_of, own_files, tools_hash
from processes import machine  # (scripts/lib, too)
import tool_process  # (scripts/lib, on the path the context put it on)
from processes import tool_python  # (scripts/lib, too)


def record(ctx: Context, head: str, base: str, cc: str, verdict: str, *more: str) -> None:
    """identity-tools.sh's what_if_record_verdict: <takes diff> [<takes moved> <mode> <published asset>]."""
    ctx.shell("what_if_record_verdict", [head, base, cc, verdict, *more])


def rows_moved(files: Sequence[str]) -> int:
    """The rows of the diffs, their headers aside (`cat files | grep -cv '^country'`); a missing file reads empty."""
    lines: List[str] = []
    for path in files:
        if os.path.isfile(path):
            lines += lines_of(path)
    return sum(1 for line in lines if not line.startswith("country"))


class Verdicts:
    def __init__(self, ctx: Context, head: str, base: str, out: str) -> None:
        self.ctx, self.base, self.out = ctx, base, out
        self.head = head if head != "." or not ctx.clean_tree() else ctx.git("rev-parse", "HEAD").stdout.strip()
        stamp = datetime.datetime.fromtimestamp(machine.wall(), datetime.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        self.publish_args = [self.head, base, ",".join(ctx.o.ccs),
                             tools_hash(ctx.repo, [*TOOL_FILES, *own_files(ctx.repo)]), f"{stamp}-{ctx.pid}"]
        self.asset = ""
        if self.head != ".":
            done = self.carry("asset-name", self.publish_args)
            self.asset = done.stdout.rstrip("\n") if done.code == 0 else ""

    def carry(self, command: str, args: Sequence[str]) -> tool_process.Captured:
        return self.ctx.capture(tool_python.isolated("scripts/identity_stamp_carry.py", command, *args), cwd=self.ctx.repo)

    def recorded(self, cc: str, diff_code: int, whole: bool) -> None:
        """`cc`'s verdict recorded, and published when the run has an asset."""
        if self.head == ".":
            return
        files = [os.path.join(self.out, f"takes-diff-{cc}.tsv")]
        if self.ctx.o.final:
            files.append(os.path.join(self.out, f"final-diff-{cc}.tsv"))
        # a diff it did not write counts as unknown, never as none moved
        moved = str(rows_moved(files)) if all(os.path.isfile(f) for f in files) else "-"
        mode = f"{'whole' if whole else 'selected'},{'final' if self.ctx.o.final else 'model-only'}"
        record(self.ctx, self.head, self.base, cc, "done" if diff_code == 0 else "verify-failed", files[0], moved, mode,
               self.asset)
        if not self.asset:
            return
        done = self.carry("publish", self.publish_args)
        sys.stderr.write(done.stdout)
        if done.code != 0:
            self.ctx.log(f"{cc}: the verdict is recorded but not published")


def pure_move(ctx: Context) -> bool:
    """A range that only moves code (scripts/pure-move-check) resolves nothing: True when it did, its verdicts
    recorded. Not for --bisect, nor a `.` head with uncommitted edits (the check reads commits)."""
    o = ctx.o
    if o.bisect_good or ctx.env.get("KINOWO_PURE_MOVE_CHECKED"):
        return False
    head = o.head if o.head != "." else ("HEAD" if ctx.clean_tree() else "")
    if head and pure_moved(ctx, head):
        return True
    ctx.env["KINOWO_PURE_MOVE_CHECKED"] = "1"  # the vault's re-run of this tool asks no second time
    return False


def pure_moved(ctx: Context, head: str) -> bool:
    """base..head checked (scripts/pure-move-check): a pure move's verdicts recorded, else said."""
    o = ctx.o
    check = [os.path.join(ctx.repo, "scripts/pure-move-check"), f"{o.base}..{head}", "--quiet"]
    done = ctx.runner.capture(tool_process.Command(["sh", "-c", 'exec "$@" 2>&1', "sh", *check], ctx.env))
    if done.code != 0:
        said = done.stdout.rstrip("\n").split("\n")[-1]
        ctx.log(f"not a pure move (scripts/pure-move-check exit {done.code}: {said}): resolving as before")
        return False
    print("PURE MOVE: what-if skipped (scripts/pure-move-check)", flush=True)
    base, sha = ctx.commit_of(o.base), ctx.commit_of(head)
    if base is None or sha is None:
        raise Exit(2)
    for cc in o.ccs:
        record(ctx, sha, base, cc, "pure-move", "-")
    return True
