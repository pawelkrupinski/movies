"""identity-slice's run (scripts/identity_slice_cli.py): a log line read into its listing and build, the options checked,
the mode chosen -- --explain, --faithfulness, --drift, --bisect -- else each ref decided on ONE recording, its takes
diffed (scripts/identity_slice_diff.py) and tallied against the judgements, or the slices emitted as drift slices."""
from __future__ import annotations

import json
import os
import shutil
import tempfile
from typing import List

from identity_slice_cli_context import Context, Exit, cat, say
from identity_slice_cli_drift import Drift
from identity_slice_cli_keys import Keys
from identity_slice_cli_modes import Modes
from identity_slice_cli_options import Usage
from identity_slice_cli_run import Slicer
import tool_python  # (scripts/lib, on the path the context put it on)


def ifs_fields(line: str, n: int) -> List[str]:
    """`IFS=$'\\t' read -r a b c ...`: tab-separated words (runs of tabs one separator, none leading), the last word
    taking the rest of the line."""
    words = [w for w in line.split("\t") if w]
    head, rest = words[:n - 1], words[n - 1:]
    return head + ["\t".join(rest)] + [""] * (n - 1 - len(head))


def takes(path: str) -> List[str]:
    """`tail -n +2 | cut -f2,5 | sort`: each listing's key and film."""
    with open(path, errors="replace") as f:
        rows = f.read().splitlines()[1:]
    return sorted("\t".join(r.split("\t")[1:5:3]) if "\t" in r else r for r in rows)


class Flow:
    def __init__(self, ctx: Context, keeper: str) -> None:
        self.ctx, self.keys = ctx, Keys(ctx)
        self.slicer = Slicer(ctx, self.keys, keeper)
        self.modes = Modes(ctx, self.keys, self.slicer)

    def main(self) -> int:
        o, ctx = self.ctx.o, self.ctx
        if o.log:
            self.read_log_line()
        self.checked()
        ctx.work = tempfile.mkdtemp(prefix="identity-slice.", dir=ctx.env.get("TMPDIR") or "/tmp")
        o.labels = o.labels or f"{ctx.repo}/test/resources/fixtures/identity-judgements.tsv"
        # prod's filed family answers (no recording holds them): this checkout's unmatched-cluster capture unless named
        o.families = o.families or f"{ctx.repo}/test/resources/fixtures/identity-unmatched/{o.cc}.json.gz"
        if o.explain:
            return self.modes.explain()
        if o.faith == "capture":
            return self.modes.faithfulness_of_capture()
        if o.faith:
            return self.modes.faithfulness_of_run()
        return self.sliced()

    def clean_up(self) -> None:
        if self.ctx.work:
            shutil.rmtree(self.ctx.work, ignore_errors=True)

    def read_log_line(self) -> None:
        """A take-change line: its fields name the listing and the build that took it."""
        o, ctx = self.ctx.o, self.ctx
        d = json.loads(o.log)
        cc, venue, listing, build, mode = ifs_fields("\t".join(str(d.get(k, "")) for k in (
            "country", "venue", "listing", "build", "projection_mode")), 5)
        if not venue or not listing:
            say("the log line names no venue and listing")
            raise Exit(2)
        o.cc = o.cc or cc
        o.passed += ["--key", f"{venue}|{listing}"]
        if not o.refs and build and build != "unknown":
            o.refs.append(build)
        ctx.log(f"the log line: {o.cc}, '{venue}' | '{listing}', build {build or 'unknown'}, projection {mode or '?'} "
                "(the slice replays the whole resolve, as either mode does)")

    def checked(self) -> None:
        o = self.ctx.o
        if not o.cc:
            raise Usage("", usage=True)
        if len(o.refs) > 2:
            raise Usage("at most two --ref")
        if o.mirror and o.mirror not in ("on", o.cc):
            raise Usage(f"--prod-mirror {o.mirror}: the slice is {o.cc}'s — the mirror read is the slice's country")
        if o.mirror and o.explain:
            raise Usage("--prod-mirror reads prod's state beside a recording: slices and --faithfulness, not --explain")

    def sliced(self) -> int:
        o, ctx = self.ctx.o, self.ctx
        o.run = o.run or ctx.answer("identity_newest_recording", o.cc)
        o.run = ctx.answer("identity_recording_run", o.run, o.cc)
        ctx.recording = ctx.recording_of(o.run, o.cc)
        self.modes.use_mirror()
        if o.drift:
            return Drift(ctx).whole()
        if o.batch:
            o.passed += ["--batch", o.batch]
        if not o.passed:
            raise Usage("no selector", usage=True)
        if o.bisect:
            return self.bisected()
        o.refs = o.refs or ["."]
        for i, ref in enumerate(o.refs):
            self.decided(i, ref)
        if o.emit_drift:
            return Drift(ctx).emit()
        return self.shown()

    def decided(self, i: int, ref: str) -> None:
        """One ref's slices; 3: every take written, but some slice met a request it does not hold (read as unknown, as
        a leg reads a gap) -- said, kept."""
        ctx = self.ctx
        rc = self.slicer.run_at(ctx.tree_of(ref), self.keys.sha_of(ref), ctx.path(f"{i}.tsv"), ctx.o.passed,
                                ctx.path(f"{i}.txt"))
        if rc == 3:
            ctx.log(f"{ref}: some slices are incomplete (above) — their takes read the missing requests as unknown")
        elif rc != 0:
            cat(ctx.path(f"{i}.txt"))
            raise Exit(1)

    def shown(self) -> int:
        o, ctx = self.ctx.o, self.ctx
        if o.keep:
            shutil.copyfile(ctx.path(f"{len(o.refs) - 1}.tsv"), o.keep)
        if len(o.refs) == 1:
            if not o.batch:
                cat(ctx.path("0.txt"))
            return 0
        if not o.batch:
            for i, ref in enumerate(o.refs):
                print(f"### {ref}", flush=True)
                cat(ctx.path(f"{i}.txt"))
                print(flush=True)
        return self.diffed(ctx.path("0.tsv"), ctx.path("1.tsv"), o.refs[0], o.refs[1])

    def diffed(self, a: str, b: str, ra: str, rb: str) -> int:
        return self.ctx.run(tool_python.isolated(f"{self.ctx.repo}/scripts/identity_slice_diff.py", a, b, ra, rb))

    def probe(self, ref: str, out: str) -> None:
        """A bisect step's slices at <ref> into <out>: a failed one ends the tool, with its status."""
        rc = self.slicer.run_at(self.ctx.tree_of(ref), self.keys.sha_of(ref), out, self.ctx.o.passed, os.devnull)
        if rc != 0:
            raise Exit(rc)

    def bisected(self) -> int:
        """The first commit of A..B (first parent) whose take of the selected listings differs from A's."""
        o, ctx = self.ctx.o, self.ctx
        start, end = o.bisect.split("..")[0], o.bisect.split("..")[-1]
        if not ctx.need_ref(start) or not ctx.need_ref(end):
            return 2
        commits = ctx.git("rev-list", "--reverse", "--first-parent", f"{start}..{end}").stdout.split()
        if not commits:
            say(f"no commit in {o.bisect}")
            return 2
        base = ctx.path("base.tsv")
        self.probe(start, base)
        counts = ctx.capture(["sh", "-c", 'tail -n +2 "$1" | cut -f2,5 | sort | cut -f2 | sort | uniq -c | tr "\\n" ";"',
                              "sh", base]).stdout
        ctx.log(f"bisecting {len(commits)} commit(s) after {start}: the take at {start} is {counts}")
        lo, hi = 0, len(commits) - 1
        self.probe(commits[hi], ctx.path("probe.tsv"))
        if takes(base) == takes(ctx.path("probe.tsv")):
            print(f"no commit of {o.bisect} moves the take", flush=True)
            return 0
        while lo < hi:
            mid = (lo + hi) // 2
            self.probe(commits[mid], ctx.path("probe.tsv"))
            lo, hi = (mid + 1, hi) if takes(base) == takes(ctx.path("probe.tsv")) else (lo, mid)
        first = commits[lo]
        print(f"first commit moving the take: {ctx.git('log', '-1', '--format=%h %s', first).stdout}", end="", flush=True)
        self.probe(first, ctx.path("first.tsv"))
        short = ctx.git("rev-parse", "--short", first).stdout.strip()
        return self.diffed(base, ctx.path("first.tsv"), f"{short}~1", short)
