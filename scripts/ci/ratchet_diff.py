#!/usr/bin/env python3
# Purpose: The ratchet diff: every frozen row that changed between two refs, before and after
# THE RATCHET DIFF (scripts/ratchet-diff; CLAUDE.md, "Code quality ratchets" and the coupling ratchet): every frozen row
# that changed between two refs, as a before -> after table, so a lane's report pastes computed numbers, never narrated
# ones. Read from the committed baselines at each ref (cheap), or, with --measure, the head side from fresh measures
# of the working tree (an agent may have skipped --update on a fall).
#
#   ratchet_diff.py --base A [--head B] [--quality-rows F] [--coupling-rows F] [--js-rows F] [--tsv]
#       A, B: refs (B defaults to HEAD). --quality-rows / --coupling-rows / --js-rows: the head side from those measured
#       rows (quality.QualityReport's rows.tsv, couplingReport's rows.tsv, js-quality-report's over-limit.tsv) instead of
#       B's baselines -- what --measure passes.
#
# The metrics: cognitive, cyclomatic, inner-defs (complexity-baseline.tsv); method-lines, file-lines (length-baseline.tsv);
# copies (duplication-baseline.tsv); fanout, distance, cycle-edge (coupling-baseline.tsv); and the JS/TS ratchet's, each
# printed `js <kind>` (js-quality-baseline.tsv, scripts/ci/js_quality_ratchet.py: no move credit). Each changed row is marked
#   fell      its value fell
#   rose      its value rose (or it moved and rose)
#   new       a newly frozen piece (a row past the limit that the base did not have)
#   measured  a new quality row whose every source the range left unchanged: the code was already like this, and a
#             change to the measure (2026-11-21: member vals and given aliases) or a limit is what froze it
#   removed   gone from the baseline: within the limit now (or its code gone)
#   moved     re-keyed with the same value or lower (a renamed source: git -M; a moved class: the same simple name) --
#             the ratchets' own move rules (quality_ratchet.moved_key, coupling_ratchet.moved)
# then, per metric, the rows and the summed value before -> after and how many rows are still over the limit at head.
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from typing import Dict, List, Optional, Tuple

# Its sibling ratchets' bytecode stays out of the checkout: under `python3 -I` (its test's run) PYTHONDONTWRITEBYTECODE
# is ignored, and their __pycache__ landed in scripts/ci (Order independence #105).
sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import coupling_ratchet  # noqa: E402
import js_quality_ratchet  # noqa: E402
import quality_ratchet  # noqa: E402
import tool_git  # noqa: E402

COUPLING = coupling_ratchet.BASELINE_PATH
# metric -> (baseline file, ratchet family); the order the tables print in
METRICS: List[Tuple[str, str, str]] = [
    (kind, r.baseline, "quality") for name in ("complexity", "length", "duplication")
    for r in [quality_ratchet.RATCHETS[name]] for kind in r.kinds
] + [("fanout", COUPLING, "coupling"), ("distance", COUPLING, "coupling"), ("cycle-edge", COUPLING, "coupling")] + [
    (kind, js_quality_ratchet.BASELINE, "js") for kind in js_quality_ratchet.KINDS if kind != "clone-tokens"
]
JS_PREFIX = "js "  # a JS/TS metric's printed name: `js cognitive`, `js fanout` -- apart from the Scala rows of one kind
LIMIT_ZERO = ("copies", "cycle-edge")  # every row of these is over the limit (a clone group, a cycle edge)

Row = Tuple[str, str, Optional[int], Optional[int], str]  # metric, key, before, after, mark


def renames(base: str, head: Optional[str]) -> Dict[str, str]:
    """The sources git's rename detection follows from `base` to `head` (the working tree when None)."""
    fields = tool_git.Git.here().text("diff", "-M", "--name-status", "-z", base, *([head] if head else [])).split("\0")
    out, i = {}, 0
    while i < len(fields) - 1:
        status = fields[i]
        if status.startswith(("R", "C")):
            if status.startswith("R"):
                out[fields[i + 1]] = fields[i + 2]
            i += 3
        else:
            i += 2
    return out


class RefRenames:
    """quality_ratchet.Renames' shape (follow(path) -> the path at head) over one diff between two refs."""

    def __init__(self, moves: Dict[str, str]):
        self.moves = moves

    def follow(self, path: str) -> str:
        return self.moves.get(path, path)


class Untouched:
    """The sources that exist at `base` and that the range from it to `head` (the working tree when None) never changed."""

    def __init__(self, base: str, head: Optional[str]):
        changed = set(tool_git.Git.here().text("diff", "--name-only", "-z", base, *([head] if head else [])).split("\0"))
        self.paths = {p for p in tool_git.Git.here().text("ls-tree", "-r", "--name-only", "-z", base).split("\0") if p and p not in changed}

    def holds(self, key: str) -> bool:
        """Whether every source of a quality key (`<path>:<Owner.method>`, `<path>`, `<path>|<path>...`) is untouched."""
        return all(p in self.paths for p in key.partition(":")[0].split("|"))


MARKS = ("fell", "rose", "new", "measured", "removed (within the limit)", "moved")


def limit_of(rows, metric: str) -> int:
    if metric in LIMIT_ZERO:
        return 0
    return coupling_ratchet.of_kind(rows, "limit").get(metric, 0)


def diff_metric(metric: str, family: str, before: Dict[str, int], after: Dict[str, int], moves: RefRenames,
                untouched: Optional[Untouched] = None) -> List[Row]:
    out: List[Row] = []
    gone = {k: v for k, v in before.items() if k not in after}
    fresh = {k: v for k, v in after.items() if k not in before}
    for key in sorted(set(before) & set(after)):
        a, b = before[key], after[key]
        if a != b:
            out.append((metric, key, a, b, "fell" if b < a else "rose"))
    for key, value in sorted(fresh.items()):
        if family == "quality":
            origin = next((old for old in sorted(gone) if quality_ratchet.moved_key(old, moves) == key), None)
        elif metric == "fanout":
            simple = key.rsplit(".", 1)[-1]
            origin = next((old for old in sorted(gone) if old.rsplit(".", 1)[-1] == simple), None)
        else:
            origin = None
        if origin is not None:
            was = gone.pop(origin)
            out.append((metric, f"{origin} => {key}", was, value, "rose" if value > was else "moved"))
        else:
            measured = family == "quality" and untouched is not None and untouched.holds(key)
            out.append((metric, key, None, value, "measured" if measured else "new"))
    out += [(metric, key, value, None, "removed (within the limit)") for key, value in sorted(gone.items())]
    return out


def rows_at(ref: str, path: str, family: str = ""):
    parse = js_quality_ratchet.parse if family == "js" else quality_ratchet.parse
    return parse(quality_ratchet.file_at(ref, path))


def collect(base: str, head: Optional[str], quality_rows: str = "", coupling_rows: str = "", js_rows: str = ""):
    """Every changed row, and per metric (rows, sum, over the limit) before and after."""
    head_ref = head if not (quality_rows or coupling_rows or js_rows) else None
    moves, untouched = RefRenames(renames(base, head_ref)), Untouched(base, head_ref)
    measured = {}
    for family, path in (("quality", quality_rows), ("coupling", coupling_rows), ("js", js_rows)):
        if path:
            with open(path) as f:
                text = f.read()
            measured[family] = js_quality_ratchet.parse(text) if family == "js" else quality_ratchet.parse(text)
            if family == "js":  # the measure's rows of tests and scripts are reported, never frozen
                production = js_quality_ratchet.production_paths()
                measured[family] = {(k, key): v for (k, key), v in measured[family].items()
                                    if k == "limit" or js_quality_ratchet.gated(key, production)}
    changed: List[Row] = []
    totals = []
    for metric, baseline, family in METRICS:
        old = rows_at(base, baseline, family)
        new = measured.get(family) if family in measured else rows_at(head or "HEAD", baseline, family)
        before, after = coupling_ratchet.of_kind(old, metric), coupling_ratchet.of_kind(new, metric)
        rows = diff_metric(metric, family, before, after, moves, untouched)
        if family == "js":
            metric = JS_PREFIX + metric
            rows = [(metric, *row[1:]) for row in rows]
        changed += rows
        over = lambda values, lim: sum(1 for v in values.values() if v > lim)
        marks = {m: sum(1 for r in rows if r[4] == m) for m in MARKS}
        totals.append((metric, len(before), len(after), sum(before.values()), sum(after.values()),
                       over(before, limit_of(old, metric)), over(after, limit_of(new, metric)), limit_of(new, metric), marks))
    return changed, totals


def show(v) -> str:
    if v is None:
        return "-"
    return str(int(v)) if v == int(v) else f"{round(v, 2):g}"


def markdown(base: str, head: str, changed: List[Row], totals) -> str:
    lines = [f"### Ratchet rows, {base} -> {head}", ""]
    if changed:
        lines += ["| metric | row | before | after | change |", "|---|---|---:|---:|---|"]
        lines += [f"| {m} | `{k}` | {show(a)} | {show(b)} | {mark} |" for m, k, a, b, mark in changed]
    else:
        lines.append("No ratchet row changed.")
    lines += ["", "| metric | limit | rows | sum | fell | rose | new | measured | removed | moved | over the limit |",
              "|---|---:|---|---|---:|---:|---:|---:|---:|---:|---|"]
    for metric, n0, n1, s0, s1, o0, o1, lim, marks in totals:
        lines.append(f"| {metric} | {show(lim)} | {n0} -> {n1} | {show(s0)} -> {show(s1)} | " + " | ".join(str(marks[m]) for m in MARKS) + f" | {o0} -> {o1} |")
    return "\n".join(lines) + "\n"


def tsv(changed: List[Row], totals) -> str:
    lines = ["# metric\trow\tbefore\tafter\tchange"]
    lines += [f"{m}\t{k}\t{show(a)}\t{show(b)}\t{mark}" for m, k, a, b, mark in changed]
    lines.append("# total\tmetric\trows-before\trows-after\tsum-before\tsum-after\tover-before\tover-after\tlimit")
    lines += [f"total\t{m}\t{n0}\t{n1}\t{show(s0)}\t{show(s1)}\t{o0}\t{o1}\t{show(lim)}" for m, n0, n1, s0, s1, o0, o1, lim, _ in totals]
    return "\n".join(lines) + "\n"


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description="Every ratchet row changed between two refs (scripts/ratchet-diff).")
    parser.add_argument("--base", required=True)
    parser.add_argument("--head", default="")
    parser.add_argument("--quality-rows", default="")
    parser.add_argument("--coupling-rows", default="")
    parser.add_argument("--js-rows", default="")
    parser.add_argument("--tsv", action="store_true")
    args = parser.parse_args(argv)
    try:
        head = args.head or "HEAD"
        changed, totals = collect(args.base, head, args.quality_rows, args.coupling_rows, args.js_rows)
        shown_head = "the working tree (measured)" if args.quality_rows or args.coupling_rows or args.js_rows else head
        sys.stdout.write(tsv(changed, totals) if args.tsv else markdown(args.base, shown_head, changed, totals))
        return 0
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"ratchet_diff: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
