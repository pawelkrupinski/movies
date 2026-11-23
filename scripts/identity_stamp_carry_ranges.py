#!/usr/bin/env python3
"""THE QUEUE REWRITES WHAT LAND WROTE (2026-11-23: one carry landed in two days, every other refused as citing "a commit
not in the pushed history"): `auto` writes a change's rows for the commits `agentctl land` rebased, and the queue then
picks those commits again -- onto a main that moved, after another change of a batch -- so the commits a row cites are
never pushed. Each range a row cites is therefore also written to RANGES, keyed by its base and head, with its first-parent
commit count and its whole diff's stable patch-id: what a rebase that does not touch the change keeps. `verify` proves a
row whose head the pushed history does not hold at the pushed range with that count and patch-id instead -- its proof run
again THERE (pure-move-check, or the committed what-if record bound to that same patch-id), its digests made again THERE.
RANGES only says where to look, never what holds: a rebase over identity code (main's, or a batch's earlier change's)
moves the pushed digests off the row's, and the row is refused.

Tested by scripts/identity_stamp_carry_test.py."""

from __future__ import annotations

import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_stamp_carry_published import diff_id, read_text  # noqa: E402  (the one patch-id of a range)
from pure_move_check import git  # noqa: E402

RANGES = "test/resources/identity-stamp-carry-ranges.tsv"

RANGES_HEADER = """# THE CARRY ROWS' RANGES (scripts/identity_stamp_carry_ranges.py, "THE QUEUE REWRITES"): each base..head a row of
# identity-stamp-carry.tsv cites, with its first-parent commit count and its whole diff's `git patch-id --stable`, so the
# pre-push stampcarry check finds the range again in the pushed history after agentctl's queue picked its commits anew.
# Written with the rows, never by hand; only where to look -- the proof and the digests are made again there.
#
# base\thead\tcommits\tpatch-id
"""


def write_ranges(path: str, ranges: set[tuple[str, str]]) -> str:
    """Appends each (base, head) of `ranges` that `path` lacks: the file to commit beside the table."""
    held = read_ranges(read_text(path))
    lines: list[str] = []
    for base, head in sorted(ranges - set(held)):
        count = git("rev-list", "--count", "--first-parent", f"{base}..{head}").strip()
        lines.append(f"{base}\t{head}\t{count}\t{diff_id(base, head)}\n")
    with open(path, "a") as f:
        f.write(RANGES_HEADER if not os.path.isfile(path) or not os.path.getsize(path) else "")
        f.writelines(lines)
    return path


def read_ranges(text: str) -> dict[tuple[str, str], tuple[int, str]]:
    out: dict[tuple[str, str], tuple[int, str]] = {}
    for line in text.splitlines():
        fields = line.split("\t")
        if line.startswith("#") or len(fields) != 4 or not fields[2].isdigit():
            continue
        out[(fields[0], fields[1])] = (int(fields[2]), fields[3])
    return out


def cited_range(base: str, head: str, old: str, new: str) -> tuple[str, str] | None:
    """Where the pushed history old..new holds the cited range base..head: itself when `new` holds its head, else the
    newest first-parent pair h~count..h of old..new whose diff has the range's patch-id (RANGES at `new`); None when
    none has."""
    if subprocess.run(["git", "merge-base", "--is-ancestor", head, new], capture_output=True).returncode == 0:
        return base, head
    count, patch_id = read_ranges(git("show", f"{new}:{RANGES}", ok=(0, 128))).get((base, head), (0, ""))
    if not count or not patch_id:
        return None
    for tip in git("rev-list", "--first-parent", f"{old}..{new}").split():
        start = git("rev-parse", "-q", "--verify", f"{tip}~{count}^{{commit}}", ok=(0, 1, 128)).strip()
        if start and diff_id(start, tip) == patch_id:
            return start, tip
    return None


def record_of(record: tuple[str, str] | None, base: str, head: str) -> tuple[str, str]:
    """The (head, base) a what-if row's committed record is named by: the range the row cites (`record`, when the
    queue rewrote it into base..head), else base..head itself."""
    return record if record else (head, base)
