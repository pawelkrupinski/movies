"""scripts/venue-replay with two refs: every listing either ref alone reads, and every field the two read differently.

    python3 -I -B scripts/venue_replay_diff.py A.tsv B.tsv <ref A> <ref B>

Each TSV is VenueReplay's `--out`: a header of field names, one listing a row, keyed by its raw title and detail URL.
"""
from __future__ import annotations

import csv
import io
import os
import sys
from typing import Dict, List, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from seams.tool_files import Files  # noqa: E402


Rows = Dict[Tuple[str, str], Dict[str, str]]


def rows(text: str) -> Rows:
    return {(r["raw"], r["url"]): r for r in csv.DictReader(io.StringIO(text, newline=""), delimiter="\t", quoting=csv.QUOTE_NONE)}


def diff(a: Rows, b: Rows, ref_a: str, ref_b: str) -> str:
    lines: List[str] = [f"== {ref_a} → {ref_b}"]
    for key in sorted(set(a) | set(b)):
        raw = key[0]
        if key not in b:
            lines.append(f"- {raw}: only at {ref_a}")
        elif key not in a:
            lines.append(f"+ {raw}: only at {ref_b}")
        else:
            moved = [f for f in a[key] if a[key][f] != b[key].get(f)]
            if moved:
                lines.append(f"~ {raw}")
                lines.extend(f"    {f}: {a[key][f]!r}\n    {' ' * len(f)}→ {b[key][f]!r}" for f in moved)
    if len(lines) == 1:
        lines.append("no listing or field moved")
    return "\n".join(lines)


def main(argv: Sequence[str], files: Files) -> None:
    path_a, path_b, ref_a, ref_b = argv[1:5]
    print(diff(rows(files.read_text(path_a)), rows(files.read_text(path_b)), ref_a, ref_b))


if __name__ == "__main__":
    main(sys.argv, Files.here())
