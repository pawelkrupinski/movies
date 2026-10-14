"""scripts/venue-replay with two refs: every listing either ref alone reads, and every field the two read differently.

    python3 -I scripts/venue_replay_diff.py A.tsv B.tsv <ref A> <ref B>

Each TSV is VenueReplay's `--out`: a header of field names, one listing a row, keyed by its raw title and detail URL.
"""
import csv
import sys


def rows(path):
    with open(path, encoding="utf-8", newline="") as f:
        return {(r["raw"], r["url"]): r for r in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE)}


def diff(a, b, ref_a, ref_b):
    lines = [f"== {ref_a} → {ref_b}"]
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


def main(argv):
    path_a, path_b, ref_a, ref_b = argv[1:5]
    print(diff(rows(path_a), rows(path_b), ref_a, ref_b))


if __name__ == "__main__":
    main(sys.argv)
