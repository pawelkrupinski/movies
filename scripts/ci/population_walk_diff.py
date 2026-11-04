#!/usr/bin/env python3
"""THE POPULATION WALK, JUDGED AGAINST THE CHANGE'S BASE (the landing check's half of
services.identity.PopulationMonotonicitySpec; convergence-reds root cause 2, fix C).

The spec, run by the pre-push hook's identity check, writes a CANDIDATE file for each committed slice whose flips
(a listing's take going A -> B -> A, none -> A -> none, along an arrival chain) differ from the known rows
(identity-population-flips-known.tsv): `slice <name>`, `head <venue> <raw title> <line>` per flip at the change's
head, `known <venue> <raw title> <note>` per known row. scripts/ci/population-walk-diff walks the same slices with the
base's code (PopulationMonotonicitySpec.main: `<slice> <venue> <raw title> <shape: path>` rows, `# <slice>: ...` lines)
and this judges, listing by listing (venue, raw title):

  - NEW: flips at the head, not known, and not at the base -- the change adds it: REFUSED, the listing, slice and chain
    named;
  - CURED: a known row the base flips and the head does not -- the change cures it: REFUSED until the row goes (the
    known list only shrinks; main's spec would fail on it);
  - PRE-EXISTING: flips at the head and the base alike, not known (main's own), and a known row neither flips (a stale
    row main holds): said, passed -- not this change's;
  - a slice the base could not walk (its walk failed, no row came back): judged on the known rows alone, as the spec
    would -- never passed blind.

Usage: population_walk_diff.py judge <candidates dir> <base rows file>   (exit 1: refused)
       population_walk_diff.py slices <candidates dir>                   (the candidate slices, one a line)

`differential(head, base)` is the shared core: the keys only the head holds, only the base holds, and both hold.
"""

import os
import sys


def differential(head, base):
    """(new, gone, both): the keys of `head` not in `base`, of `base` not in `head`, and in both -- in head's order,
    then base's."""
    bset, hset = set(base), set(head)
    return ([k for k in head if k not in bset], [k for k in base if k not in hset], [k for k in head if k in bset])


def read_candidates(directory):
    """{slice: {"head": {(venue, title): line}, "known": {(venue, title): note}}} of every candidate file."""
    out = {}
    if not os.path.isdir(directory):
        return out
    for name in sorted(os.listdir(directory)):
        if not name.endswith(".tsv"):
            continue
        slice_name, head, known = None, {}, {}
        with open(os.path.join(directory, name), encoding="utf-8") as fh:
            for raw in fh:
                cols = raw.rstrip("\n").split("\t")
                if cols[0] == "slice" and len(cols) >= 2:
                    slice_name = cols[1]
                elif cols[0] == "head" and len(cols) >= 4:
                    head[(cols[1], cols[2])] = cols[3]
                elif cols[0] == "known" and len(cols) >= 4:
                    known[(cols[1], cols[2])] = cols[3]
        if slice_name:
            out[slice_name] = {"head": head, "known": known}
    return out


def read_base(path):
    """({slice: {(venue, title): shape-and-path}} of every slice the base walked, {slice: why} of those it could not)."""
    walked, failed = {}, {}
    if not os.path.exists(path):
        return walked, failed
    with open(path, encoding="utf-8") as fh:
        for raw in fh:
            line = raw.rstrip("\n")
            if line.startswith("# "):
                head, _, rest = line[2:].partition(": ")
                if rest.startswith("FAILED"):
                    failed[head] = rest
                else:
                    walked.setdefault(head, {})
                continue
            cols = line.split("\t")
            if len(cols) >= 4:
                walked.setdefault(cols[0], {})[(cols[1], cols[2])] = cols[3]
    return walked, failed


def judge(candidates, walked, failed):
    """(refusals, notes): one line each."""
    refusals, notes = [], []
    for name, c in sorted(candidates.items()):
        head, known = c["head"], c["known"]
        if name not in walked:
            why = failed.get(name, "no rows came back")
            notes.append(f"{name}: the base could not walk it ({why}): judged on the known rows alone")
            for k, line in head.items():
                if k not in known:
                    refusals.append(f"NEW flip (base unwalked): {name}: {line}")
            for k, note in known.items():
                if k not in head:
                    refusals.append(f"CURED known row (base unwalked) -- remove it from identity-population-flips-known.tsv: "
                                    f"{name}: {k[0]} | {k[1]} ({note})")
            continue
        base = walked[name]
        fresh = [k for k in head if k not in known]
        new, _, both = differential(fresh, list(base))
        for k in new:
            refusals.append(f"NEW flip the base does not have: {name}: {head[k]}")
        for k in both:
            notes.append(f"pre-existing (the base flips it too, main's): {name}: {head[k]}")
        for k, note in known.items():
            if k in head:
                continue
            if k in base:
                refusals.append(f"CURED known row (the base flips it, this change does not) -- remove it from "
                                f"identity-population-flips-known.tsv: {name}: {k[0]} | {k[1]} ({note})")
            else:
                notes.append(f"stale known row (the base does not flip it either, main's): {name}: {k[0]} | {k[1]}")
    return refusals, notes


def main(argv):
    if len(argv) == 2 and argv[0] == "slices":
        for name in sorted(read_candidates(argv[1])):
            print(name)
        return 0
    if len(argv) != 3 or argv[0] != "judge":
        print(__doc__.split("\n\n")[-2], file=sys.stderr)
        return 2
    candidates = read_candidates(argv[1])
    walked, failed = read_base(argv[2])
    refusals, notes = judge(candidates, walked, failed)
    for n in notes:
        print(f"population walk: {n}")
    for r in refusals:
        print(f"population walk: {r}")
    print(f"population walk: {len(candidates)} slice(s) judged against the base: "
          f"{'REFUSED, ' + str(len(refusals)) + ' row(s)' if refusals else 'nothing new'}")
    return 1 if refusals else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
