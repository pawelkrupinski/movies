#!/usr/bin/env python3
# ROTATING DRIFT SEEDS (the owner, 2026-10-31): each convergence leg keeps its two incremental = whole rows,
# `takes-drift-1 (<country>)` and `takes-drift-2 (<country>)` -- the job NAMES stay, the gate, agentctl and the ruleset
# match them -- but the seed each row replays is the RUN's, not the row number. On seeds 1 and 2 alone order-dependent
# bugs on any other seed slipped through: UK "Swan Lake" flipped on seed 3 only, a Cosi/Fishguard flip on seed 11 only,
# both found by slices replaying seeds 1-12.
#
# THE SCHEME: run number N's rows replay seeds((N-1)*2 mod 64)+1 and +2: run 1 seeds 1 and 2, run 2 seeds 3 and 4, ...
# run 32 seeds 63 and 64, run 33 seeds 1 and 2 again. The two rows always differ, every seed of 1..64 is replayed once
# every 32 runs, and the same run -- a rerun of it included (`github.run_number` stays) -- replays the same two.
#
# PINS: a dispatch's `drift-seeds` names seeds instead -- `S1,S2`, `S1,` (row 1 alone on a drift-only run) or `,S2`
# (row 2 alone); a bare `S` is `S,`. scripts/identity-drift-ci pins a red run's own seeds to replay exactly what it
# failed, and `--against main` pins main's. On a full (not drift-only) run an empty side still replays the run's seed.
#
# A red names its seed (identity-drift-check.sh: the ::error title, verdict.txt, the step summary, the leg's commit
# status) with a one-line reproducer, and every seed found red anywhere is cut into a drift slice
# (docs/design/identity-slice.md, "A red seed becomes a slice") so IdentityDriftSlicesSpec covers it on every run.
#
# usage:
#   drift_seeds.py outputs --run-number N [--pins P] [--drift-only true|false]
#                                       `drift-seed-1=S1` and `drift-seed-2=S2` for $GITHUB_OUTPUT (a side left out of a
#                                       drift-only run is empty)
#   drift_seeds.py seed --row 1|2 --run-number N [--pins P]
#                                       the one seed that row replays
#   drift_seeds.py pins P               P normalised to `S1,S2` (either side may be empty); exit 2 when malformed
#   drift_seeds.py slices <cc> <fixtures dir> <tsv>...
#                                       the committed drift slices (test/resources/fixtures/drift-slices/<cc>/<name>/)
#                                       whose core listings the TSVs' rows (drift-<cc>.tsv, flips-<cc>.tsv) name, a dir
#                                       a line: the fast reproducer of a red listing that has one
# Tested by drift_seeds_test.py.

from __future__ import annotations

import csv
import gzip
import json
import os
import sys

RANGE = 64
ROWS = (1, 2)


def rotation(run_number: int) -> tuple[int, int]:
    """The two seeds run `run_number` replays: distinct, deterministic per run, all of 1..RANGE every RANGE/2 runs."""
    if run_number < 1:
        raise ValueError(f"run number {run_number}: runs count from 1")
    first = (2 * (run_number - 1)) % RANGE + 1
    return first, first + 1


def parse_pins(text: str | None) -> tuple[int | None, int | None]:
    """`S1,S2`, `S1,`, `,S2`, `S` (= `S,`) or empty, each seed a positive integer."""
    text = (text or "").strip()
    if text in ("", "both", ","):
        return None, None
    parts = text.split(",")
    if len(parts) == 1:
        parts.append("")
    if len(parts) != 2:
        raise ValueError(f"drift seeds {text!r}: at most two, `S1,S2`")

    def seed(part):
        part = part.strip()
        if not part:
            return None
        if not part.isdigit() or int(part) < 1:
            raise ValueError(f"drift seeds {text!r}: {part!r} is no positive seed")
        return int(part)

    return seed(parts[0]), seed(parts[1])


def resolve(run_number: int, pins: str | None = "", drift_only: bool = False) -> tuple[int | None, int | None]:
    """Each row's seed: a pinned one as pinned; an unpinned one the run's rotation -- or, on a drift-only run naming any
    pin, none (the row does not run). The two rows never replay the same seed."""
    one, two = parse_pins(pins)
    if one is not None and one == two:
        raise ValueError(f"drift seeds {pins!r}: the two rows replay two different seeds")
    if one is None and two is None:
        return rotation(run_number)
    if drift_only:
        return one, two
    spare = [s for s in rotation(run_number) + (rotation(run_number + 1)) if s not in (one, two)]
    return (one if one is not None else spare[0]), (two if two is not None else spare[0])


def slices(cc: str, fixtures: str, tsvs: list[str]) -> list[str]:
    """The drift slices of `cc` whose core listings a TSV row (columns venue, rawTitle) names."""
    named = set()
    for path in tsvs:
        if not os.path.isfile(path):
            continue
        with open(path, newline="") as f:
            for row in csv.DictReader(f, delimiter="\t"):
                if row.get("venue") is not None and row.get("rawTitle") is not None:
                    named.add((row["venue"], row["rawTitle"]))
    root = os.path.join(fixtures, "drift-slices", cc)
    out = []
    if not named or not os.path.isdir(root):
        return out
    for name in sorted(os.listdir(root)):
        path = os.path.join(root, name, "slice.json.gz")
        if not os.path.isfile(path):
            continue
        try:
            with gzip.open(path, "rt") as f:
                core = json.load(f).get("core") or []
        except (OSError, ValueError):
            continue
        # a core listing: "N\0venue\0url\0title" (a listing) or "P\0venue\0title\0" (a page's)
        for entry in core:
            parts = str(entry).split("\0")
            title = parts[3] if parts[0] == "N" and len(parts) > 3 else parts[2] if len(parts) > 2 else ""
            if len(parts) > 1 and (parts[1], title) in named:
                out.append(os.path.join(root, name))
                break
    return out


def options(argv):
    opts, rest = {}, []
    it = iter(argv)
    for a in it:
        if a.startswith("--"):
            opts[a[2:]] = next(it, "")
        else:
            rest.append(a)
    return opts, rest


def main(argv):
    if not argv:
        print("usage: drift_seeds.py outputs|seed|pins|slices ... (see the header)", file=sys.stderr)
        return 2
    command, (opts, rest) = argv[0], options(argv[1:])
    try:
        if command == "outputs":
            one, two = resolve(int(opts["run-number"]), opts.get("pins", ""), opts.get("drift-only", "") == "true")
            print(f"drift-seed-1={one or ''}")
            print(f"drift-seed-2={two or ''}")
            return 0
        if command == "seed":
            row = int(opts["row"])
            if row not in ROWS:
                raise ValueError(f"row {row}: takes-drift-1 or takes-drift-2")
            seed = resolve(int(opts["run-number"]), opts.get("pins", ""))[row - 1]
            print(seed)
            return 0
        if command == "pins":
            one, two = parse_pins(rest[0] if rest else "")
            print(f"{one or ''},{two or ''}")
            return 0
        if command == "slices":
            print("\n".join(slices(rest[0], rest[1], rest[2:])))
            return 0
    except (KeyError, ValueError, IndexError) as e:
        print(f"drift_seeds.py {command}: {e}", file=sys.stderr)
        return 2
    print(f"drift_seeds.py: no command {command!r}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
