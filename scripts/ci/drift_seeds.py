#!/usr/bin/env python3
# FIXED DRIFT SEEDS (the owner, 2026-11-04: "a gate that only fails on what the change adds"; supersedes the rotating seeds
# of 2026-10-31). Each convergence leg keeps its incremental = whole rows, `takes-drift-1 (<country>)` and
# `takes-drift-2 (<country>)` -- the job NAMES stay, the gate, agentctl and the ruleset match them -- and each row replays
# the COMMITTED seeds test/resources/fixtures/drift-gate-seeds.tsv assigns it for its country, the same on every run.
# A country whose seeds take longer than a row may (a German seed is a 10-20 minute row) spreads them over further rows,
# `takes-drift-3 (<country>)` and on, by naming the row in the file (rows_of): the preflight plans every row a country
# has (convergence_reuse.py drift_phases, convergence_slots.py packs them into 20 runners); a pin reaches rows 1 and 2
# alone. A new row is a new job name: it lands `unproven` in .github/gating-checks.tsv.
# Rotating seeds made the gate a random sampler of old defects: a flip that had always been there failed whichever
# unrelated commit drew its seed (3 of 7 red streaks to 2026-11-04 ended with no identity change). Fixed seeds make
# a row's verdict a property of the change (scripts/ci/identity-drift-check.sh judges it against its BASE on the same
# seeds: only a flip the head adds fails), and THE EXPLORER (.github/workflows/identity-drift-explorer.yml) walks many
# random seeds nightly, appending each seed that found a NEW flip here (`add`), so the gate grows without turning random.
#
# THE SET: initially every seed that ever found a flip (the baselines, the drift slices, the commit history) that fits
# the rows' time (one replay is 3-16 minutes a seed: ES 2 a row, the others 1), the rest replayed by the explorer.
#
# PINS: a dispatch's `drift-seeds` names seeds instead -- `S1,S2`, `S1,` (row 1 alone on a drift-only run) or `,S2`
# (row 2 alone); a bare `S` is `S,`. scripts/identity-drift-ci pins a red run's own seeds to replay exactly what it
# failed. An unpinned row replays its fixed set.
#
# A red names its seed (identity-drift-check.sh: the ::error title, verdict.txt, the step summary, the leg's commit
# status) with a one-line reproducer, and every seed found red anywhere is cut into a drift slice
# (docs/design/identity-slice.md, "A red seed becomes a slice") so IdentityDriftSlicesSpec covers it on every run.
#
# usage:
#   drift_seeds.py outputs [--pins P] [--drift-only true|false] [--run-number N]
#                                       `drift-seed-1=S1` and `drift-seed-2=S2` for $GITHUB_OUTPUT: the pins (empty: the
#                                       row's fixed set)
#   drift_seeds.py seeds --cc CC --row 1|2 [--pins P] [--file TSV]
#                                       the seeds that row replays, space-separated: its pin, else its fixed set
#   drift_seeds.py rows --cc CC [--pins P] [--file TSV]
#                                       CC's takes-drift rows a run replays, space-separated (rows_of): 1 2 and any further
#                                       row the file names; a pinned dispatch's pinned rows alone
#   drift_seeds.py gate --cc CC [--file TSV]
#                                       every fixed seed of CC, space-separated
#   drift_seeds.py add --cc CC --seed S --why TEXT [--date YYYY-MM-DD] [--file TSV]
#                                       append S to CC's fixed set, on the row of CC's holding fewest (no-op when there)
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

ROWS = (1, 2)
HERE = os.path.dirname(os.path.abspath(__file__))
SEEDS_FILE = os.path.join(HERE, "..", "..", "test", "resources", "fixtures", "drift-gate-seeds.tsv")
COLUMNS = ("country", "seed", "row", "added", "why")


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


def resolve(run_number: int = 1, pins: str | None = "", drift_only: bool = False) -> tuple[int | None, int | None]:
    """Each row's PIN, or None: an unpinned row replays its fixed set (`fixed`). The two pins never name one seed.
    (`run_number` and `drift_only` are kept for the callers: neither moves a seed any more.)"""
    one, two = parse_pins(pins)
    if one is not None and one == two:
        raise ValueError(f"drift seeds {pins!r}: the two rows replay two different seeds")
    return one, two


def read_set(path: str | None = None) -> list[dict]:
    """The committed fixed seeds: one dict a row of drift-gate-seeds.tsv (`#` lines and the header aside)."""
    rows = []
    with open(path or SEEDS_FILE, newline="") as f:
        lines = [line for line in f if line.strip() and not line.startswith("#")]
    for row in csv.DictReader(lines, delimiter="\t"):
        if not row.get("seed", "").strip().isdigit() or not row.get("row", "").strip().isdigit() or int(row["row"]) < 1:
            raise ValueError(f"drift-gate-seeds: every row names a country, a positive seed and a row 1, 2, ...: {row}")
        rows.append({k: (row.get(k) or "").strip() for k in COLUMNS})
    return rows


def fixed(cc: str, row: int | None = None, path: str | None = None) -> list[int]:
    """CC's fixed seeds (row 1's, row 2's, or both), in file order, each once."""
    out = []
    for r in read_set(path):
        if r["country"] == cc and (row is None or int(r["row"]) == row) and int(r["seed"]) not in out:
            out.append(int(r["seed"]))
    return out


def rows_of(cc: str, path: str | None = None, pins: str | None = "") -> list[int]:
    """CC's takes-drift rows: 1 and 2 always, and every further row the file assigns a seed (a country whose seeds take
    longer than a row should, Germany's, spreads them over more rows: convergence_slots.py packs them). A pinned dispatch
    runs the rows its pins name alone -- a pin reaches rows 1 and 2, never an extra row."""
    if parse_pins(pins) != (None, None):
        return [r for r, pin in zip(ROWS, resolve(1, pins)) if pin is not None]
    return sorted(set(ROWS) | {int(r["row"]) for r in read_set(path) if r["country"] == cc})


def seeds_of_row(cc: str, row: int, pins: str | None = "", path: str | None = None) -> list[int]:
    """What row `row` replays: its pin, else its fixed set."""
    if row not in rows_of(cc, path):
        raise ValueError(f"row {row}: {cc}'s takes-drift rows are {', '.join(map(str, rows_of(cc, path)))}")
    if row not in ROWS:
        return [] if parse_pins(pins) != (None, None) else fixed(cc, row, path)
    pinned = resolve(1, pins)[row - 1]
    if pinned is not None:
        return [pinned]
    if parse_pins(pins) != (None, None):
        return []  # the other row is pinned: a pinned dispatch replays its pins alone
    return fixed(cc, row, path)


def add(cc: str, seed: int, why: str, date: str, path: str | None = None) -> bool:
    """Append `seed` to CC's fixed set, on the row of CC's (rows_of) holding fewest of its seeds (the lowest on a tie);
    False when there."""
    path = path or SEEDS_FILE
    if seed in fixed(cc, None, path):
        return False
    rows = rows_of(cc, path)
    counts = {r: len(fixed(cc, r, path)) for r in rows}
    row = min(rows, key=lambda r: (counts[r], r))
    with open(path) as f:
        text = f.read()
    with open(path, "a") as f:
        if text and not text.endswith("\n"):
            f.write("\n")
        f.write("\t".join([cc, str(seed), str(row), date, " ".join(why.split())]) + "\n")
    return True


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
        print("usage: drift_seeds.py outputs|seeds|rows|gate|add|pins|slices ... (see the header)", file=sys.stderr)
        return 2
    command, (opts, rest) = argv[0], options(argv[1:])
    try:
        if command == "outputs":
            one, two = resolve(1, opts.get("pins", ""))
            print(f"drift-seed-1={one or ''}")
            print(f"drift-seed-2={two or ''}")
            return 0
        if command == "seeds":
            print(" ".join(map(str, seeds_of_row(opts["cc"], int(opts["row"]), opts.get("pins", ""), opts.get("file")))))
            return 0
        if command == "rows":
            print(" ".join(map(str, rows_of(opts["cc"], opts.get("file"), opts.get("pins", "")))))
            return 0
        if command == "gate":
            print(" ".join(map(str, fixed(opts["cc"], None, opts.get("file")))))
            return 0
        if command == "add":
            import datetime
            seed = int(opts["seed"])
            if seed < 1:
                raise ValueError(f"seed {seed}: seeds are positive")
            added = add(opts["cc"], seed, opts.get("why", ""), opts.get("date") or datetime.date.today().isoformat(), opts.get("file"))
            print("added" if added else "already there")
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
