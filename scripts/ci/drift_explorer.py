#!/usr/bin/env python3
# THE DRIFT EXPLORER'S BOOKKEEPING (the owner, 2026-11-04: "a gate that only fails on what the change adds"; supersedes
# identity_flips_baseline.py). The drift gate replays FIXED seeds (drift-gate-seeds.tsv) and fails only on what a change
# adds over its base, so a flip main shows on a seed the gate does not replay is found here instead:
# .github/workflows/identity-drift-explorer.yml replays main on seeds the gate does not hold, every night, never fails,
# and publishes the flips no backlog row names as the asset `new-flips.tsv` on the release `identity-flip-explorer`.
# Actions cannot push main (the ruleset's bypass is the owner and agentctl's lander app), so AGENTCTL ADOPTS the asset:
# its tick reads it and, for each flip no backlog row names, files ONE item and lands a commit made by `file` below --
# the backlog row (kind drift) and the seed onto the gate (drift-gate-seeds.tsv, drift_seeds.py add), so every later
# change is judged on that seed too. commit_rules.py lets exactly that commit add a drift row (explorer_filing).
#
# usage:
#   drift_explorer.py pick --cc CC --count N --salt S [--file SEEDS_TSV]
#                       N seeds for CC to explore, space-separated: in 3..999, none on the gate, drawn from S (the run
#                       id: a rerun replays the same)
#   drift_explorer.py new-flips --backlog TSV --date YYYY-MM-DD --ref REF FLIPS_TSV...
#                       the asset rows (NEW_HEADER) of the flips TSVs' listings no backlog row of their country names, one
#                       per listing (its lowest seed); REF names where the replay's output is (`run:<id>/<artifact>`)
#   drift_explorer.py file --asset TSV [--backlog TSV] [--seeds TSV] [--owner WHO]
#                       append each asset row whose listing no backlog row names to the backlog (kind drift) and its seed
#                       to the gate; prints one line per filed flip (`filed <cc> <seed> <venue> | <title>`), exit 0
#                       (nothing new: no line, nothing written)
# Tested by drift_explorer_test.py.

from __future__ import annotations

import csv
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import drift_seeds  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
BACKLOG = os.path.join(ROOT, "test/resources/fixtures/identity-flip-backlog.tsv")
FLIP_COLUMNS = ["country", "seed", "venue", "rawTitle", "films", "reasons", "takes"]
NEW_HEADER = ["venue", "rawTitle", "country", "seed", "firstSeen", "slice", "takes"]
BACKLOG_COLUMNS = ["kind", "country", "seed", "slice", "venue", "rawTitle", "takes", "firstSeen", "owner", "note"]


def clean(text: str) -> str:
    return text.replace("\t", " ").replace("\n", " ").strip()


def read_tsv(path: str) -> list[dict]:
    """A TSV's rows as dicts by its header (its first line not `#`)."""
    if not os.path.exists(path):
        return []
    with open(path, newline="", encoding="utf-8") as f:
        lines = [line for line in f if line.strip() and not line.startswith("#")]
    reader = csv.reader(lines, delimiter="\t", quoting=csv.QUOTE_NONE)
    header = next(reader, None) or []
    return [dict(zip(header, cols + [""] * (len(header) - len(cols)))) for cols in reader]


def known(backlog: str) -> set[tuple[str, str, str]]:
    return {(r.get("country", ""), r.get("venue", ""), r.get("rawTitle", "")) for r in read_tsv(backlog)}


def pick(cc: str, count: int, salt: str, path: str | None = None) -> list[int]:
    gate = set(drift_seeds.fixed(cc, path=path))
    pool = [s for s in range(3, 1000) if s not in gate]
    return sorted(random.Random(f"{cc}:{salt}").sample(pool, min(count, len(pool))))


def new_flips(backlog: str, date: str, ref: str, paths: list[str]) -> list[list[str]]:
    have, out = known(backlog), {}
    for path in paths:
        with open(path, newline="", encoding="utf-8") as f:
            reader = csv.reader(f, delimiter="\t", quoting=csv.QUOTE_NONE)
            header = next(reader, None)
            if header is None:
                continue
            if header[: len(FLIP_COLUMNS)] != FLIP_COLUMNS:
                raise ValueError(f"{path}: not a flips TSV (header {header!r})")
            for cols in reader:
                if len(cols) < 4 or not cols[0] or not cols[1].isdigit():
                    continue
                cc, seed, venue, title = cols[0], int(cols[1]), clean(cols[2]), clean(cols[3])
                if (cc, venue, title) in have:
                    continue
                takes = clean(cols[6]) if len(cols) > 6 else ""
                key = (cc, venue, title)
                if key not in out or seed < int(out[key][3]):
                    out[key] = [venue, title, cc, str(seed), date, ref, takes]
    return [out[k] for k in sorted(out)]


def file(asset: str, backlog: str, seeds: str, owner: str) -> list[str]:
    have, filed, rows = known(backlog), [], []
    for r in read_tsv(asset):
        key = (r.get("country", ""), r.get("venue", ""), r.get("rawTitle", ""))
        if not all(key) or not r.get("seed", "").isdigit() or key in have:
            continue
        have.add(key)
        rows.append(["drift", key[0], r["seed"], "", key[1], key[2], r.get("takes", ""), r.get("firstSeen", ""), owner,
                     f"explorer: found on seed {r['seed']} ({r.get('slice', '')}); cut its slice with scripts/identity-slice "
                     f"{key[0]} --venue V --title T --emit-drift-slice"])
        drift_seeds.add(key[0], int(r["seed"]), f"explorer: {key[1]} \"{key[2]}\"", r.get("firstSeen", ""), path=seeds)
        filed.append(f"filed {key[0]} {r['seed']} {key[1]} | {key[2]}")
    if rows:
        with open(backlog, "a", encoding="utf-8") as f:
            f.write("".join("\t".join(clean(c) for c in row) + "\n" for row in rows))
    return filed


def main(argv: list[str]) -> int:
    if not argv or argv[0] not in ("pick", "new-flips", "file"):
        print(__doc__ or "usage: drift_explorer.py pick|new-flips|file ...", file=sys.stderr)
        return 2
    command, rest, opts = argv[0], [], {}
    it = iter(argv[1:])
    for a in it:
        if a.startswith("--"):
            opts[a[2:]] = next(it, "")
        else:
            rest.append(a)
    try:
        if command == "pick":
            print(" ".join(map(str, pick(opts["cc"], int(opts.get("count", "1")), opts.get("salt", ""), opts.get("file") or None))))
        elif command == "new-flips":
            rows = new_flips(opts.get("backlog") or BACKLOG, opts["date"], opts.get("ref", ""), rest)
            sys.stdout.write("\t".join(NEW_HEADER) + "\n" + "".join("\t".join(r) + "\n" for r in rows))
        else:
            for line in file(opts["asset"], opts.get("backlog") or BACKLOG, opts.get("seeds") or drift_seeds.SEEDS_FILE,
                             opts.get("owner") or "convergence-fixer"):
                print(line)
    except (KeyError, OSError, ValueError) as e:
        print(f"drift_explorer: {e}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
