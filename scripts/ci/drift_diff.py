#!/usr/bin/env python3
# THE DIFFERENTIAL DRIFT VERDICT (the owner, 2026-11-04: "a gate that only fails on what the change adds"). A takes-drift
# row replays its fixed seeds (scripts/ci/drift_seeds.py) at the HEAD and at its BASE, over the same recording, and
# hands each seed's two flips TSVs (scripts.IdentityDrift's flips-<cc>.tsv) here:
#   - a flip the head shows and the base does not -- a listing (venue, raw title) flipping on that seed at the head only
#     -- is the change's: it FAILS the row, unless a newborn check's TEMPORARY row on identity-flips-allow.tsv names it;
#   - a flip both show is PRE-EXISTING: printed as a `pre-existing (backlog)` line with its backlog row (owner, first
#     seen; identity-flip-backlog.tsv) or "not in the backlog yet", and never failed -- an old flip is the backlog's,
#     burnt down by its owner and found by the explorer (.github/workflows/identity-drift-explorer.yml), not a red on
#     whichever commit replays its seed;
#   - a flip only the base shows is one the change REMOVES: said, never failed.
# Divergences (incremental vs whole) are no flips: the head must have none beyond identity-drift-allow.tsv whatever the
# base did -- a divergence is a correctness bug, judged by the check itself, not here.
#
# The lines it prints are agentctl's view of the red (~/bin/lib/agentctl/wait.py, flip_listings): the head-only flips
# under `## <cc> flips the change adds ...` as `      <venue> | <title>: <takes> [seed S]` rows -- the red's key -- and
# the pre-existing and removed ones prefixed `pre-existing (backlog):` / `removed:`, which it never reads as the red.
#
# usage: drift_diff.py --cc CC --seed S --head FLIPS_TSV (--base FLIPS_TSV | --same-code) [--base-said TEXT]
#                      [--allow identity-flips-allow.tsv] [--backlog identity-flip-backlog.tsv] [--new-out TSV]
#   --same-code: the base's identity code and harness are the head's (the same key), so over the same recording and
#                seed its flips ARE the head's: every flip is pre-existing, nothing is replayed twice.
# Prints `new=N pre-existing=M removed=K` last; exit 0 none new, 1 some, 2 unreadable input. Tested by drift_diff_test.py.

from __future__ import annotations

import csv
import os
import sys


def read_tsv(path: str) -> list[dict]:
    with open(path, newline="") as f:
        lines = [line for line in f if line.strip() and not line.startswith("#")]
    return list(csv.DictReader(lines, delimiter="\t"))


def flips(path: str, seed: str) -> dict[tuple[str, str], dict]:
    """The flips of one seed, by listing (venue, raw title): the first row of each."""
    out = {}
    for row in read_tsv(path):
        if row.get("venue") is None or row.get("rawTitle") is None:
            raise ValueError(f"{path}: no venue/rawTitle columns")
        if row.get("seed", seed) != seed:
            continue
        out.setdefault((row["venue"], row["rawTitle"]), row)
    return out


def allowed(cc: str, path: str | None) -> list[tuple[str, str]]:
    if not path or not os.path.isfile(path):
        return []
    return [(r["venue"], r["rawTitle"]) for r in read_tsv(path) if r.get("country") == cc]


def backlog(cc: str, path: str | None) -> dict[tuple[str, str], dict]:
    if not path or not os.path.isfile(path):
        return {}
    return {(r["venue"], r["rawTitle"]): r for r in read_tsv(path)
            if r.get("country") == cc and r.get("kind", "drift") == "drift"}


def covered(key: tuple[str, str], allow: list[tuple[str, str]]) -> bool:
    return any((venue == "*" or venue == key[0]) and title == key[1] for venue, title in allow)


def options(argv):
    opts = {}
    it = iter(argv)
    for a in it:
        if a == "--same-code":
            opts["same-code"] = "1"
        elif a.startswith("--"):
            opts[a[2:]] = next(it, "")
        else:
            raise ValueError(f"unexpected argument {a!r}")
    return opts


def main(argv) -> int:
    try:
        o = options(argv)
        cc, seed = o["cc"], o["seed"]
        head = flips(o["head"], seed)
        base = dict(head) if o.get("same-code") else flips(o["base"], seed)
        allow = allowed(cc, o.get("allow"))
        known = backlog(cc, o.get("backlog"))
    except (KeyError, ValueError, OSError) as e:
        print(f"drift_diff.py: {e}", file=sys.stderr)
        return 2
    said = o.get("base-said") or ("the base (the same identity code)" if o.get("same-code") else "the base")
    added = sorted(k for k in head if k not in base)
    temporary = [k for k in added if covered(k, allow)]
    new = [k for k in added if k not in temporary]
    pre = sorted(k for k in head if k in base)
    removed = sorted(k for k in base if k not in head)

    def take(row):
        return " ".join((row.get("takes") or row.get("films") or "").split())

    print(f"## {cc} flips the change adds (seed {seed}, head vs {said}): {len(new)} flip(s), "
          f"{len(temporary)} allow-listed, {len(pre)} pre-existing, {len(removed)} removed")
    for k in new:
        print(f"      {k[0]} | {k[1]}: {take(head[k])} [seed {seed}]")
    for k in temporary:
        print(f"      allow-listed (identity-flips-allow.tsv): {k[0]} | {k[1]} [seed {seed}]")
    for k in pre:
        row = known.get(k)
        where = (f"backlog: owner {row.get('owner') or '?'}, first seen {row.get('firstSeen') or '?'}" if row
                 else "not in the backlog yet: the explorer files it")
        print(f"      pre-existing (backlog): {k[0]} | {k[1]} [seed {seed}] -- in the base too, not failed; {where}")
    for k in removed:
        print(f"      removed: {k[0]} | {k[1]} [seed {seed}] -- the base flips it, the head does not")
    if o.get("new-out"):
        cols = ["country", "seed", "venue", "rawTitle", "films", "reasons", "takes", "cause"]
        with open(o["new-out"], "w", newline="") as f:
            w = csv.writer(f, delimiter="\t", lineterminator="\n")
            w.writerow(cols)
            for k in new:
                w.writerow([head[k].get(c, "") for c in cols])
    print(f"new={len(new)} pre-existing={len(pre)} removed={len(removed)}")
    return 1 if new else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
