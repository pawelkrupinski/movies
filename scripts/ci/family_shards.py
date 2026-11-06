#!/usr/bin/env python3
"""THE FAMILY EXPLORER'S ROWS (.github/workflows/identity-family-explorer.yml): how many shards each country's families
are split across, so each walk row stays near 8 minutes on a hosted runner.

The shard counts come from the cold walks measured on the laptop (4 threads, 2026-11-06: DE 822 s, UK 898 s, PL 370 s,
US more than 2870 s -- stopped unfinished) at two to three times as slow on a hosted runner (the families split by their
first member's key, services.identity.FamilyConvergence.shardOf). Spain was not measured: its count is a guess for the
first night's runtimes to correct. A warm night (an unchanged recording, rules the change did not reach) walks a fraction.

  family_shards.py plan [--countries "de uk"]   prints, for the workflow's plan job, `matrix=<json>`, `expect=<cc=n,...>`
                                                and `countries=<cc cc ...>`; exit 2 on an unknown country

Tested by family_shards_test.py.
"""
import json
import sys

# country code -> shards; the order is the order the rows start in (the slowest first)
SHARDS = {"us": 14, "uk": 4, "de": 4, "pl": 2, "es": 3}
ROW_MINUTES = 8


def plan(requested):
    unknown = [c for c in requested if c not in SHARDS]
    if unknown:
        raise ValueError(f"unknown country code(s): {' '.join(unknown)} (known: {' '.join(SHARDS)})")
    codes = [c for c in SHARDS if not requested or c in requested]
    include = [{"code": c, "index": i, "shards": SHARDS[c], "name": f"{c} {i}/{SHARDS[c]}"}
               for c in codes for i in range(1, SHARDS[c] + 1)]
    return {"include": include}, ",".join(f"{c}={SHARDS[c]}" for c in codes), " ".join(codes)


def main(argv):
    if not argv or argv[0] != "plan":
        print(__doc__, file=sys.stderr)
        return 2
    countries = []
    if len(argv) >= 3 and argv[1] == "--countries":
        countries = argv[2].replace(",", " ").split()
    try:
        matrix, expect, codes = plan(countries)
    except ValueError as e:
        print(f"family_shards.py: {e}", file=sys.stderr)
        return 2
    print(f"matrix={json.dumps(matrix, separators=(',', ':'))}")
    print(f"expect={expect}")
    print(f"countries={codes}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
