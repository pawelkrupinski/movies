#!/usr/bin/env python3
"""THE FAMILY EXPLORER'S ROWS (.github/workflows/identity-family-explorer.yml): how many shards each country's families
are split across, so each walk row stays under about 10 minutes (setup included) on a hosted runner.

The families are split by MEASURED COST, not by count (services.identity.FamilyConvergence.balance: longest first into the
lightest row, from test/resources/fixtures/family-costs-<cc>.tsv, the walk seconds the first cold run recorded); a family
of 64 or more listings -- the sampled tier, a handful of which cost most of a country's walk -- is walked with ORDERS
seeded arrival orders instead of 8, the orders changing every ISO week. The shard counts are the recorded costs at about
the recorded seconds that make about 6.5 minutes of a hosted runner's walk (the second cold run's ratios: DE 0.70, UK 0.77, PL 0.84, US 1.15). A family the costs do not name costs the average
of its members'.

  family_shards.py plan [--countries "de uk"] [--date YYYY-MM-DD]
        prints, for the workflow's plan job, `matrix=<json>`, `expect=<cc=n,...>`, `countries=<cc cc ...>` and `week=<ISO
        week number: the orders' salt>`; exit 2 on an unknown country
  family_shards.py costs --from DIR --out DIR
        rewrites <out>/family-costs-<cc>.tsv from the family-walks-<cc>.tsv files under DIR (a run's downloaded artifacts)

Tested by family_shards_test.py.
"""
import csv
import datetime
import glob
import json
import os
import sys

# country code -> shards; the order is the order the rows start in (the slowest first)
SHARDS = {"us": 11, "de": 11, "uk": 5, "pl": 2, "es": 1}
# the sampled tier's arrival orders a walk (FamilyConvergence's default is 8; at 8, then 2, four US rows took 20+ minutes)
ORDERS = 1


def plan(requested):
    unknown = [c for c in requested if c not in SHARDS]
    if unknown:
        raise ValueError(f"unknown country code(s): {' '.join(unknown)} (known: {' '.join(SHARDS)})")
    codes = [c for c in SHARDS if not requested or c in requested]
    include = [{"code": c, "index": i, "shards": SHARDS[c], "name": f"{c} {i}/{SHARDS[c]}"}
               for c in codes for i in range(1, SHARDS[c] + 1)]
    return {"include": include}, ",".join(f"{c}={SHARDS[c]}" for c in codes), " ".join(codes)


def costs(source, out):
    """The cost files: family, tier, subsets and seconds of every walked family of a country, from its shards' walks."""
    rows = {}
    for path in glob.glob(os.path.join(source, "**", "family-walks-??.tsv"), recursive=True):
        code = os.path.basename(path)[len("family-walks-"):][:2]
        with open(path, newline="", encoding="utf-8") as f:
            for row in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE):
                rows.setdefault(code, {})[row["family"]] = [row["family"], row["tier"], row["subsets"], row["seconds"]]
    for code, families in sorted(rows.items()):
        with open(os.path.join(out, f"family-costs-{code}.tsv"), "w", encoding="utf-8") as f:
            f.write("family\ttier\tsubsets\tseconds\n")
            for family in sorted(families):
                f.write("\t".join(families[family]) + "\n")
    return sorted(rows)


def main(argv):
    if argv and argv[0] == "costs" and len(argv) == 5 and argv[1] == "--from" and argv[3] == "--out":
        print(" ".join(costs(argv[2], argv[4])))
        return 0
    if not argv or argv[0] != "plan":
        print(__doc__, file=sys.stderr)
        return 2
    argv = list(argv)
    today = datetime.date.today()
    if "--date" in argv:
        i = argv.index("--date")
        today = datetime.date.fromisoformat(argv[i + 1])
        del argv[i:i + 2]
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
    print(f"week={today.isocalendar()[1]}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
