#!/usr/bin/env python3
# THE KNOWN-FLIPS BASELINE, generated -- never typed (the owner, 2026-10-29: "fail build only on new flips, focus in
# eliminating all old flips and then fail on all flips"). test/resources/fixtures/identity-flips-baseline.tsv holds one
# row per flip main's convergence already showed when the flip gate turned to failing on NEW flips only: the drift check
# (scripts.IdentityDrift --flips-baseline) passes a baselined listing's flips, counted ("N known flip(s) remaining"),
# fails every other, and names a row whose listing no longer flips "fixed, remove its row". The file only SHRINKS
# (scripts/ci/commit_rules.py refuses any added row, no override); once empty, every flip fails: the strict gate.
#
# usage:
#   identity_flips_baseline.py from-tsvs --first-seen DATE --owner WHO --note TEXT FLIPS_TSV...   rows of `flips-<cc>.tsv`
#   identity_flips_baseline.py from-run RUN [--owner WHO] [--out FILE]   the takes-drift rows' flips of an "Identity model
#                                                                        convergence" run (its identity-drift-* artifacts)
# Writes the whole file (header and rows) to --out, else stdout. Tested by identity_flips_baseline_test.py.

from __future__ import annotations

import csv
import glob
import json
import os
import subprocess
import sys
import tempfile

HEADER = """\
# THE KNOWN-FLIPS BASELINE (the owner, 2026-10-29: "fail build only on new flips, focus in eliminating all old flips and
# then fail on all flips"). One row per flip main's convergence showed when the gate turned to failing on NEW flips only,
# generated from a run's flips by scripts/ci/identity_flips_baseline.py -- never typed. The drift check
# (scripts/ci/identity-drift-check.sh, scripts.IdentityDrift --flips-baseline) passes a flip of a listing named here,
# counted loudly as "N known flips remaining" (a ::warning and the step summary), and FAILS every other flip.
# ONLY SHRINKS: a commit adding a row is refused, no override (scripts/ci/commit_rules.py). A commit fixing a flip
# removes its row; a run whose seed shows the listing no longer flips names it "fixed, remove its row".
# THE END STATE IS AUTOMATIC: with no row left, every flip fails -- the strict gate. Divergences have no baseline.
# Owner: the convergence red's fixer. A newborn check's own findings go TEMPORARY on identity-flips-allow.tsv instead.
country\tseed\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote
"""
FLIP_COLUMNS = ["country", "seed", "venue", "rawTitle", "films", "reasons", "takes"]


def clean(text: str) -> str:
    return text.replace("\t", " ").replace("\n", " ").strip()


def rows_of(paths: list[str], first_seen: str, owner: str, note: str) -> list[list[str]]:
    """One baseline row per (country, seed, venue, raw title) the flips TSVs name, sorted."""
    seen = {}
    for path in paths:
        with open(path, newline="", encoding="utf-8") as f:
            reader = csv.reader(f, delimiter="\t", quoting=csv.QUOTE_NONE)
            header = next(reader, None)
            if header is None:
                continue
            if header[: len(FLIP_COLUMNS)] != FLIP_COLUMNS:
                raise ValueError(f"{path}: not a flips TSV (header {header!r})")
            for cols in reader:
                if len(cols) < 5 or not cols[0]:
                    continue
                key = (cols[0], cols[1], cols[2], cols[3])
                reasons = cols[5] if len(cols) > 5 else ""
                seen.setdefault(key, [cols[0], cols[1], clean(cols[2]), clean(cols[3]), clean(cols[4]), first_seen, owner,
                                      clean(": ".join(p for p in (note, reasons) if p))])
    return [seen[k] for k in sorted(seen, key=lambda k: (k[0], int(k[1]) if k[1].isdigit() else 0, k[3], k[2]))]


def render(rows: list[list[str]]) -> str:
    return HEADER + "".join("\t".join(r) + "\n" for r in rows)


def gh(*args: str) -> str:
    tool = os.environ.get("KINOWO_GH") or "gh"
    return subprocess.run([tool, *args], check=True, capture_output=True, text=True, timeout=300).stdout


def from_run(run: str, owner: str) -> list[list[str]]:
    info = json.loads(gh("run", "view", run, "--json", "databaseId,number,createdAt,headSha,conclusion"))
    with tempfile.TemporaryDirectory() as tmp:
        gh("run", "download", str(info["databaseId"]), "--pattern", "identity-drift-*", "--dir", tmp)
        paths = sorted(glob.glob(os.path.join(tmp, "identity-drift-*", "flips-*.tsv")))
        if not paths:
            raise ValueError(f"run {run} holds no identity-drift-*/flips-*.tsv (did its takes-drift rows run?)")
        note = f"convergence #{info.get('number')} (run {info.get('databaseId')}, {str(info.get('headSha'))[:9]})"
        return rows_of(paths, str(info.get("createdAt", ""))[:10], owner or f"the fixer of convergence #{info.get('number')}", note)


def main(argv: list[str]) -> int:
    if not argv or argv[0] not in ("from-tsvs", "from-run"):
        print(__doc__ or "usage: identity_flips_baseline.py from-tsvs|from-run ...", file=sys.stderr)
        return 2
    command, rest, opts = argv[0], [], {}
    it = iter(argv[1:])
    for a in it:
        if a.startswith("--"):
            opts[a[2:]] = next(it, "")
        else:
            rest.append(a)
    try:
        if command == "from-tsvs":
            if not rest or not opts.get("first-seen") or not opts.get("owner"):
                print("from-tsvs --first-seen DATE --owner WHO [--note TEXT] FLIPS_TSV...", file=sys.stderr)
                return 2
            rows = rows_of(rest, opts["first-seen"], opts["owner"], opts.get("note", ""))
        else:
            if not rest:
                print("from-run RUN [--owner WHO] [--out FILE]", file=sys.stderr)
                return 2
            rows = from_run(rest[0], opts.get("owner", ""))
    except (OSError, ValueError, subprocess.SubprocessError) as e:
        print(f"identity-flips-baseline: {e}", file=sys.stderr)
        return 2
    text = render(rows)
    if opts.get("out"):
        with open(opts["out"], "w", encoding="utf-8") as f:
            f.write(text)
        print(f"identity-flips-baseline: {len(rows)} row(s) -> {opts['out']}", file=sys.stderr)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
