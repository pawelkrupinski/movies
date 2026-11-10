#!/usr/bin/env python3
"""THE FAMILY EXPLORER'S BOOKKEEPING (the owner, 2026-11-06, option A: the per-family convergence check is a NIGHTLY
explorer outside the gate; .github/workflows/identity-family-explorer.yml). Each walk row writes the flips of its shard
of one country's families (services.identity.FamilyConvergence, `family-flips-<cc>.tsv`, identity-flip-backlog.tsv's row
shape, kind `family`, the rules a listing's walk fired in the note). This joins them:

  family-flips-<cc>.tsv     one a country, its shards joined, sorted -- published only when EVERY shard of it is here
  family-flips-ranked.tsv   every country's flips by deciding rule, the rule with most listings first: the list structural
                            fix 2 (the rule burn-down) reads. Columns: rank, rule, listings (of this rule), country,
                            venue, rawTitle, shape (stage kind), tier, witness. A listing is under every DECIDING rule it
                            fired (accept/pooled/veto/join/apart): never a `refused:*` one -- Acceptance.refusals
                            records every alone rule that did not take a node, so every flip through `none` carries the
                            same dozen and they would top the ranking while deciding nothing (dated-title, 37 of 46
                            flips, 2026-11-08). A listing that fired only refusals is under REFUSALS_ONLY. The
                            country's file keeps every rule.
  family-flips-new.tsv      the flips last night's file for the country did not hold (the first night has no such file:
                            it is the baseline, nothing is new) -- reported, never filed as a backlog row: production
                            rarely reaches these states
A country with a shard missing keeps last night's file and its place in the ranking (said); nothing is half-published.

  family_flips_report.py --shards DIR --expect de=4,uk=4 --previous DIR --out DIR
      DIR/<any>/identity-family-<cc>-<i>of<n>/family-flips-<cc>.tsv are the walks' artifacts; --previous holds last
      night's family-flips-<cc>.tsv; prints a markdown summary on stdout. Exit 0, 2 on unreadable input.

Tested by family_flips_report_test.py.
"""
import argparse
import csv
import glob
import os
import re
import sys

BACKLOG_COLUMNS = ["kind", "country", "seed", "slice", "venue", "rawTitle", "takes", "firstSeen", "owner", "note"]
RANKED_COLUMNS = ["rank", "rule", "listings", "country", "venue", "rawTitle", "shape", "tier", "witness"]
NO_RULE = "(no rule recorded)"
REFUSALS_ONLY = "(refusals only)"
REFUSAL = "refused:"
# a note: "<stage kind>, ... [<tier>] <family> rules: <rule> <rule>" (FamilyConvergence.flipsTsv)
NOTE = re.compile(r"^(?P<shape>.*?) (?P<tier>\[[^\]]*\]) (?P<family>.*?)(?: rules: (?P<rules>.*))?$")


def read_rows(path):
    with open(path, newline="", encoding="utf-8") as f:
        lines = [line for line in f if line.strip() and not line.startswith("#")]
    reader = csv.reader(lines, delimiter="\t", quoting=csv.QUOTE_NONE)
    header = next(reader, None)
    if header != BACKLOG_COLUMNS:
        raise ValueError(f"{path}: header {header}, not {BACKLOG_COLUMNS}")
    return [dict(zip(header, cols + [""] * (len(header) - len(cols)))) for cols in reader]


def write_tsv(path, columns, rows):
    with open(path, "w", encoding="utf-8") as f:
        f.write("\t".join(columns) + "\n")
        for row in rows:
            f.write("\t".join(str(row.get(c, "")).replace("\t", " ").replace("\n", " ") for c in columns) + "\n")


def key(row):
    return (row["country"], row["venue"], row["rawTitle"])


def parse_note(note):
    m = NOTE.match(note)
    if not m:
        return note, "", []
    return m["shape"], m["tier"], (m["rules"] or "").split()


def deciding(rules):
    """The rules a listing ranks under: its non-refusal ones; REFUSALS_ONLY when it fired only refusals."""
    if not rules:
        return [NO_RULE]
    return [rule for rule in rules if not rule.startswith(REFUSAL)] or [REFUSALS_ONLY]


def ranked(rows):
    """The rows of RANKED_COLUMNS: a row a (listing, deciding rule), the rules with most listings first."""
    by_rule = {}
    for row in rows:
        shape, tier, rules = parse_note(row.get("note", ""))
        for rule in deciding(rules):
            by_rule.setdefault(rule, []).append({"country": row["country"], "venue": row["venue"], "rawTitle": row["rawTitle"],
                                                 "shape": shape, "tier": tier, "witness": row.get("takes", "")})
    order = sorted(by_rule, key=lambda rule: (-len(by_rule[rule]), rule in (NO_RULE, REFUSALS_ONLY), rule))
    out = []
    for rank, rule in enumerate(order, 1):
        for listing in sorted(by_rule[rule], key=lambda r: (r["country"], r["venue"], r["rawTitle"])):
            out.append({"rank": rank, "rule": rule, "listings": len(by_rule[rule]), **listing})
    return out


def joined(shards_dir, code, count):
    """(rows, missing): the country's shards' flips, and which shards (1-based) are not here."""
    rows, missing = [], []
    for i in range(1, count + 1):
        found = glob.glob(os.path.join(shards_dir, "**", f"identity-family-{code}-{i}of{count}", f"family-flips-{code}.tsv"), recursive=True)
        if not found:
            missing.append(i)
            continue
        rows += read_rows(found[0])
    return sorted(rows, key=key), missing


def report(shards_dir, expect, previous_dir, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    lines, current, new_rows, notes = [], [], [], []
    for code, count in expect.items():
        rows, missing = joined(shards_dir, code, count)
        before = os.path.join(previous_dir, f"family-flips-{code}.tsv")
        previous = read_rows(before) if os.path.isfile(before) else None
        if missing:
            notes.append(f"{code}: shard(s) {' '.join(map(str, missing))} of {count} missing -- last night's file kept, nothing published")
            current += previous or []
            continue
        write_tsv(os.path.join(out_dir, f"family-flips-{code}.tsv"), BACKLOG_COLUMNS, rows)
        current += rows
        if previous is None:
            notes.append(f"{code}: no file from an earlier night -- this one is the baseline, nothing is new")
        else:
            held = {key(r) for r in previous}
            new_rows += [r for r in rows if key(r) not in held]
        lines.append(f"| {code} | {count} | {len(rows)} | {sum(1 for r in new_rows if r['country'] == code) if previous is not None else 'baseline'} |")
    write_tsv(os.path.join(out_dir, "family-flips-ranked.tsv"), RANKED_COLUMNS, ranked(current))
    write_tsv(os.path.join(out_dir, "family-flips-new.tsv"), BACKLOG_COLUMNS, new_rows)

    top = {}
    for r in ranked(current):
        top.setdefault((r["rank"], r["rule"]), r["listings"])
    out = ["### Family explorer: flips by country", "", "| country | shards | flipping listings | new since last night |", "|---|---|---|---|", *lines, ""]
    out += [f"- {n}" for n in notes] + ([""] if notes else [])
    out += ["Top rules (listings): " + (", ".join(f"{rule} {n}" for (_, rule), n in sorted(top.items())[:8]) or "none"), ""]
    if new_rows:
        out += ["New since last night:", "", "```", *(f"{r['country']} | {r['venue']} | {r['rawTitle']}: {r['takes']}" for r in new_rows), "```"]
    return "\n".join(out)


def main(argv=None):
    p = argparse.ArgumentParser(description="the family explorer's report")
    p.add_argument("--shards", required=True)
    p.add_argument("--expect", required=True, help="cc=shards,cc=shards")
    p.add_argument("--previous", required=True)
    p.add_argument("--out", required=True)
    a = p.parse_args(argv)
    try:
        expect = {c: int(n) for c, n in (pair.split("=") for pair in a.expect.split(",") if pair)}
        print(report(a.shards, expect, a.previous, a.out))
    except (ValueError, OSError) as e:
        print(f"family_flips_report.py: {e}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
