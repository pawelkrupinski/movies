#!/usr/bin/env python3
"""Which title rules fire on prod's live listings, beside the recordings: dead in prod vs dead in the recordings.

  scripts/rule-firing-prod [--window 7d] [--country cc] [--all-kinds] [--tsv]

Reads, over `--window`:
  Prometheus   kinowo_worker_identity_rule_fires_total{rule}: the live listings each rule fired on, added up over every
               written whole-corpus projection (RuleFiringCount), and _rule_counted_listings_total, the listings counted
               (over ssh monitoring-1, read-only; $RULE_FIRING_MONITORING_HOST overrides)
  title-rules.tsv            common/src/main/resources/: every title rule's id, kind and season
  the consolidation inventory docs/investigations/2026-10-22-rule-consolidation.md: each row's firing counts in the
                             ablation's recordings (its `recordings (by country)` column)

and sorts every title-rules.tsv row into: DEAD IN BOTH (fired on no live listing and in no recording), DEAD IN PROD ONLY,
DEAD IN RECORDINGS ONLY, LIVE IN BOTH, and rows the inventory does not list (added or merged since). A seasonal row
(its `season` column) is marked: a window outside its season cannot see it. `--all-kinds` also lists the model's and the
later stages' rule ids (accept:, veto:, apart:, stage:, …) by their firings, the zero ones first. `--tsv`: one row a rule.
Exit 2 when Prometheus has counted no listing over the window (the counter not deployed yet, or the window too short).
"""

import argparse
import json
import os
import re
import shlex
import subprocess
import sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RULES_TSV = os.path.join(REPO, "common/src/main/resources/title-rules.tsv")
INVENTORY = os.path.join(REPO, "docs/investigations/2026-10-22-rule-consolidation.md")
PROMETHEUS = "http://10.20.0.11:9090/api/v1/query"
MONITORING_HOST = os.environ.get("RULE_FIRING_MONITORING_HOST", "monitoring-1")


def title_rules(text):
    """title-rules.tsv's rows: id -> (kind, season)."""
    rows = {}
    for line in text.splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        cells = line.split("\t")
        rows[cells[0]] = (cells[1] if len(cells) > 1 else "", cells[6] if len(cells) > 6 else "")
    return rows


def inventory(text):
    """The consolidation inventory's per-row table: id -> its recordings column ('pl:92, uk:3', '' for none)."""
    recorded = {}
    for line in text.splitlines():
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cells) == 10 and re.fullmatch(r"`[^`]+`", cells[0]) and re.fullmatch(r"\d+", cells[5]):
            recorded[cells[0].strip("`")] = cells[6]
    return recorded


def fired_in_recordings(column):
    """Whether a recordings column names any firing ('pl:92' yes, '' or 'pl:0' no)."""
    return any(int(n) > 0 for n in re.findall(r":\s*(\d+)", column))


def selector(country):
    return f'{{country="{country}"}}' if country else ""


def queries(window, country):
    sel = selector(country)
    return (f"sum by (rule) (increase(kinowo_worker_identity_rule_fires_total{sel}[{window}]))",
            f"sum(increase(kinowo_worker_identity_rule_counted_listings_total{sel}[{window}]))")


def vector(response):
    """A Prometheus instant vector as {rule (or ''): value}."""
    data = json.loads(response)
    if data.get("status") != "success":
        raise ValueError(f"Prometheus said {data.get('status')}: {data.get('error', '')}")
    return {r["metric"].get("rule", ""): float(r["value"][1]) for r in data["data"]["result"]}


def classify(rules, recorded, prod):
    """Each title rule's class, from its prod firings (`prod`: 'title:<id>' -> listings) and the inventory."""
    out = []
    for rule_id, (kind, season) in sorted(rules.items()):
        live_prod = prod.get(f"title:{rule_id}", 0) > 0
        if rule_id not in recorded:
            cls = "NOT IN INVENTORY (live in prod)" if live_prod else "NOT IN INVENTORY (dead in prod)"
        else:
            live_rec = fired_in_recordings(recorded[rule_id])
            cls = {(False, False): "DEAD IN BOTH", (False, True): "DEAD IN PROD ONLY",
                   (True, False): "DEAD IN RECORDINGS ONLY", (True, True): "LIVE IN BOTH"}[(live_prod, live_rec)]
        out.append((cls, rule_id, kind, season, round(prod.get(f"title:{rule_id}", 0)), recorded.get(rule_id, "")))
    return out


ORDER = ["DEAD IN BOTH", "DEAD IN PROD ONLY", "DEAD IN RECORDINGS ONLY", "NOT IN INVENTORY (dead in prod)",
         "NOT IN INVENTORY (live in prod)", "LIVE IN BOTH"]


def render(rows, counted, window, others=None):
    lines = [f"rule-firing-prod: {len(rows)} title rules over {window}, {round(counted)} listings counted"]
    for cls in ORDER:
        members = [r for r in rows if r[0] == cls]
        lines.append(f"\n{cls} ({len(members)})")
        if cls == "LIVE IN BOTH":
            continue
        for _, rule_id, kind, season, fires, rec in members:
            lines.append(f"  {rule_id}  [{kind}{', season ' + season if season else ''}]  prod {fires}  recordings {rec or '-'}")
    if others is not None:
        lines.append(f"\nOTHER RULES ({len(others)}), fewest firings first")
        for rule, fires in sorted(others.items(), key=lambda kv: (kv[1], kv[0])):
            lines.append(f"  {rule}  prod {round(fires)}")
    return "\n".join(lines)


def ask(query):
    command = ["ssh", MONITORING_HOST, f"curl -s {PROMETHEUS} --data-urlencode {shlex.quote('query=' + query)}"]
    done = subprocess.run(command, capture_output=True, text=True, timeout=120)
    if done.returncode != 0:
        raise RuntimeError(f"ssh {MONITORING_HOST} failed: {done.stderr.strip()[:300]}")
    return done.stdout


def main(argv, ask=ask, read=lambda path: open(path, encoding="utf-8").read()):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--window", default="7d")
    parser.add_argument("--country", default="")
    parser.add_argument("--all-kinds", action="store_true")
    parser.add_argument("--tsv", action="store_true")
    args = parser.parse_args(argv)
    fires_q, counted_q = queries(args.window, args.country)
    prod = vector(ask(fires_q))
    counted = sum(vector(ask(counted_q)).values())
    if counted <= 0:
        print(f"rule-firing-prod: Prometheus counted no listing over {args.window}: the counter is not deployed yet, or the "
              "window is shorter than a whole-corpus projection", file=sys.stderr)
        return 2
    rows = classify(title_rules(read(RULES_TSV)), inventory(read(INVENTORY)), prod)
    if args.tsv:
        print("class\trule\tkind\tseason\tprod\trecordings")
        for row in rows:
            print("\t".join(str(c) for c in row))
    else:
        others = {r: v for r, v in prod.items() if not r.startswith("title:")} if args.all_kinds else None
        print(render(rows, counted, args.window, others))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
