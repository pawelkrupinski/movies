#!/usr/bin/env python3
"""The SUSPECT GROUPINGS of one country: the clusters GroupingCoherence (identity) found members of more than one film in,
ranked, most suspect first -- report-only, never a verdict.

Reads the identity takes check's resolve dump (`decisions-<cc>.jsonl`, integration.IdentityResolveDump): every listing's
line, its `cluster`, and on each suspect cluster's first member a `grouping` field (score, flagged, signals, the members a
finding names, each finding). Writes:

  <out>/grouping-suspects-<cc>.json  every cluster with a finding, ranked, each member's venue and billing, and how many
                                     of its members the hand judgements (identity-judgements.tsv) call right or wrong
  <out>/grouping-suspects-<cc>.md    the top --top flagged as a table, appended to --summary (the step summary) too

and, with --events (or CI_METRICS_FILE / RUNNER_TEMP set), one `identity_suspect_groupings` line to the job's CI metrics
(scripts/ci/job_metrics.py): ci_trends exports it to VictoriaLogs, which Grafana trends per country.

Usage: grouping_suspects.py <cc> <decisions.jsonl> --out DIR [--judgements TSV] [--summary FILE] [--top N] [--events]
Exit 0 always on a readable dump (a report), 2 on usage or an unreadable dump."""
import argparse
import csv
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

KIND = "identity_suspect_groupings"


def read_dump(path):
    """{cluster: {"members": [line, ...], "grouping": field or None}} from a decisions dump."""
    clusters = {}
    with open(path, encoding="utf-8") as handle:
        for raw in handle:
            if not raw.strip():
                continue
            line = json.loads(raw)
            cluster = clusters.setdefault(line.get("cluster", -1), {"members": [], "grouping": None})
            cluster["members"].append(line)
            if line.get("grouping"):
                cluster["grouping"] = line["grouping"]
    return clusters


def read_judgements(path, country):
    """{(venue or "*", rawTitle, film): verdict} of `country`'s hand judgements."""
    judged = {}
    if not path or not os.path.exists(path):
        return judged
    with open(path, encoding="utf-8") as handle:
        rows = csv.DictReader((line for line in handle if not line.startswith("#")), delimiter="\t")
        for row in rows:
            if row.get("country") == country:
                judged[(row.get("venue"), row.get("rawTitle"), row.get("film"))] = row.get("verdict")
    return judged


def verdict_of(judged, member):
    film = "tmdb:%s" % member["film"] if member.get("film") is not None else None
    return judged.get((member.get("venue"), member.get("rawTitle"), film)) or judged.get(("*", member.get("rawTitle"), film))


def suspects(clusters, judged):
    """Every cluster with a finding, ranked: flagged first, then by score, by members a finding names, by size."""
    out = []
    for cluster, found in clusters.items():
        grouping = found["grouping"]
        if not grouping:
            continue
        involved = set(grouping.get("involved") or [])
        verdicts = [verdict_of(judged, m) for m in found["members"]]
        members = sorted(({"venue": m.get("venue"), "rawTitle": m.get("rawTitle"), "involved": m.get("key") in involved}
                          for m in found["members"]), key=lambda m: (not m["involved"], m["venue"] or "", m["rawTitle"] or ""))
        out.append(dict(cluster=cluster, score=grouping.get("score", 0), flagged=bool(grouping.get("flagged")),
                        film=grouping.get("film"), title=grouping.get("title"), size=len(found["members"]),
                        signals=grouping.get("signals") or [], findings=grouping.get("findings") or [],
                        judged_right=verdicts.count("right"), judged_wrong=verdicts.count("wrong"),
                        involved=len(involved), members=members))
    out.sort(key=lambda s: (not s["flagged"], -s["score"], -s["involved"], -s["size"], s["film"] or ""))
    for rank, suspect in enumerate(out, 1):
        suspect["rank"] = rank
    return out


def totals(country, ranked):
    flagged = [s for s in ranked if s["flagged"]]
    return {"country": country, "clusters": len(ranked), "flagged": len(flagged),
            "facts": sum(1 for s in flagged if "facts" in s["signals"]),
            "alone": sum(1 for s in flagged if "alone" in s["signals"]),
            "judged_right": sum(1 for s in flagged if s["judged_right"] and not s["judged_wrong"]),
            "judged_wrong": sum(1 for s in flagged if s["judged_wrong"]),
            "top_score": max((s["score"] for s in flagged), default=0)}


def cell(text, limit=160):
    text = " ".join(str(text).split()).replace("|", "\\|")
    return text if len(text) <= limit else text[:limit - 1] + "…"


def table(country, ranked, top):
    figures = totals(country, ranked)
    lines = ["### Suspect groupings (%s): %d flagged of %d clusters with a finding -- report-only" % (
        country, figures["flagged"], figures["clusters"]), "",
        "Clusters whose members' own facts conflict, or a member of which would resolve to another film alone "
        "(`GroupingCoherence`). Judged: members the hand judgements call right / wrong.", ""]
    flagged = [s for s in ranked if s["flagged"]][:top]
    if not flagged:
        return "\n".join(lines + ["_none flagged_", ""])
    lines += ["| # | score | film | members | signals | evidence | judged |", "|---|---|---|---|---|---|---|"]
    for s in flagged:
        evidence = "; ".join("%s: %s" % (f["kind"], f["why"]) for f in s["findings"][:2])
        lines.append("| %d | %.2f | %s %s | %d | %s | %s | %d / %d |" % (
            s["rank"], s["score"], s["film"], cell(s["title"], 60), s["size"], ", ".join(s["signals"]), cell(evidence),
            s["judged_right"], s["judged_wrong"]))
    return "\n".join(lines + [""])


def emit(figures):
    import job_metrics
    job_metrics.append(dict(kind=KIND, **figures))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("country")
    parser.add_argument("dump")
    parser.add_argument("--out", required=True)
    parser.add_argument("--judgements")
    parser.add_argument("--summary")
    parser.add_argument("--top", type=int, default=20)
    parser.add_argument("--events", action="store_true")
    args = parser.parse_args(argv)
    try:
        clusters = read_dump(args.dump)
    except (OSError, ValueError) as error:
        print("grouping suspects (%s): no dump to read: %s" % (args.country, error), file=sys.stderr)
        return 2
    ranked = suspects(clusters, read_judgements(args.judgements, args.country))
    figures = totals(args.country, ranked)
    os.makedirs(args.out, exist_ok=True)
    with open(os.path.join(args.out, "grouping-suspects-%s.json" % args.country), "w", encoding="utf-8") as handle:
        json.dump(dict(figures, suspects=ranked), handle, ensure_ascii=False, indent=1)
    text = table(args.country, ranked, args.top)
    with open(os.path.join(args.out, "grouping-suspects-%s.md" % args.country), "w", encoding="utf-8") as handle:
        handle.write(text)
    if args.summary:
        with open(args.summary, "a", encoding="utf-8") as handle:
            handle.write(text + "\n")
    if args.events or os.environ.get("CI_METRICS_FILE") or os.environ.get("GITHUB_ACTIONS") == "true":
        emit(figures)
    print("grouping suspects (%s): %d flagged of %d clusters with a finding (facts %d, alone %d; judged right %d, wrong %d)" % (
        args.country, figures["flagged"], figures["clusters"], figures["facts"], figures["alone"], figures["judged_right"],
        figures["judged_wrong"]))
    return 0


if __name__ == "__main__":
    sys.exit(main())
