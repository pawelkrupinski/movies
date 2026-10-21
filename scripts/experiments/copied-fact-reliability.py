#!/usr/bin/env python3
"""Per feed: how often its copied facts back the judged-right film vs a judged-wrong one.

Reads copied-<cc>.jsonl files (integration.CopiedFactReliability). For each (listing, judged film) of a feed-sourced
listing, S = the summed calibrated contribution of the facts the feed copied (year.delta, director, runtime.delta,
originalTitle, only those copied and present). Counts, per feed: right/wrong film x copy supports (S>0) / dissents (S<0)
/ silent (S==0), each listing key counted once per film.
usage: copied-fact-reliability.py <copied-*.jsonl>... [--by-evidence]
"""
import json, sys, collections, math

SIG = {"Year": "year.delta", "Directors": "director", "Runtime": "runtime.delta", "OriginalTitle": "originalTitle"}
files = [a for a in sys.argv[1:] if not a.startswith("--")]
human_only = "--human" in sys.argv
counts = collections.defaultdict(collections.Counter)
taken = collections.defaultdict(collections.Counter)
examples = collections.defaultdict(list)
seen = set()
for path in files:
    cc = path.rsplit("copied-", 1)[-1].split(".")[0]
    for line in open(path):
        r = json.loads(line)
        src = r["source"]
        if src == "venue":
            continue
        signals = [SIG[f] for f in r["copied"] if f in SIG]
        for f in r["films"]:
            s = sum(v for k, v in f["weights"].items() if k in signals)
            sign = "+" if s > 0.05 else "-" if s < -0.05 else "0"
            if f["id"] == r["taken"]:
                taken[src][sign] += 1
            v = f["verdict"]
            if v not in ("right", "wrong"):
                continue
            if human_only and not (f["evidence"] or "").startswith(("review page:", "hand label:")):
                continue
            key = (path, cc, r["venue"], r["rawTitle"], f["id"])
            if key in seen:
                continue
            seen.add(key)
            counts[src][v + sign] += 1
            if (v, sign) in (("wrong", "+"), ("right", "-")):
                examples[src].append(f'{cc} {r["venue"]} | {r["rawTitle"]} -> {f["title"]} ({f.get("year")}) {v} S={s:.2f}')

print("feed\tR+\tR-\tR0\tW+\tW-\tW0\tP(right|support)\tP(wrong|dissent)\ttaken + / - / 0")
for src in sorted(set(counts) | set(taken)):
    c = counts[src]
    sup = c["right+"] + c["wrong+"]
    dis = c["right-"] + c["wrong-"]
    ps = f'{(c["right+"] + 0.5) / (sup + 1):.3f}' if sup else "-"
    pd = f'{(c["wrong-"] + 0.5) / (dis + 1):.3f}' if dis else "-"
    t = taken[src]
    print(f'{src}\t{c["right+"]}\t{c["right-"]}\t{c["right0"]}\t{c["wrong+"]}\t{c["wrong-"]}\t{c["wrong0"]}\t{ps}\t{pd}\t{t["+"]}/{t["-"]}/{t["0"]}')
if "--examples" in sys.argv:
    for src, ex in examples.items():
        print(f"\n## {src}")
        for e in sorted(set(ex))[:60]:
            print(e)
