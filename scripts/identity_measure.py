#!/usr/bin/env python3
"""THE WHOLE-CORPUS MEASUREMENT's classification: every take a commit moved, per country, split by its judgement.

scripts/identity-measure runs the identity takes check (scripts/ci/identity-takes-check.sh, the `takes` row of each
hermetic convergence leg) for one commit against one base, downloads each row's `identity-takes-<cc>` artifact -- the
check's own output: {base,head}/decisions-<cc>.jsonl (the model's takes), {base,head}/ratchet-takes.tsv (the final
takes), verdict.txt, base.txt -- and hands the directory here.

The moves are the check's: identity_takes_diff.py pairs the listings and names each lost / changed / gained take
(`pair`, `moves_of`), on the model's takes and on the final ones. This module only says, per move, what the judgements
(test/resources/fixtures/identity-judgements.tsv) make of its two films:

  cure            the base's film was judged wrong; the head takes none, or a film judged right
  right-lost      the base's film was judged right; the head takes none, or a film nobody judged
  right->wrong    the base's film was judged right; the head's wrong
  wrong->wrong    both films judged wrong (the head's state is named: known-wrong, undecided, open)
  new-wrong       the head's film is judged wrong, the base's was not judged (a change or a gain)
  gained-right    the base took none; the head takes a film judged right
  gained-unjudged the base took none; the head takes a film nobody judged
  to-right        the base's film was not judged; the head's is judged right
  right->right    both judged right (two right films for one billing)
  accepted        an accepted loss in the judgements accepts the move
  unjudged        neither film judged (a loss, a change, a wrong->unjudged)

LANDABLE only when, over every country and both stages, right-lost, right->wrong and new-wrong are 0 and every
country's check produced its moves (a row that could not run, or left no artifact, is no measurement); every unjudged
move is then listed in full, to judge before landing.

usage: identity_measure.py <download dir> --judgements F [--countries pl,uk,...] [--rows rows.json] [--run-url U]
                           [--json] [--examples N]
  <download dir>/identity-takes-<cc>/   the row's artifact, as `gh run download` lays it out
  rows.json                             {cc: "success" | "failure" | ...}: each takes row's own conclusion
Exit 0 LANDABLE, 1 NOT LANDABLE, 2 unreadable input.
"""

import argparse
import json
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
import identity_takes_diff as takes  # noqa: E402
from identity_judgements import accepted as accepted_rows, labels_by_country, label_of, normalise, read  # noqa: E402

COUNTRIES = ("pl", "uk", "de", "us", "es")
BUCKETS = ("cure", "right-lost", "right->wrong", "wrong->wrong", "new-wrong", "gained-right", "gained-unjudged",
           "to-right", "right->right", "accepted", "unjudged")
BLOCKING = ("right-lost", "right->wrong", "new-wrong")
STAGES = (("model", "decisions-{cc}.jsonl"), ("final", "ratchet-takes.tsv"))


def bucket_of(move, before, after):
    """The bucket of one move, given the judgements' verdicts (right / wrong / None) on its base and head films."""
    if before == "wrong":
        if after == "wrong":
            return "wrong->wrong"
        if move["after"] is None or after == "right":
            return "cure"
    if after == "wrong":
        return "right->wrong" if before == "right" else "new-wrong"
    if move.get("acceptedLoss"):
        return "accepted"
    if before == "right":
        return "right->right" if after == "right" else "right-lost"
    if move["kind"] == "gained":
        return "gained-right" if after == "right" else "gained-unjudged"
    if after == "right":
        return "to-right"
    return "unjudged"


def states_of(rows, cc):
    """{(venue, rawTitle, film): state} of the wrong rows -- known-wrong, or an undecided suspect -- for `cc`."""
    out = {}
    for r in rows:
        if r.country == cc and (r.verdict == "wrong" or r.verdict == "undecided"):
            out[(r.venue, normalise(r.rawTitle), r.film)] = "undecided" if r.verdict == "undecided" else r.state
    return out


def state_of(states, venue, title, film):
    title = normalise(title)
    return states.get((venue, title, film)) or states.get(("*", title, film))


def classify(cc, base, head, rows):
    """{bucket: [move]} for one country and stage: the check's moves (identity_takes_diff.moves_of), each bucketed."""
    labels = labels_by_country(rows)[cc]
    accepted = [(v, t, f, "none") for v, t, f in accepted_rows(rows, cc)]
    pairs, _, _ = takes.pair(base, head)
    states = states_of(rows, cc)
    out = {b: [] for b in BUCKETS}
    for m in takes.moves_of(cc, pairs, labels, accepted):
        before = label_of(labels, m["venue"], m["rawTitle"], takes.ref(m["before"]))
        after = label_of(labels, m["venue"], m["rawTitle"], takes.ref(m["after"]))
        m["acceptedLoss"] = takes.accepted_move(m, accepted)
        b = bucket_of(m, before, after)
        entry = {"kind": m["kind"], "venue": m["venue"], "rawTitle": m["rawTitle"], "before": m["old"], "after": m["new"],
                 "headWhy": m["newWhy"]}
        if b == "wrong->wrong":
            entry["target"] = state_of(states, m["venue"], m["rawTitle"], takes.ref(m["after"])) or "open"
        out[b].append(entry)
    return out


def read_country(directory, cc, rows):
    """{"verdict": [lines], "base": sha, "stages": {stage: buckets}, "missing": reason or None} of one country's artifact."""
    root = os.path.join(directory, f"identity-takes-{cc}")
    if not os.path.isdir(root):
        return {"verdict": [], "base": None, "stages": {}, "missing": "no identity-takes artifact (the row did not run, or failed before uploading)"}
    def text(name):
        path = os.path.join(root, name)
        if not os.path.exists(path):
            return ""
        with open(path, encoding="utf-8") as f:
            return f.read()
    verdict = [l for l in text("verdict.txt").splitlines() if l.strip()]
    out = {"verdict": verdict, "base": text("base.txt").strip() or None, "stages": {}, "missing": None}
    unchanged = any("nothing can have moved" in l or "no take can have moved" in l for l in verdict)
    for stage, name in STAGES:
        b_path, h_path = (os.path.join(root, side, name.format(cc=cc)) for side in ("base", "head"))
        if not (os.path.exists(b_path) and os.path.exists(h_path)):
            if unchanged:
                out["stages"][stage] = {b: [] for b in BUCKETS}
            else:
                out["missing"] = out["missing"] or f"no {stage} takes in the artifact (the check could not resolve both sides)"
            continue
        if stage == "final":
            base, head = takes.final_sides(b_path, h_path, cc)
        else:
            base, head = takes.load(b_path), takes.load(h_path)
        out["stages"][stage] = classify(cc, base, head, rows)
    return out


def landable(countries):
    """(landable?, [reason]) over every country's stages."""
    reasons = []
    for cc, c in countries.items():
        if c["missing"]:
            reasons.append(f"{cc}: {c['missing']}")
        for stage, buckets in c["stages"].items():
            for b in BLOCKING:
                if buckets[b]:
                    reasons.append(f"{cc} {stage}: {len(buckets[b])} {b}")
    return not reasons, reasons


def counts(buckets):
    moves = [m for ms in buckets.values() for m in ms]
    return {k: sum(1 for m in moves if m["kind"] == k) for k in ("lost", "changed", "gained")}


def move_line(m):
    target = f" [target {m['target']}]" if "target" in m else ""
    return f"{m['venue']} | {m['rawTitle']}: {m['before']} -> {m['after']}{target}"


SHORT = {"cure": "cure", "right-lost": "R-lost", "right->wrong": "R>W", "wrong->wrong": "W>W", "new-wrong": "new-W",
         "gained-right": "gain-R", "gained-unjudged": "gain-?", "to-right": ">R", "right->right": "R>R",
         "accepted": "acc", "unjudged": "unjdg"}


def render(countries, rows, examples=3):
    head = ["cc", "stage", "lost", "chgd", "gain"] + [SHORT[b] for b in BUCKETS] + ["takes row"]
    table = [head]
    for cc, c in countries.items():
        row_state = rows.get(cc, "?")
        first = (c["verdict"] or [""])[0][:70]
        if c["missing"] and not c["stages"]:
            table.append([cc, "-", "-", "-", "-"] + ["-"] * len(BUCKETS) + [f"{row_state}: {c['missing']}"])
            continue
        for stage, buckets in c["stages"].items():
            n = counts(buckets)
            table.append([cc, stage, str(n["lost"]), str(n["changed"]), str(n["gained"])] +
                         [str(len(buckets[b])) for b in BUCKETS] + [f"{row_state}" + (f": {first}" if stage == "model" and first else "")])
    widths = [max(len(r[i]) for r in table) for i in range(len(head) - 1)]
    out = ["  ".join(r[i].ljust(widths[i]) for i in range(len(widths))) + "  " + r[-1] for r in table]
    out.append("")
    out.append("buckets: " + ", ".join(f"{SHORT[b]}={b}" for b in BUCKETS))
    for cc, c in countries.items():
        for stage, buckets in c["stages"].items():
            for b in BUCKETS:
                ms = buckets[b]
                if not ms or b == "unjudged":
                    continue
                out.append(f"\n{cc} {stage} {b} ({len(ms)}):")
                out += [f"  {move_line(m)}" for m in ms[:examples]]
                if len(ms) > examples:
                    out.append(f"  ... {len(ms) - examples} more (--json lists them all)")
    unjudged = [(cc, stage, m) for cc, c in countries.items() for stage, buckets in c["stages"].items()
                for m in buckets["unjudged"] + buckets["gained-unjudged"]]
    if unjudged:
        out.append(f"\nEVERY UNJUDGED MOVE ({len(unjudged)}) -- judge each in test/resources/fixtures/identity-judgements.tsv "
                   "(scripts/label-lookup, scripts/identity-judge-cards) before landing:")
        out += [f"  {cc} {stage} {m['kind']:7} {move_line(m)}" for cc, stage, m in unjudged]
    return "\n".join(out)


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    p.add_argument("dir")
    p.add_argument("--judgements", required=True)
    p.add_argument("--countries", default=",".join(COUNTRIES))
    p.add_argument("--rows", help="a JSON file: {cc: the takes row's conclusion}")
    p.add_argument("--run-url", default="")
    p.add_argument("--json", action="store_true")
    p.add_argument("--examples", type=int, default=3)
    a = p.parse_args(argv)
    try:
        rows = read(a.judgements)
        conclusions = {}
        if a.rows:
            with open(a.rows, encoding="utf-8") as f:
                conclusions = json.load(f)
        countries = {cc: read_country(a.dir, cc, rows) for cc in a.countries.split(",") if cc}
    except (OSError, ValueError, KeyError, TypeError, IndexError) as e:
        print(f"identity-measure: could not read the inputs: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    ok, reasons = landable(countries)
    if a.json:
        print(json.dumps({"run": a.run_url, "landable": ok, "reasons": reasons,
                          "countries": {cc: dict(c, row=conclusions.get(cc)) for cc, c in countries.items()}},
                         ensure_ascii=False, indent=1))
    else:
        if a.run_url:
            print(f"run {a.run_url}\n")
        print(render(countries, conclusions, a.examples))
        print("\n" + ("LANDABLE: no right take lost or turned wrong, no new wrong take" if ok else
                      "NOT LANDABLE:\n" + "\n".join(f"  - {r}" for r in reasons)))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
