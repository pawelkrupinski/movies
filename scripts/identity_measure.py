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

THE KIND of measure, printed first (`measure: full` / `measure: footprint`): `full` is the takes rows' resolve of the whole
recording at base and head; `footprint` is scripts/identity-measure --footprint's proof, on the laptop, that a title-rule
change reads every title the base's resolve read alike and so moves no take (its countries' takes are then unmoved,
"no take can have moved"). A footprint never stands for the full measure a change lands on.

LANDABLE only when, over every country and both stages, right-lost, right->wrong and new-wrong are 0 and every
country's check produced its moves (a row that could not run, or left no artifact, is no measurement); every unjudged
move is then listed in full, to judge before landing. A country the run held no takes row of (rows.json names the rows
it held: a footprint-limited run holds only its countries'), or whose row was skipped, and that left no artifact, is
NOT MEASURED (outside the footprint): shown as such, and no reason either way -- though a run measuring no country at all
is no measurement.

usage: identity_measure.py <download dir> --judgements F [--countries pl,uk,...] [--rows rows.json] [--run-url U]
                           [--json] [--examples N] [--moved-out FILE] [--kind full|footprint]
  <download dir>/identity-takes-<cc>/   the row's artifact, as `gh run download` lays it out
  rows.json                             {cc: "success" | "failure" | ...}: each takes row's own conclusion
Exit 0 LANDABLE, 1 NOT LANDABLE, 2 unreadable input.
"""

import argparse
import json
import os
import sys
from typing import Any, Dict, List, Optional, Sequence, Tuple, TypedDict

sys.path[:0] = [os.path.join(os.path.dirname(os.path.abspath(__file__)), part) for part in ("ci", "lib")]
import identity_takes_diff as takes  # noqa: E402
from seams.tool_files import Files  # noqa: E402
from identity_judgements import Row, country_views, label_of, normalise, own_or_every, read  # noqa: E402

COUNTRIES = ("pl", "uk", "de", "us", "es")
BUCKETS = ("cure", "right-lost", "right->wrong", "wrong->wrong", "new-wrong", "gained-right", "gained-unjudged",
           "to-right", "right->right", "accepted", "unjudged")
BLOCKING = ("right-lost", "right->wrong", "new-wrong")
KINDS = ("full", "footprint")
Move = Dict[str, Any]
Buckets = Dict[str, List[Move]]
KIND_LINE = {"full": "measure: full -- every take of the recording resolved at the base and the head",
             "footprint": "measure: footprint -- the base's title readings read again under the head's rules (scripts/identity-measure "
                          "--footprint); the full measure is still the gate a change lands on"}
STAGES = (("model", "decisions-{cc}.jsonl"), ("final", "ratchet-takes.tsv"))


def _wrong_bucket(move: Move, before: Optional[str], after: Optional[str]) -> Optional[str]:
    """The bucket of a move whose base or head film is judged wrong, or None when neither is."""
    if before == "wrong" and after == "wrong":
        return "wrong->wrong"
    if before == "wrong" and (move["after"] is None or after == "right"):
        return "cure"
    if after == "wrong":
        return "right->wrong" if before == "right" else "new-wrong"
    return None


def _judged_bucket(move: Move, before: Optional[str], after: Optional[str]) -> str:
    """The bucket of a move neither of whose films is judged wrong, by what the base and head films are judged."""
    if before == "right":
        return "right->right" if after == "right" else "right-lost"
    if move["kind"] == "gained":
        return "gained-right" if after == "right" else "gained-unjudged"
    return "to-right" if after == "right" else "unjudged"


def bucket_of(move: Move, before: Optional[str], after: Optional[str]) -> str:
    """The bucket of one move, given the judgements' verdicts (right / wrong / None) on its base and head films."""
    return _wrong_bucket(move, before, after) or ("accepted" if move.get("acceptedLoss") else _judged_bucket(move, before, after))


def states_of(rows, cc):
    """{(venue, rawTitle, film): state} of the wrong rows -- known-wrong, or an undecided suspect -- for `cc`."""
    out = {}
    for r in rows:
        if r.country == cc and (r.verdict == "wrong" or r.verdict == "undecided"):
            out[(r.venue, normalise(r.rawTitle), r.film)] = "undecided" if r.verdict == "undecided" else r.state
    return out


def classify(cc: str, base: List[Move], head: List[Move], rows: List[Row]) -> Buckets:
    """{bucket: [move]} for one country and stage: the check's moves (identity_takes_diff.moves_of), each bucketed."""
    labels, accepted, _ = country_views(rows, cc)
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
            entry["target"] = own_or_every(states, m["venue"], m["rawTitle"], takes.ref(m["after"])) or "open"
        out[b].append(entry)
    return out


UNMEASURED = "not measured (outside the footprint)"


class Country(TypedDict):
    verdict: List[str]
    base: Optional[str]
    stages: Dict[str, Dict[str, list]]
    missing: Optional[str]
    unmeasured: bool


def _absent_country(cc: str, conclusions: Optional[Dict[str, str]]) -> Country:
    """A country with no artifact: never run (outside the footprint), or failed before uploading."""
    if conclusions and conclusions.get(cc, "skipped") == "skipped":
        return {"verdict": [], "base": None, "stages": {}, "missing": None, "unmeasured": True}
    return {"verdict": [], "base": None, "stages": {}, "unmeasured": False,
            "missing": "no identity-takes artifact (the row did not run, or failed before uploading)"}


def _stage_buckets(files: Files, root: str, cc: str, stage: str, name: str, rows: List[Row]) -> Optional[Buckets]:
    """One stage's buckets from the artifact's base and head takes, or None when either side did not write them."""
    b_path, h_path = (files.path(root, side, name.format(cc=cc)) for side in ("base", "head"))
    if not (files.exists(b_path) and files.exists(h_path)):
        return None
    if stage == "final":
        base, head = takes.final_sides(files, b_path, h_path, cc)
    else:
        base, head = takes.load(files, b_path), takes.load(files, h_path)
    return classify(cc, base, head, rows)


def _read_stages(files: Files, root: str, cc: str, rows: List[Row], unchanged: bool) -> Tuple[Dict[str, Buckets], Optional[str]]:
    """Every stage's buckets, and why the first stage with none has none (a side that wrote none: the check could not
    resolve both sides) -- none moved, when the check says nothing can have."""
    stages: Dict[str, Buckets] = {}
    missing = None
    for stage, name in STAGES:
        buckets = _stage_buckets(files, root, cc, stage, name, rows)
        if buckets is None and unchanged:
            buckets = {b: [] for b in BUCKETS}
        if buckets is not None:
            stages[stage] = buckets
        else:
            missing = missing or f"no {stage} takes in the artifact (the check could not resolve both sides)"
    return stages, missing


def read_country(files: Files, directory, cc, rows, conclusions=None) -> Country:
    """{"verdict": [lines], "base": sha, "stages": {stage: buckets}, "missing": reason or None, "unmeasured": bool} of one
    country's artifact. `conclusions`, the run's takes rows ({cc: conclusion}), tells a row that never ran from one that
    failed before uploading."""
    root = files.path(directory, f"identity-takes-{cc}")
    if not files.is_dir(root):
        return _absent_country(cc, conclusions)
    def text(name):
        return files.read_text_or(files.path(root, name), "")
    verdict = [l for l in text("verdict.txt").splitlines() if l.strip()]
    unchanged = any("nothing can have moved" in l or "no take can have moved" in l for l in verdict)
    stages, missing = _read_stages(files, root, cc, rows, unchanged)
    return {"verdict": verdict, "base": text("base.txt").strip() or None, "stages": stages, "missing": missing, "unmeasured": False}


def _country_reasons(cc: str, c: Country) -> List[str]:
    """Why one country's stages are not landable: what is missing, and each blocking bucket a stage holds."""
    reasons = [f"{cc}: {c['missing']}"] if c["missing"] else []
    return reasons + [f"{cc} {stage}: {len(buckets[b])} {b}" for stage, buckets in c["stages"].items()
                      for b in BLOCKING if buckets[b]]


def landable(countries):
    """(landable?, [reason]) over every country's stages."""
    reasons = []
    if countries and all(c["unmeasured"] for c in countries.values()):
        reasons.append(f"no country was measured (no takes row of {', '.join(countries)} ran)")
    for cc, c in countries.items():
        reasons += _country_reasons(cc, c)
    return not reasons, reasons


def counts(buckets):
    moves = [m for ms in buckets.values() for m in ms]
    return {k: sum(1 for m in moves if m["kind"] == k) for k in ("lost", "changed", "gained")}


def moved(countries):
    """The countries any of whose takes moved, in either stage: what a cross-country change is judged by."""
    return [cc for cc, c in countries.items() if any(sum(counts(b).values()) for b in c["stages"].values())]


def move_line(m):
    target = f" [target {m['target']}]" if "target" in m else ""
    return f"{m['venue']} | {m['rawTitle']}: {m['before']} -> {m['after']}{target}"


SHORT = {"cure": "cure", "right-lost": "R-lost", "right->wrong": "R>W", "wrong->wrong": "W>W", "new-wrong": "new-W",
         "gained-right": "gain-R", "gained-unjudged": "gain-?", "to-right": ">R", "right->right": "R>R",
         "accepted": "acc", "unjudged": "unjdg"}


def _stage_row(cc: str, stage: str, buckets: Buckets, row_state: str, first: str) -> List[str]:
    n = counts(buckets)
    note = f": {first}" if stage == "model" and first else ""
    return [cc, stage, str(n["lost"]), str(n["changed"]), str(n["gained"])] + [str(len(buckets[b])) for b in BUCKETS] + [f"{row_state}{note}"]


def _country_rows(cc: str, c: Country, row_state: str) -> List[List[str]]:
    """The table rows of one country: one a stage, or one saying why it has none."""
    dashes = ["-", "-", "-", "-"] + ["-"] * len(BUCKETS)
    if c["unmeasured"]:
        return [[cc] + dashes + [UNMEASURED]]
    if c["missing"] and not c["stages"]:
        return [[cc] + dashes + [f"{row_state}: {c['missing']}"]]
    # the takes' own line ("identity takes (cc): ... lost N, changed N ...") when the check wrote one
    first = next((l for l in c["verdict"] if l.startswith("identity takes")), (c["verdict"] or [""])[0])[:70]
    return [_stage_row(cc, stage, buckets, row_state, first) for stage, buckets in c["stages"].items()]


def _example_lines(countries: Dict[str, Country], examples: int) -> List[str]:
    """The first `examples` moves of every bucket worth reading (not the unjudged, which are all listed after)."""
    out = []
    shown = [(cc, stage, b, buckets[b]) for cc, c in countries.items() for stage, buckets in c["stages"].items()
             for b in BUCKETS if buckets[b] and b != "unjudged"]
    for cc, stage, b, ms in shown:
        out.append(f"\n{cc} {stage} {b} ({len(ms)}):")
        out += [f"  {move_line(m)}" for m in ms[:examples]]
        if len(ms) > examples:
            out.append(f"  ... {len(ms) - examples} more (--json lists them all)")
    return out


def _unjudged_lines(countries: Dict[str, Country]) -> List[str]:
    unjudged = [(cc, stage, m) for cc, c in countries.items() for stage, buckets in c["stages"].items()
                for m in buckets["unjudged"] + buckets["gained-unjudged"]]
    if not unjudged:
        return []
    return ([f"\nEVERY UNJUDGED MOVE ({len(unjudged)}) -- judge each in test/resources/fixtures/identity-judgements.tsv "
             "(scripts/label-lookup, scripts/identity-judge-cards) before landing:"]
            + [f"  {cc} {stage} {m['kind']:7} {move_line(m)}" for cc, stage, m in unjudged])


def render(countries, rows, examples=3):
    head = ["cc", "stage", "lost", "chgd", "gain"] + [SHORT[b] for b in BUCKETS] + ["takes row"]
    table = [head]
    for cc, c in countries.items():
        table += _country_rows(cc, c, rows.get(cc, "?"))
    widths = [max(len(r[i]) for r in table) for i in range(len(head) - 1)]
    out = ["  ".join(r[i].ljust(widths[i]) for i in range(len(widths))) + "  " + r[-1] for r in table]
    out.append("")
    out.append("buckets: " + ", ".join(f"{SHORT[b]}={b}" for b in BUCKETS))
    return "\n".join(out + _example_lines(countries, examples) + _unjudged_lines(countries))


DOCSTRING = __doc__ or ""  # None under python -OO


def _arguments(argv: Optional[Sequence[str]]) -> argparse.Namespace:
    p = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
    p.add_argument("dir")
    p.add_argument("--judgements", required=True)
    p.add_argument("--countries", default=",".join(COUNTRIES))
    p.add_argument("--rows", help="a JSON file: {cc: the takes row's conclusion}")
    p.add_argument("--run-url", default="")
    p.add_argument("--json", action="store_true")
    p.add_argument("--examples", type=int, default=3)
    p.add_argument("--moved-out", help="write the countries whose takes moved (comma-separated) to this file")
    p.add_argument("--kind", choices=KINDS, default="full", help="the kind of measure the takes come from (THE KIND)")
    return p.parse_args(argv)


def _print_text(a: argparse.Namespace, ok: bool, reasons: List[str], countries: Dict[str, Country],
                conclusions: Dict[str, str]) -> None:
    print(KIND_LINE[a.kind])
    if a.run_url:
        print(f"run {a.run_url}")
    print()
    print(render(countries, conclusions, a.examples))
    landable_line = "LANDABLE: no right take lost or turned wrong, no new wrong take"
    if a.kind == "footprint":
        landable_line = "LANDABLE (footprint): no take can move -- before landing, the full measure: scripts/identity-measure without --footprint"
    print("\n" + (landable_line if ok else "NOT LANDABLE:\n" + "\n".join(f"  - {r}" for r in reasons)))


def main(argv=None):
    a = _arguments(argv)
    try:
        files = Files.here()
        rows = read(files, a.judgements)
        conclusions = json.loads(files.read_text(a.rows)) if a.rows else {}
        countries = {cc: read_country(files, a.dir, cc, rows, conclusions) for cc in a.countries.split(",") if cc}
    except (OSError, ValueError, KeyError, TypeError, IndexError) as e:
        print(f"identity-measure: could not read the inputs: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    ok, reasons = landable(countries)
    if a.moved_out:
        files.write_text(a.moved_out, ",".join(moved(countries)) + "\n")
    if a.json:
        print(json.dumps({"measure": a.kind, "run": a.run_url, "landable": ok, "reasons": reasons,
                          "countries": {cc: dict(c, row=conclusions.get(cc)) for cc, c in countries.items()}},
                         ensure_ascii=False, indent=1))
    else:
        _print_text(a, ok, reasons, countries, conclusions)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
