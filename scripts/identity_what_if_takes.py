"""WHAT A CHANGE'S TAKES DID (the DIFF half of scripts/identity_what_if.py, whose header says how it reads): each listing's base
and head take bucketed by the judgements exactly as scripts/identity_measure.py buckets a CI measurement, plus prod's served
take beside each, the report printed and the moved listings written as a TSV. Imported by identity_what_if.py, which has put
scripts/, scripts/ci and scripts/lib on sys.path."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import sys
from typing import Any, Dict, List, Optional, Sequence, Set, Tuple

import header_tsv  # noqa: E402
import identity_measure as measure  # noqa: E402
from identity_judgements import Row, read as read_judgements  # noqa: E402
from seams.tool_files import Files  # noqa: E402

# a listing's take row (key, venue, rawTitle, take, family, rules ...), a side's rows by key, a diff's report
Take = Dict[str, str]
Takes = Dict[str, Take]
Report = Dict[str, Any]


def film_of(take: str) -> Optional[int]:
    return int(take[5:]) if take.startswith("tmdb:") else None


def as_row(r: Take, take: str) -> Dict[str, Any]:
    venue, title = r["venue"], r["rawTitle"]
    return {"key": r["key"], "venue": venue, "rawTitle": title, "film": film_of(take), "filmTitle": None,
            "basis": take, "explanation": [r.get("rules", "").replace("\x1f", " ")[:200]], "blocker": None}


def merged_head(base_rows: List[Take], head_rows: List[Take]) -> Tuple[List[Take], Set[str]]:
    """Each listing's head take: its re-resolve's where the head re-resolved it, the base's elsewhere."""
    head = {r["key"]: r for r in head_rows}
    return [head.get(r["key"], r) for r in base_rows], set(head)


def _awaiting(base_by: Takes, head_by: Takes) -> Set[str]:
    """The listings whose head take is Pending where the base had one: a question the head asks that prod's store never held
    (new query text, a new shape's search). The what-if refuses every live ask, a real resolve would ask and answer it: not a
    lost take -- the listing reads as the base's, counted apart, so the verdict is of the takes the head can decide."""
    return {k for k, h in head_by.items() if h["take"] == "pending" and base_by[k]["take"] != "pending"}


def _prod_agreement(base_by: Takes, head_by: Takes, prod: Dict[str, str]) -> Dict[str, int]:
    compared = [k for k in base_by if prod.get(k, "missing") != "missing"]
    return {"compared": len(compared), "base agrees": sum(base_by[k]["take"] == prod[k] for k in compared),
            "head agrees": sum(head_by[k]["take"] == prod[k] for k in compared),
            "toward prod": sum(base_by[k]["take"] != prod[k] and head_by[k]["take"] == prod[k] for k in compared),
            "away from prod": sum(base_by[k]["take"] == prod[k] and head_by[k]["take"] != prod[k] for k in compared)}


def _moved_rows(cc: str, moved_keys: Set[str], base_by: Takes, head_by: Takes, prod: Dict[str, str],
                bucket_of: Dict[Tuple[str, str], str]) -> List[Take]:
    rows = []
    for k in sorted(moved_keys, key=lambda k: (base_by[k]["venue"], base_by[k]["rawTitle"])):
        b, h = base_by[k], head_by[k]
        pending = "pending" if "pending" in (b["take"], h["take"]) else "unpaired"
        rows.append({"country": cc, "venue": b["venue"], "rawTitle": b["rawTitle"], "base": b["take"], "head": h["take"],
                     "prod": prod.get(k, "missing"), "bucket": bucket_of.get((b["venue"], b["rawTitle"]), pending),
                     "family": b["family"], "headRules": h.get("rules", "").replace("\x1f", " ")})
    return rows


def _unsound(base_by: Takes, resolved_by: Takes, whole_rows: Optional[List[Take]], reresolved: Set[str]) -> List[Dict[str, Any]]:
    """The listings whose what-if take differs from the whole head resolve's (none when no whole resolve was read)."""
    if whole_rows is None:
        return []
    whole = {r["key"]: r["take"] for r in whole_rows}
    return [{"venue": base_by[k]["venue"], "rawTitle": base_by[k]["rawTitle"], "what-if": resolved_by[k]["take"],
             "whole": whole.get(k, "absent"), "selected": k in reresolved}
            for k in sorted(base_by)
            if whole.get(k, "absent") != resolved_by[k]["take"] and (k in whole or k in reresolved)]


def _examples(buckets: Dict[str, List[Dict[str, Any]]]) -> Dict[str, List[str]]:
    return {b: [f"{e['venue']} | {e['rawTitle']}: {e['before']} -> {e['after']}" for e in v[:3]] for b, v in buckets.items() if v}


def _pending_moves(moved_keys: Set[str], base_by: Takes, head_by: Takes) -> int:
    return sum(1 for k in moved_keys if "pending" in (base_by[k]["take"], head_by[k]["take"]))


def _by_key(rows: List[Take]) -> Takes:
    return {r["key"]: r for r in rows}


def _model_rows(rows: List[Take]) -> List[Dict[str, Any]]:
    return [as_row(r, r["take"]) for r in rows]


def _bucket_counts(buckets: Dict[str, List[Dict[str, Any]]]) -> Dict[str, int]:
    return {b: len(v) for b, v in buckets.items() if v}


def _bucket_index(buckets: Dict[str, List[Dict[str, Any]]]) -> Dict[Tuple[str, str], str]:
    return {(e["venue"], e["rawTitle"]): bucket for bucket, entries in buckets.items() for e in entries}


def _moved_keys(base_rows: List[Take], head_by: Takes) -> Set[str]:
    return {r["key"] for r in base_rows if r["take"] != head_by[r["key"]]["take"]}


def diff(cc: str, base_rows: List[Take], head_rows: List[Take], prod: Dict[str, str], judgement_rows: List[Row],
         whole_rows: Optional[List[Take]] = None) -> Tuple[Report, List[Take]]:
    head_taken, reresolved = merged_head(base_rows, head_rows)
    base_by, resolved_by = _by_key(base_rows), _by_key(head_taken)
    awaiting = _awaiting(base_by, resolved_by)
    head_taken = [base_by[r["key"]] if r["key"] in awaiting else r for r in head_taken]
    head_by = _by_key(head_taken)
    buckets = measure.classify(cc, _model_rows(base_rows), _model_rows(head_taken), judgement_rows)
    moved_keys = _moved_keys(base_rows, head_by)
    report = {"country": cc, "listings": len(base_rows), "reresolved": len(reresolved & set(base_by)),
              "moved": len(moved_keys), "awaiting": len(awaiting), "buckets": _bucket_counts(buckets),
              "pending": _pending_moves(moved_keys, base_by, head_by), "prod": _prod_agreement(base_by, head_by, prod),
              "unsound": _unsound(base_by, resolved_by, whole_rows, reresolved), "examples": _examples(buckets)}
    return report, _moved_rows(cc, moved_keys, base_by, head_by, prod, _bucket_index(buckets))


def read_takes(path: str) -> List[Take]:
    """A side's rows as the take files write them (key, venue, rawTitle, take, family, rules ...)."""
    return header_tsv.read(path)


TSV_COLUMNS = ("country", "venue", "rawTitle", "base", "head", "prod", "bucket", "family", "headRules")


def render(report: Report, stage: str = "model") -> str:
    out = [f"== {report['country']} {stage}: {report['listings']} listings, {report['reresolved']} re-resolved at the head; "
           f"{report['moved']} take(s) moved ({report['pending']} to or from pending)"]
    if report["awaiting"]:
        out.append(f"  {report['awaiting']} listing(s) await an answer the head asks and the answer store does not hold "
                   "(--offline, or a live ask that failed): counted as no move; a CI measure fills them")
    if report["buckets"]:
        out.append("  " + ", ".join(f"{b} {n}" for b, n in report["buckets"].items()))
    for b, examples in report["examples"].items():
        if b in measure.BLOCKING or b in ("cure", "gained-right"):
            out += [f"    {b}: {e}" for e in examples]
    p = report["prod"]
    out.append(f"  vs prod's served take ({p['compared']} listings prod holds): base agrees {p['base agrees']}, head "
               f"{p['head agrees']}; {p['toward prod']} moved toward prod, {p['away from prod']} away")
    blocking = sum(report["buckets"].get(b, 0) for b in measure.BLOCKING)
    out.append(f"  {'CLEAN' if not blocking else 'BLOCKING'}: right-lost {report['buckets'].get('right-lost', 0)}, "
               f"right->wrong {report['buckets'].get('right->wrong', 0)}, new-wrong {report['buckets'].get('new-wrong', 0)}")
    return "\n".join(out)


def final_rows(base_rows: List[Take], path: str) -> Optional[List[Take]]:
    """`final.tsv`'s rows as take rows (the final take in `take`), each with its listing's venue, title and family from
    the base's model rows; None when the side wrote none."""
    if not os.path.exists(path):
        return None
    by = {r["key"]: r for r in base_rows}
    return [dict(by[r["key"]], take=r["final"], rules=r.get("basis", "")) for r in header_tsv.read(path) if r["key"] in by]


def write_rows(path: str, rows: Sequence[Take]) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write("\t".join(TSV_COLUMNS) + "\n")
        f.writelines("\t".join(str(r[c]) for c in TSV_COLUMNS) + "\n" for r in rows)


def verified(report: Report, stage: str) -> bool:
    """Prints the --verify verdict of one stage's report; False when a take differs from the whole head resolve."""
    if report["unsound"]:
        print(f"  VERIFY FAILED ({stage}): {len(report['unsound'])} listing(s) take otherwise in the whole head resolve:")
        for u in report["unsound"][:20]:
            print(f"    {u['venue']} | {u['rawTitle']}: what-if {u['what-if']}, whole {u['whole']}"
                  f"{' (selected)' if u['selected'] else ' (OUTSIDE the selection)'}")
        return False
    print(f"  VERIFY OK ({stage}): the whole head resolve takes as the what-if on every listing")
    return True


def _whole_rows(head: str) -> Optional[List[Take]]:
    path = os.path.join(head, "takes-whole.tsv")
    return header_tsv.read(path) if os.path.exists(path) else None


def _final_diff(args: argparse.Namespace, base_rows: List[Take], prod: Dict[str, str],
                judgements: List[Row]) -> Tuple[Optional[Report], List[Take]]:
    """(the final take's report, its moved listings), or (None, []) when a side wrote no final.tsv (--model-only)."""
    base_final = final_rows(base_rows, os.path.join(args.base, "final.tsv"))
    head_final = final_rows(base_rows, os.path.join(args.head, "final.tsv"))
    if base_final is None or head_final is None:
        return None, []
    whole_final = final_rows(base_rows, os.path.join(args.head, "final-whole.tsv")) if args.verify else None
    return diff(args.cc, base_final, head_final, prod, judgements, whole_final)


def _print_reports(args: argparse.Namespace, report: Report, final_report: Optional[Report]) -> None:
    if args.json:
        print(json.dumps(report))
        return
    print(render(report, "model"))
    print(render(final_report, "final") if final_report
          else f"== {args.cc} final: not settled (--model-only, or a side without final.tsv)")


def _verify_exit(report: Report, final_report: Optional[Report]) -> int:
    """0 when every stage's take is the whole head resolve's, else 1; `final_report` None where it was not resolved whole."""
    ok = verified(report, "model")
    if final_report:
        ok = verified(final_report, "final") and ok
    return 0 if ok else 1


def _write_moves(tsv: Optional[str], rows: List[Take], final_moves: Optional[List[Take]]) -> None:
    """The moved listings of the model's take (and the final take's beside it) as TSVs, when --tsv names one."""
    if tsv and final_moves is not None:
        write_rows(tsv.replace("takes-diff-", "final-diff-"), final_moves)
    if tsv:
        write_rows(tsv, rows)


def run_diff(args: argparse.Namespace, files: Files) -> int:
    base_rows = read_takes(os.path.join(args.base, "takes.tsv"))
    head_rows = read_takes(os.path.join(args.head, "takes.tsv"))
    prod = {r["key"]: r["prod"] for r in read_takes(os.path.join(args.base, "prod.tsv"))}
    whole = _whole_rows(args.head) if args.verify else None
    if args.verify and whole is None:
        print("--verify: the head wrote no takes-whole.tsv", file=sys.stderr)
        return 2
    judgements = read_judgements(files, args.judgements)
    report, rows = diff(args.cc, base_rows, head_rows, prod, judgements, whole)
    final_report, final_moves = _final_diff(args, base_rows, prod, judgements)
    if final_report:
        report["final"] = final_report
    _write_moves(args.tsv, rows, final_moves if final_report else None)
    _print_reports(args, report, final_report)
    if not args.verify:
        return 0
    final_whole = final_report and os.path.exists(os.path.join(args.head, "final-whole.tsv"))
    return _verify_exit(report, final_report if final_whole else None)


def _digest(pairs: Any) -> str:
    return hashlib.sha256("\n".join(sorted(pairs)).encode()).hexdigest()[:12]


def _listing_take(rows: List[Take], finals: Dict[str, str], listing: str) -> str:
    """The take of the listing (`[cc|]venue|raw title`) under every key it bills under, `absent` when none."""
    parts = listing.split("|")
    venue, title = (parts[-2], parts[-1]) if len(parts) >= 2 else (None, parts[0])
    hits = sorted((r for r in rows if r["rawTitle"] == title and (venue is None or r["venue"] == venue)), key=lambda r: r["key"])
    return "; ".join(f"model {r['take']}, final {finals.get(r['key'], '-')}" for r in hits) or "absent"


def side_take(side: str, listing: Optional[str] = None) -> str:
    """One whole side's take, as --bisect compares sides: the listing's (`[cc|]venue|raw title`) model and final take --
    every key it bills under -- or, with none, a digest of every take."""
    rows = header_tsv.read(os.path.join(side, "takes.tsv"))
    final_path = os.path.join(side, "final.tsv")
    finals = {r["key"]: r["final"] for r in header_tsv.read(final_path)} if os.path.exists(final_path) else {}
    if listing is not None:
        return _listing_take(rows, finals, listing)
    final = _digest(k + chr(9) + v for k, v in finals.items()) if finals else "-"
    return f"model {_digest(r['key'] + chr(9) + r['take'] for r in rows)}, final {final} ({len(rows)} takes)"
