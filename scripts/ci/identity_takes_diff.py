#!/usr/bin/env python3
"""THE IDENTITY TAKES CHECK: which listings a code change moved to another film, on one recording.

Both inputs are `decisions-<cc>.jsonl` files `IdentityResolveDumpIntegrationSpec` writes (one line a
listing: key, venue, rawTitle, film, filmTitle, basis, explanation, blocker), resolved from the SAME
pinned recording -- one by the base commit, one by the head. The recording is identical, so every
listing whose film differs was moved by the code between them, never by new data: a nightly
recording changes both sides at once and moves nothing here.

Per listing (matched by its serialised key, else by venue and raw title):
  lost     the base took a TMDB film, the head takes none
  changed  the head takes another TMDB film
  gained   the base took none, the head takes one

LOST and CHANGED fail the check unless accepted, either way:
  - labels.tsv (test/resources/fixtures/identity-unmatched; a `*` venue labels every venue) says the head's film is `right` for the
    listing, or the base's film is `wrong` (and the head's is not labelled wrong);
  - accepted.tsv (test/resources/fixtures/identity-takes) lists the move:
    country, venue (or *), rawTitle, before, after (tmdb:<id> or none, or *), note.
GAINED is reported, never failed -- unless labels.tsv says the gained film is `wrong`.

THE FINAL TAKES. The decisions above are the MODEL's: the agreement stage (fills, corrections, the families' agreed
takes, billedBeside on them) reads them again before anything is served, and a change there moved nothing in them --
9c8170f5e's range dropped 213 UK takes in prod (211 "CBeebies Panto 2026: Treasure Island") with this check at 0/0/0.
With --final the inputs are instead the unmatched-cluster ratchet's `ratchet-takes.tsv` (UnmatchedClustersRatchetSpec:
the model and the agreement stage over the checked-in capture and its families' answers), written by the base and the
head over the SAME capture: a listing absent from a side took no film there. The same verdicts apply.

usage: identity_takes_diff.py <cc> <base.jsonl> <head.jsonl> [--labels F] [--accepted F] [--summary F]
         [--base-name S] [--head-name S] [--final]
Exit 1 when an unaccepted move is found, 2 on unreadable input.
"""

import argparse
import json
import sys


def load(path):
    with open(path, encoding="utf-8") as f:
        return [json.loads(line) for line in f if line.strip()]


def take(row):
    film = row.get("film")
    return None if film is None else int(film)


def ref(film):
    return "none" if film is None else f"tmdb:{film}"


def labels_of(path, cc):
    out = {}
    if not path:
        return out
    with open(path, encoding="utf-8") as f:
        for n, line in enumerate(f):
            cols = line.rstrip("\n").split("\t")
            if n == 0 or len(cols) < 5 or cols[0] != cc:
                continue
            out[(cols[1], cols[2], cols[3])] = cols[4]
    return out


def label_of(labels, venue, title, film):
    """The verdict labels.tsv gives `film` for the listing, read as UnmatchedClusters.verdict reads it: a label for every
    venue ("*") covers this one too, and the venue's own label calling the film right overrules every venue's calling it
    wrong (PL Kino Iluzjon's "Lalka" is Has' 1968 film, the bare "Lalka" everywhere else the 2026 one)."""
    own, every = labels.get((venue, title, film)), labels.get(("*", title, film))
    if own == "right" or every is None:
        return own
    return "wrong" if "wrong" in (own, every) else every


def accepted_of(path, cc):
    out = []
    if not path:
        return out
    try:
        f = open(path, encoding="utf-8")
    except FileNotFoundError:
        return out
    with f:
        for line in f:
            if not line.strip() or line.startswith("#"):
                continue
            cols = line.rstrip("\n").split("\t")
            if cols[0] == "country" or len(cols) < 5 or cols[0] != cc:
                continue
            out.append(tuple(cols[1:5]))
    return out


def ratchet_rows(path, cc):
    """{(venue, rawTitle): row} of the takes a ratchet-takes.tsv holds for `cc` -- every status but LOST (an expected take
    now taking none) carries the film the listing takes; a TMDB id only, as the model's decisions carry."""
    out = {}
    with open(path, encoding="utf-8") as f:
        header = f.readline().rstrip("\n").split("\t")
        for line in f:
            cols = dict(zip(header, line.rstrip("\n").split("\t")))
            if cols.get("country") != cc or cols.get("status") == "LOST" or not cols.get("film", "").startswith("tmdb:"):
                continue
            out[(cols["venue"], cols["rawTitle"])] = {
                "key": f"{cols['venue']}|{cols['rawTitle']}", "venue": cols["venue"], "rawTitle": cols["rawTitle"],
                "film": int(cols["film"][5:]), "filmTitle": cols.get("title"), "basis": cols.get("basis"),
                "explanation": [cols.get("explanation", "")], "blocker": None}
    return out


def final_sides(base_path, head_path, cc):
    """The base's and the head's final takes, each listing on both sides: absent from one took no film there."""
    base, head = ratchet_rows(base_path, cc), ratchet_rows(head_path, cc)
    def side(rows, k):
        return rows.get(k) or {"key": f"{k[0]}|{k[1]}", "venue": k[0], "rawTitle": k[1], "film": None, "filmTitle": None,
                               "basis": "none", "explanation": ["takes no film"], "blocker": "no film"}
    keys = sorted(set(base) | set(head))
    return [side(base, k) for k in keys], [side(head, k) for k in keys]


def pair(base, head):
    """(base row, head row) per listing, by key, then by venue + raw title for keys the change re-spelled -- only where
    that title is billed ONCE at the venue on each side: two same-titled listings (Belle 2013 and Belle 2021) are never
    paired by guess, which could read as a swap or hide a move; they count as on one side only."""
    head_by_key = {r["key"]: r for r in head}
    pairs, base_left = [], []
    for b in base:
        h = head_by_key.pop(b["key"], None)
        if h is None:
            base_left.append(b)
        else:
            pairs.append((b, h))
    head_by_title, base_by_title = {}, {}
    for h in head_by_key.values():
        head_by_title.setdefault((h["venue"], h["rawTitle"]), []).append(h)
    for b in base_left:
        base_by_title.setdefault((b["venue"], b["rawTitle"]), []).append(b)
    unpaired = 0
    for title, bs in base_by_title.items():
        hs = head_by_title.pop(title, [])
        if len(bs) == 1 and len(hs) == 1:
            pairs.append((bs[0], hs[0]))
        else:
            unpaired += len(bs) + len(hs)
    unpaired += sum(len(v) for v in head_by_title.values())
    return pairs, unpaired


def kind_of(before, after):
    if before == after:
        return None
    if after is None:
        return "lost"
    if before is None:
        return "gained"
    return "changed"


def verdict(cc, move, labels, accepted):
    """(accepted?, why) for one move."""
    venue, title, before, after = move["venue"], move["rawTitle"], ref(move["before"]), ref(move["after"])
    for a_venue, a_title, a_before, a_after in accepted:
        if a_venue in ("*", venue) and a_title == title and a_before in ("*", before) and a_after in ("*", after):
            return True, "accepted.tsv"
    now = label_of(labels, venue, title, after)
    was = label_of(labels, venue, title, before)
    if now == "wrong":
        return False, f"labels.tsv: {after} is WRONG"
    if move["kind"] == "gained":
        return True, "gained" + (" (labelled right)" if now == "right" else "")
    if now == "right":
        return True, f"labels.tsv: {after} is right"
    if was == "wrong":
        return True, f"labels.tsv: {before} was wrong"
    return False, "not accepted"


def why(row):
    parts = [row.get("basis") or "?"] + list(row.get("explanation") or [])[:2]
    if row.get("film") is None and row.get("blocker"):
        parts.append(f"blocker {row['blocker']}")
    return ": ".join(str(p) for p in parts)


def film_name(row):
    return ref(take(row)) + (f" {row['filmTitle']}" if row.get("filmTitle") else "")


def moves_of(cc, base, head, labels, accepted):
    pairs, unpaired = pair(base, head)
    moves = []
    for b, h in pairs:
        kind = kind_of(take(b), take(h))
        if kind is None:
            continue
        move = {"kind": kind, "venue": b["venue"], "rawTitle": b["rawTitle"], "before": take(b), "after": take(h),
                "old": film_name(b), "new": film_name(h), "oldWhy": why(b), "newWhy": why(h)}
        move["accepted"], move["verdict"] = verdict(cc, move, labels, accepted)
        moves.append(move)
    moves.sort(key=lambda m: (m["accepted"], m["kind"], m["rawTitle"], m["venue"]))
    return moves, unpaired


def cell(text):
    return str(text).replace("|", "\\|").replace("\n", " ")


def render(cc, moves, unpaired, base_name, head_name, listings, what="identity takes", where="one recording"):
    failing = [m for m in moves if not m["accepted"]]
    counts = {k: sum(1 for m in moves if m["kind"] == k) for k in ("lost", "changed", "gained")}
    head_line = (f"{what} ({cc}): {listings} listings, base {base_name} vs head {head_name} on {where} -- "
                 f"lost {counts['lost']}, changed {counts['changed']}, gained {counts['gained']}; "
                 f"{len(failing)} not accepted" + (f"; {unpaired} listing(s) on one side only" if unpaired else ""))
    text = [head_line] + [
        f"  {'FAIL' if not m['accepted'] else 'ok  '} {m['kind']:7} {m['venue']} | {m['rawTitle']}: {m['old']} -> {m['new']} "
        f"[{m['verdict']}]\n        base: {m['oldWhy']}\n        head: {m['newWhy']}" for m in moves]
    md = [f"### {what[0].upper() + what[1:]} ({cc})", "", head_line, ""]
    if moves:
        md += ["| | kind | venue | listing | base film | head film | base reason | head reason | verdict |",
               "|---|---|---|---|---|---|---|---|---|"]
        md += [f"| {'❌' if not m['accepted'] else '✓'} | {m['kind']} | {cell(m['venue'])} | {cell(m['rawTitle'])} | "
               f"{cell(m['old'])} | {cell(m['new'])} | {cell(m['oldWhy'])} | {cell(m['newWhy'])} | {cell(m['verdict'])} |"
               for m in moves[:300]]
        if len(moves) > 300:
            md.append(f"\n... and {len(moves) - 300} more (the job log has them all)")
    if failing:
        md += ["", "To accept a move, label the head's film `right` in `test/resources/fixtures/identity-unmatched/labels.tsv`, "
                   "or add `country\tvenue|*\trawTitle\tbefore\tafter|*\tnote` to `test/resources/fixtures/identity-takes/accepted.tsv`."]
    return "\n".join(text), "\n".join(md) + "\n", failing


def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("cc")
    p.add_argument("base")
    p.add_argument("head")
    p.add_argument("--labels")
    p.add_argument("--accepted")
    p.add_argument("--summary")
    p.add_argument("--base-name", default="base")
    p.add_argument("--head-name", default="head")
    p.add_argument("--final", action="store_true", help="the inputs are ratchet-takes.tsv files: the final takes")
    a = p.parse_args(argv)
    # Exit 2 for anything that is not a verdict -- an unreadable file, a row without its fields -- so it can never read as
    # exit 1's "the code moved a take".
    try:
        base, head = final_sides(a.base, a.head, a.cc) if a.final else (load(a.base), load(a.head))
        if not base or not head:
            print(f"identity takes ({a.cc}): an empty side (base {len(base)}, head {len(head)} listings) -- nothing resolved",
                  file=sys.stderr)
            return 2
        moves, unpaired = moves_of(a.cc, base, head, labels_of(a.labels, a.cc), accepted_of(a.accepted, a.cc))
    except (OSError, ValueError, KeyError, TypeError, IndexError) as e:
        print(f"identity takes ({a.cc}): could not read the inputs: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    text, md, failing = render(a.cc, moves, unpaired, a.base_name, a.head_name, len(head),
                               *(("identity final takes", "one capture") if a.final else ()))
    print(text)
    if a.summary:
        with open(a.summary, "a", encoding="utf-8") as f:
            f.write(md)
    return 1 if failing else 0


if __name__ == "__main__":
    sys.exit(main())
