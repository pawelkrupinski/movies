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
  - labels.tsv (test/resources/fixtures/identity-unmatched) says the head's film is `right` for the
    listing, or the base's film is `wrong` (and the head's is not labelled wrong);
  - accepted.tsv (test/resources/fixtures/identity-takes) lists the move:
    country, venue (or *), rawTitle, before, after (tmdb:<id> or none, or *), note.
GAINED is reported, never failed -- unless labels.tsv says the gained film is `wrong`.

usage: identity_takes_diff.py <cc> <base.jsonl> <head.jsonl> [--labels F] [--accepted F] [--summary F]
         [--base-name S] [--head-name S]
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
    now = labels.get((venue, title, after))
    was = labels.get((venue, title, before))
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


def render(cc, moves, unpaired, base_name, head_name, listings):
    failing = [m for m in moves if not m["accepted"]]
    counts = {k: sum(1 for m in moves if m["kind"] == k) for k in ("lost", "changed", "gained")}
    head_line = (f"identity takes ({cc}): {listings} listings, base {base_name} vs head {head_name} on one recording -- "
                 f"lost {counts['lost']}, changed {counts['changed']}, gained {counts['gained']}; "
                 f"{len(failing)} not accepted" + (f"; {unpaired} listing(s) on one side only" if unpaired else ""))
    text = [head_line] + [
        f"  {'FAIL' if not m['accepted'] else 'ok  '} {m['kind']:7} {m['venue']} | {m['rawTitle']}: {m['old']} -> {m['new']} "
        f"[{m['verdict']}]\n        base: {m['oldWhy']}\n        head: {m['newWhy']}" for m in moves]
    md = [f"### Identity takes ({cc})", "", head_line, ""]
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
    a = p.parse_args(argv)
    # Exit 2 for anything that is not a verdict -- an unreadable file, a row without its fields -- so it can never read as
    # exit 1's "the code moved a take".
    try:
        base, head = load(a.base), load(a.head)
        if not base or not head:
            print(f"identity takes ({a.cc}): an empty side (base {len(base)}, head {len(head)} listings) -- nothing resolved",
                  file=sys.stderr)
            return 2
        moves, unpaired = moves_of(a.cc, base, head, labels_of(a.labels, a.cc), accepted_of(a.accepted, a.cc))
    except (OSError, ValueError, KeyError, TypeError, IndexError) as e:
        print(f"identity takes ({a.cc}): could not read the inputs: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    text, md, failing = render(a.cc, moves, unpaired, a.base_name, a.head_name, len(head))
    print(text)
    if a.summary:
        with open(a.summary, "a", encoding="utf-8") as f:
            f.write(md)
    return 1 if failing else 0


if __name__ == "__main__":
    sys.exit(main())
