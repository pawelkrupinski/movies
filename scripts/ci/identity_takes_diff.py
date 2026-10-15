#!/usr/bin/env python3
"""THE IDENTITY TAKES CHECK: which listings a code change moved to another film, on one recording.

Both inputs are `decisions-<cc>.jsonl` files `IdentityResolveDumpIntegrationSpec` writes (one line a
listing: key, venue, rawTitle, film, filmTitle, basis, explanation, blocker, suspect), resolved from the SAME
pinned recording -- one by the base commit, one by the head. The recording is identical, so every
listing whose film differs was moved by the code between them, never by new data: a nightly
recording changes both sides at once and moves nothing here.

Per listing (matched by its serialised key, else by venue and raw title):
  lost     the base took a TMDB film, the head takes none
  changed  the head takes another TMDB film
  gained   the base took none, the head takes one

LOST and CHANGED fail the check unless accepted by the identity judgements (test/resources/fixtures/
identity-judgements.tsv, read by identity_judgements.py; a `*` row judges every venue, a venue's own row overrides it):
  - they call the head's film `right` for the listing, or the base's film `wrong` (and the head's not wrong);
  - a LOST take's row is in state accepted-loss (or accepted-right-loss): the loss is a trade accepted with why.
GAINED passes when the judgements call the gained film `right`, fails when it calls it `wrong`, and otherwise NEEDS JUDGEMENT:
  - a gained take a suspect-take signal flags (the head row's `suspect`: SuspectTakes' fragment, era, runtime, sequel,
    feed, route) fails until judged -- 648580662 gained DE "Punkabend #93" -> '93 (2025) and "Tino Bomelino" -> Tino
    (2022) as plain gains, read by no one, in run 38578038277;
  - an unflagged one passes, marked as needing judgement (WARN), never as a silent ok.

SUSPECT TAKES, scoped to what the code changed. Every head row a suspect-take signal flags (its `suspect`) whose film
no judgement judges (read as above) and the judgements do not tolerate in scope `served` is
classified against the base's row for the same listing:
  standing  the base took the same film and flagged it too: data the code did not touch -- REPORTED, never failed
            (the whole corpus's standing suspects are the Identity findings report's, identity-findings-report.yml);
  new       the head took another film, or flags a take the base did not -- the code made it: FAILS, unless
            an accepted loss accepts the listing's move (then counted as accepted);
  cured     the base's take was an unjudged suspect and the head's is not -- counted, never failed.
A nightly recording changes base and head at once, so a suspect it brings is standing on both: it never fails here.

THE FINAL TAKES. The decisions above are the MODEL's: the agreement stage (fills, corrections, the families' agreed
takes, billedBeside on them) reads them again before anything is served, and a change there moved nothing in them --
9c8170f5e's range dropped 213 UK takes in prod (211 "CBeebies Panto 2026: Treasure Island") with this check at 0/0/0.
With --final the inputs are instead the unmatched-cluster ratchet's `ratchet-takes.tsv` (UnmatchedClustersRatchetSpec:
the model and the agreement stage over the checked-in capture and its families' answers), written by the base and the
head over the SAME capture: a listing absent from a side took no film there. The same verdicts apply.

usage: identity_takes_diff.py <cc> <base.jsonl> <head.jsonl> [--judgements F] [--summary F]
         [--base-name S] [--head-name S] [--final]
Exit 1 when an unaccepted move or a new suspect take is found, 2 on unreadable input.
"""

import argparse
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_judgements import judgements_of, label_of, normalise  # noqa: E402


def load(path):
    with open(path, encoding="utf-8") as f:
        return [json.loads(line) for line in f if line.strip()]


def take(row):
    film = row.get("film")
    return None if film is None else int(film)


def ref(film):
    return "none" if film is None else f"tmdb:{film}"


def ratchet_ran(path):
    """Whether a ratchet-takes.tsv holds any take row, for any country: an empty file is a ratchet that never ran."""
    with open(path, encoding="utf-8") as f:
        f.readline()
        return any(line.strip() for line in f)


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
    """(base row, head row) per listing, the count of listings on one side only, and the head's rows among those -- by
    key, then by venue + raw title for keys the change re-spelled -- only where
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
    unpaired, heads_alone = 0, []
    for title, bs in base_by_title.items():
        hs = head_by_title.pop(title, [])
        if len(bs) == 1 and len(hs) == 1:
            pairs.append((bs[0], hs[0]))
        else:
            unpaired += len(bs) + len(hs)
            heads_alone += hs
    for hs in head_by_title.values():
        unpaired += len(hs)
        heads_alone += hs
    return pairs, unpaired, heads_alone


def kind_of(before, after):
    if before == after:
        return None
    if after is None:
        return "lost"
    if before is None:
        return "gained"
    return "changed"


def accepted_move(move, accepted):
    """Whether an accepted loss accepts the listing's move from `before` to `after`."""
    venue, title, before, after = move["venue"], move["rawTitle"], ref(move["before"]), ref(move["after"])
    return any(a_venue in ("*", venue) and normalise(a_title) == normalise(title) and a_before in ("*", before)
               and a_after in ("*", after) for a_venue, a_title, a_before, a_after in accepted)


def verdict(cc, move, labels, accepted):
    """(accepted?, why) for one move."""
    venue, title, before, after = move["venue"], move["rawTitle"], ref(move["before"]), ref(move["after"])
    if accepted_move(move, accepted):
        return True, "judgements: an accepted loss"
    now = label_of(labels, venue, title, after)
    was = label_of(labels, venue, title, before)
    if now == "wrong":
        return False, f"judgements: {after} is WRONG"
    if move["kind"] == "gained":
        if now == "right":
            return True, "gained (judged right)"
        if move.get("suspect"):
            return False, "needs judgement, suspect: " + "; ".join(move["suspect"])
        return True, "needs judgement (unjudged gain)"
    if now == "right":
        return True, f"judgements: {after} is right"
    if was == "wrong":
        return True, f"judgements: {before} was wrong"
    return False, "not accepted"


def why(row):
    parts = [row.get("basis") or "?"] + list(row.get("explanation") or [])[:2]
    if row.get("film") is None and row.get("blocker"):
        parts.append(f"blocker {row['blocker']}")
    return ": ".join(str(p) for p in parts)


def film_name(row):
    return ref(take(row)) + (f" {row['filmTitle']}" if row.get("filmTitle") else "")


def moves_of(cc, pairs, labels, accepted):
    moves = []
    for b, h in pairs:
        kind = kind_of(take(b), take(h))
        if kind is None:
            continue
        move = {"kind": kind, "venue": b["venue"], "rawTitle": b["rawTitle"], "before": take(b), "after": take(h),
                "old": film_name(b), "new": film_name(h), "oldWhy": why(b), "newWhy": why(h), "suspect": list(h.get("suspect") or [])}
        move["accepted"], move["verdict"] = verdict(cc, move, labels, accepted)
        move["judge"] = move["verdict"].startswith("needs judgement")
        moves.append(move)
    moves.sort(key=lambda m: (m["accepted"], m["kind"], m["rawTitle"], m["venue"]))
    return moves


def unjudged_suspect(row, labels, baseline):
    """Whether `row` takes a film a suspect-take signal flags that no judgement judges nor tolerates."""
    film = take(row)
    if film is None or not row.get("suspect"):
        return False
    return label_of(labels, row["venue"], row["rawTitle"], ref(film)) is None and \
        (normalise(row["rawTitle"]), ref(film)) not in baseline


def suspects_of(pairs, heads_alone, labels, baseline, accepted):
    """{"new", "standing", "accepted", "cured"}: the unjudged suspect takes, each classified against the base's row for its listing
    (the module doc). A head row on one side only (its key re-spelled ambiguously) has no base to stand on: new."""
    out = {"new": [], "standing": [], "accepted": [], "cured": []}
    def entry(b, h, flagged):
        """The listing, its head film, its base film and the reasons flagging `flagged`'s take."""
        return {"venue": h["venue"], "rawTitle": h["rawTitle"], "film": film_name(h), "base": film_name(b) if b else "-",
                "reasons": list(flagged.get("suspect") or [])}
    for b, h in pairs:
        if unjudged_suspect(h, labels, baseline):
            stood = take(b) == take(h) and bool(b.get("suspect"))
            move = {"venue": h["venue"], "rawTitle": h["rawTitle"], "before": take(b), "after": take(h)}
            out["standing" if stood else "accepted" if accepted_move(move, accepted) else "new"].append(entry(b, h, h))
        elif unjudged_suspect(b, labels, baseline):
            out["cured"].append(entry(b, h, b))
    out["new"] += [entry(None, h, h) for h in heads_alone if unjudged_suspect(h, labels, baseline)]
    for kind in out:
        out[kind].sort(key=lambda e: (e["rawTitle"], e["venue"]))
    return out


def cell(text):
    return str(text).replace("|", "\\|").replace("\n", " ")


def mark(move, failed, judge, ok):
    """The move's status: failed, accepted but needing judgement (an unjudged gain), or accepted."""
    return failed if not move["accepted"] else judge if move["judge"] else ok


def render_suspects(cc, suspects):
    """(log lines, summary lines) of the suspect classification: every new one, then the standing ones as a report."""
    new, standing, cured = suspects["new"], suspects["standing"], suspects["cured"]
    line = (f"suspect takes ({cc}): {len(new)} new (the code's: fail until judged), {len(standing)} standing at base and "
            f"head (reported, never failed), {len(cured)} cured" +
            (f", {len(suspects['accepted'])} moved onto by an accepted loss" if suspects["accepted"] else ""))
    def text(e, status):
        return (f"  {status} suspect {e['venue']} | {e['rawTitle']}: {e['film']} (base {e['base']})\n"
                f"        {'; '.join(e['reasons'])}")
    log = [line] + [text(e, "FAIL") for e in new] + [text(e, "cured") for e in cured]
    md = ["", f"#### Suspect takes ({cc})", "", line, ""]
    if new:
        md += ["| | venue | listing | head film | base film | reasons |", "|---|---|---|---|---|---|"]
        md += [f"| ❌ | {cell(e['venue'])} | {cell(e['rawTitle'])} | {cell(e['film'])} | {cell(e['base'])} | "
               f"{cell('; '.join(e['reasons']))} |" for e in new[:300]]
        md += ["", "A new suspect is the code's: judge its film in `test/resources/fixtures/identity-judgements.tsv` "
                   "(`scripts/label-lookup`, `scripts/identity-slice <cc> --venue V --title T`), or accept the loss there "
                   "(state `accepted-loss`, with its reason)."]
    if standing:
        md += ["", f"<details><summary>{len(standing)} standing suspect(s): the recording's, untouched by this change -- "
                   "triaged from the Identity findings report, never failed here</summary>", ""]
        md += [f"- {cell(e['venue'])} \\| {cell(e['rawTitle'])}: {cell(e['film'])} -- {cell('; '.join(e['reasons']))}"
               for e in standing[:200]]
        md += ["", "</details>"]
    return log, md


def render(cc, moves, unpaired, base_name, head_name, listings, what="identity takes", where="one recording", suspects=None):
    failing = [m for m in moves if not m["accepted"]]
    counts = {k: sum(1 for m in moves if m["kind"] == k) for k in ("lost", "changed", "gained")}
    judge = sum(1 for m in moves if m["judge"])
    head_line = (f"{what} ({cc}): {listings} listings, base {base_name} vs head {head_name} on {where} -- "
                 f"lost {counts['lost']}, changed {counts['changed']}, gained {counts['gained']}; "
                 f"{len(failing)} not accepted, {judge} need judgement" + (f"; {unpaired} listing(s) on one side only" if unpaired else "")
                 + (f"; {len(suspects['new'])} new suspect(s), {len(suspects['standing'])} standing" if suspects else ""))
    text = [head_line] + [
        f"  {mark(m, 'FAIL', 'WARN', 'ok  ')} {m['kind']:7} {m['venue']} | {m['rawTitle']}: {m['old']} -> {m['new']} "
        f"[{m['verdict']}]\n        base: {m['oldWhy']}\n        head: {m['newWhy']}" for m in moves]
    md = [f"### {what[0].upper() + what[1:]} ({cc})", "", head_line, ""]
    if moves:
        md += ["| | kind | venue | listing | base film | head film | base reason | head reason | verdict |",
               "|---|---|---|---|---|---|---|---|---|"]
        md += [f"| {mark(m, '❌', '⚠', '✓')} | {m['kind']} | {cell(m['venue'])} | {cell(m['rawTitle'])} | "
               f"{cell(m['old'])} | {cell(m['new'])} | {cell(m['oldWhy'])} | {cell(m['newWhy'])} | {cell(m['verdict'])} |"
               for m in moves[:300]]
        if len(moves) > 300:
            md.append(f"\n... and {len(moves) - 300} more (the job log has them all)")
    if judge:
        md += ["", "To judge a gain, judge its film `right` or `wrong` in `test/resources/fixtures/identity-judgements.tsv` "
                   "(`scripts/label-lookup`, `scripts/identity-judge-cards` for the evidence)."]
    if failing:
        md += ["", "To accept a move, judge the head's film `right` (or the base's `wrong`) in "
                   "`test/resources/fixtures/identity-judgements.tsv`; to accept a loss as a trade, set the base film's row to "
                   "state `accepted-loss` with its reason."]
    if suspects:
        log, suspect_md = render_suspects(cc, suspects)
        text += log
        md += suspect_md
    return "\n".join(text), "\n".join(md) + "\n", failing


def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("cc")
    p.add_argument("base")
    p.add_argument("head")
    p.add_argument("--judgements", help="test/resources/fixtures/identity-judgements.tsv")
    p.add_argument("--summary")
    p.add_argument("--base-name", default="base")
    p.add_argument("--head-name", default="head")
    p.add_argument("--final", action="store_true", help="the inputs are ratchet-takes.tsv files: the final takes")
    a = p.parse_args(argv)
    # Exit 2 for anything that is not a verdict -- an unreadable file, a row without its fields -- so it can never read as
    # exit 1's "the code moved a take".
    try:
        base, head = final_sides(a.base, a.head, a.cc) if a.final else (load(a.base), load(a.head))
        # The final takes list only listings some side took a film for: a country whose captured clusters take none on
        # either side (Spain's on 10-14) has no move to judge -- a verdict, provided both ratchets wrote takes at all.
        if a.final and not base and not head and ratchet_ran(a.base) and ratchet_ran(a.head):
            print(f"identity final takes ({a.cc}): the capture holds no taken listing on either side -- no move")
            return 0
        if not base or not head:
            print(f"identity takes ({a.cc}): an empty side (base {len(base)}, head {len(head)} listings) -- nothing resolved",
                  file=sys.stderr)
            return 2
        labels, accepted, baseline = judgements_of(a.judgements, a.cc)
        pairs, unpaired, heads_alone = pair(base, head)
        moves = moves_of(a.cc, pairs, labels, accepted)
        # the final takes carry no suspect signals: the model's decisions are where a suspect is judged
        suspects = None if a.final else suspects_of(pairs, heads_alone, labels, baseline, accepted)
    except (OSError, ValueError, KeyError, TypeError, IndexError) as e:
        print(f"identity takes ({a.cc}): could not read the inputs: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    text, md, failing = render(a.cc, moves, unpaired, a.base_name, a.head_name, len(head),
                               *(("identity final takes", "one capture") if a.final else ()), suspects=suspects)
    print(text)
    for m in moves:
        if m["judge"] and m["accepted"]:
            print(f"::warning::identity takes ({a.cc}): gained {m['venue']} | {m['rawTitle']} -> {m['new']} needs judgement")
    if a.summary:
        with open(a.summary, "a", encoding="utf-8") as f:
            f.write(md)
    return 1 if failing or (suspects and suspects["new"]) else 0


if __name__ == "__main__":
    sys.exit(main())
